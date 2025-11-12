#include "code_generator.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cctype>

using namespace std;

CodeGenerator::CodeGenerator(const vector<TACInstruction>& instructions, SymbolTable* symTab,
                             const map<string, string>& strLiterals)
    : tacInstructions(instructions), symbolTable(symTab), 
      frameSize(0), currentOffset(0), stringCounter(0), floatCounter(0),
      functionParamCount(0), currentFunctionFrameSize(0), inFunction(false), labelCounter(0) {
    registers.init();
    floatRegisters.init();  // ADDED: Initialize float register pool
    
    // FIXED: Import string literals from IR generator
    // These are labels like "str1" -> "\"Hello\""
    for (const auto& [label, content] : strLiterals) {
        dataSection.push_back(label + ": .asciiz " + content);
    }
}

//============================================================================
// Register Allocation
//============================================================================

void CodeGenerator::RegisterPool::init() {
    // MIPS register convention:
    // $t0-$t9: temporary registers (caller-saved)
    // $s0-$s7: saved registers (callee-saved)
    // $a0-$a3: argument registers
    // $v0-$v1: return value registers
    // $ra: return address
    // $sp: stack pointer
    // $fp: frame pointer
    
    tempRegs = {"$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8", "$t9"};
    savedRegs = {"$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"};
    varToReg.clear();
    regToVar.clear();
}

string CodeGenerator::RegisterPool::allocateTemp(const string& var) {
    if (varToReg.find(var) != varToReg.end()) {
        return varToReg[var];
    }
    
    if (!tempRegs.empty()) {
        string reg = *tempRegs.begin();
        tempRegs.erase(tempRegs.begin());
        varToReg[var] = reg;
        regToVar[reg] = var;
        return reg;
    }
    
    return "$t9";  // Fallback
}

string CodeGenerator::RegisterPool::allocateSaved(const string& var) {
    if (varToReg.find(var) != varToReg.end()) {
        return varToReg[var];
    }
    
    if (!savedRegs.empty()) {
        string reg = *savedRegs.begin();
        savedRegs.erase(savedRegs.begin());
        varToReg[var] = reg;
        regToVar[reg] = var;
        return reg;
    }
    
    return allocateTemp(var);
}

void CodeGenerator::RegisterPool::freeReg(const string& reg) {
    if (regToVar.find(reg) != regToVar.end()) {
        string var = regToVar[reg];
        varToReg.erase(var);
        regToVar.erase(reg);
        
        if (reg[1] == 't') {
            tempRegs.insert(reg);
        } else if (reg[1] == 's') {
            savedRegs.insert(reg);
        }
    }
}

void CodeGenerator::RegisterPool::spillAll() {
    varToReg.clear();
    regToVar.clear();
    init();
}

bool CodeGenerator::RegisterPool::isAllocated(const string& var) {
    return varToReg.find(var) != varToReg.end();
}

string CodeGenerator::RegisterPool::getReg(const string& var) {
    if (varToReg.find(var) != varToReg.end()) {
        return varToReg[var];
    }
    return "";
}

//============================================================================
// Float Register Pool Implementation
//============================================================================

void CodeGenerator::FloatRegisterPool::init() {
    // Even-numbered float registers for single precision
    // $f0-$f18 for temporaries (even only)
    for (int i = 0; i <= 18; i += 2) {
        tempRegs.insert("$f" + to_string(i));
    }
    
    // $f20-$f30 for saved values (even only)
    for (int i = 20; i <= 30; i += 2) {
        savedRegs.insert("$f" + to_string(i));
    }
    
    varToReg.clear();
    regToVar.clear();
}

string CodeGenerator::FloatRegisterPool::allocateTemp(const string& var) {
    if (varToReg.find(var) != varToReg.end()) {
        return varToReg[var];
    }
    
    if (!tempRegs.empty()) {
        string reg = *tempRegs.begin();
        tempRegs.erase(tempRegs.begin());
        varToReg[var] = reg;
        regToVar[reg] = var;
        return reg;
    }
    
    return "$f18";  // Fallback
}

string CodeGenerator::FloatRegisterPool::allocateSaved(const string& var) {
    if (varToReg.find(var) != varToReg.end()) {
        return varToReg[var];
    }
    
    if (!savedRegs.empty()) {
        string reg = *savedRegs.begin();
        savedRegs.erase(savedRegs.begin());
        varToReg[var] = reg;
        regToVar[reg] = var;
        return reg;
    }
    
    return allocateTemp(var);
}

void CodeGenerator::FloatRegisterPool::freeReg(const string& reg) {
    if (regToVar.find(reg) != regToVar.end()) {
        string var = regToVar[reg];
        varToReg.erase(var);
        regToVar.erase(reg);
        
        // Parse register number
        int regNum = stoi(reg.substr(2));
        if (regNum <= 18) {
            tempRegs.insert(reg);
        } else {
            savedRegs.insert(reg);
        }
    }
}

void CodeGenerator::FloatRegisterPool::spillAll() {
    varToReg.clear();
    regToVar.clear();
    init();
}

bool CodeGenerator::FloatRegisterPool::isAllocated(const string& var) {
    return varToReg.find(var) != varToReg.end();
}

string CodeGenerator::FloatRegisterPool::getReg(const string& var) {
    if (varToReg.find(var) != varToReg.end()) {
        return varToReg[var];
    }
    return "";
}

//============================================================================
// Helper Utilities
//============================================================================

void CodeGenerator::emit(const string& instruction) {
    asmCode.push_back("    " + instruction);
}

void CodeGenerator::emitLabel(const string& label) {
    asmCode.push_back(label + ":");
}

void CodeGenerator::emitComment(const string& comment) {
    asmCode.push_back("    # " + comment);
}

void CodeGenerator::emitBlankLine() {
    asmCode.push_back("");
}

string CodeGenerator::newLabel(const string& prefix) {
    return prefix + "_" + to_string(labelCounter++);
}

string CodeGenerator::getStringLabel(const string& content) {
    if (stringLabels.find(content) != stringLabels.end()) {
        return stringLabels[content];
    }
    
    string label = "_str" + to_string(stringCounter++);
    stringLabels[content] = label;
    dataSection.push_back(label + ": .asciiz " + content);
    return label;
}

bool CodeGenerator::isImmediate(const string& operand) {
    if (operand.empty()) return false;
    if (operand[0] == '-' || isdigit(operand[0])) {
        for (size_t i = 1; i < operand.size(); i++) {
            if (!isdigit(operand[i])) return false;
        }
        return true;
    }
    return false;
}

bool CodeGenerator::isFloatLiteral(const string& operand) {
    if (operand.empty()) return false;
    // Check if it contains a decimal point
    return operand.find('.') != string::npos;
}

string CodeGenerator::getOrCreateFloatLabel(const string& value) {
    if (floatLabels.find(value) != floatLabels.end()) {
        return floatLabels[value];
    }
    string label = "_fconst" + to_string(floatCounter++);
    floatLabels[value] = label;
    dataSection.push_back(label + ": .float " + value);
    return label;
}

bool CodeGenerator::isTemp(const string& operand) {
    return operand.size() > 0 && operand[0] == 't' && isdigit(operand[1]);
}

bool CodeGenerator::isVariable(const string& operand) {
    return !isImmediate(operand) && !isTemp(operand) && !operand.empty();
}

int CodeGenerator::getImmediate(const string& operand) {
    return stoi(operand);
}

// FIXED: Helper function to get element size and proper load/store instructions based on type
// Returns: {size_in_bytes, load_instruction, store_instruction}
// Example: "int" -> {4, "lw", "sw"}, "char" -> {1, "lb", "sb"}
CodeGenerator::TypeInfo CodeGenerator::getTypeInfo(const string& varName) {
    // Strip SSA suffix (e.g., arr#2 -> arr)
    string baseName = varName;
    size_t hashPos = varName.find('#');
    if (hashPos != string::npos) {
        baseName = varName.substr(0, hashPos);
    }
    
    // Look up in varTypes map first (for temporaries)
    if (varTypes.find(varName) != varTypes.end() || varTypes.find(baseName) != varTypes.end()) {
        string type = varTypes.find(varName) != varTypes.end() ? varTypes[varName] : varTypes[baseName];
        if (type.find("char") != string::npos && type.find('*') == string::npos) {
            return {1, "lb", "sb"};  // char (not char*)
        }
        if (type.find("short") != string::npos && type.find('*') == string::npos) {
            return {2, "lh", "sh"};  // short (not short*)
        }
        // int, float, pointers are all 4 bytes
        return {4, "lw", "sw"};
    }
    
    // Look up in symbol table
    Symbol* sym = symbolTable->lookuph(baseName);
    if (sym) {
        string baseType = sym->type;
        // If it's a pointer, size is always 4 bytes (pointer size on MIPS32)
        if (sym->pointerDepth > 0) {
            return {4, "lw", "sw"};
        }
        // If it's an array, get element type
        if (sym->isArray) {
            // Array elements - check base type
            if (baseType == "char") {
                return {1, "lb", "sb"};
            }
            if (baseType == "short") {
                return {2, "lh", "sh"};
            }
            // int, float arrays
            return {4, "lw", "sw"};
        }
        // Scalar type
        if (baseType == "char") {
            return {1, "lb", "sb"};
        }
        if (baseType == "short") {
            return {2, "lh", "sh"};
        }
    }
    
    // Default: assume 4-byte integer/float/pointer
    return {4, "lw", "sw"};
}

int CodeGenerator::getVarOffset(const string& varName) {
    // ✅ CRITICAL FIX: Strip SSA suffix FIRST before any lookups
    // This ensures matrix2d#1 and matrix2d refer to the SAME stack location
    string baseName = varName;
    size_t hashPos = varName.find('#');
    if (hashPos != string::npos) {
        baseName = varName.substr(0, hashPos);
    }
    
    // Check if already allocated (using base name)
    if (varOffsets.find(baseName) != varOffsets.end()) {
        return varOffsets[baseName];
    }
    
    // FIXED: Check if this variable was already identified as a global during pre-scan
    // If it's in staticVars, it's a global and shouldn't be on the stack
    if (staticVars.find(baseName) != staticVars.end()) {
        return -1;  // Not on stack - it's in .data section
    }
    
    // This is a local variable - allocate stack space for it
    Symbol* sym = symbolTable->lookuph(baseName);
    
    // Allocate new offset for local variable - check if it's an array
    int varSize = 4;  // Default size for scalar variables
    
    if (sym && sym->isArray && !sym->arrayDimensions.empty()) {
        // Calculate total array size
        varSize = 4;  // Base element size (assuming int/float = 4 bytes)
        for (int dim : sym->arrayDimensions) {
            if (dim > 0) {
                varSize *= dim;
            }
        }
    }
    currentOffset -= varSize;
    // ✅ Store using BASE NAME so all SSA versions map to same location
    varOffsets[baseName] = currentOffset;
    return currentOffset;
}

int CodeGenerator::calculateFrameSize(size_t funcStartIdx, size_t funcEndIdx) {
    // Pre-scan the function to find all variables that will need stack space
    int minOffset = 0;  // Track the most negative offset
    map<string, int> tempOffsets;
    int tempOffset = 0;
    
    // Safety check
    if (funcEndIdx > tacInstructions.size()) {
        funcEndIdx = tacInstructions.size();
    }
    
    for (size_t i = funcStartIdx; i < funcEndIdx; i++) {
        const TACInstruction& instr = tacInstructions[i];
        
        // Skip labels and control flow instructions for operands
        bool isControlFlow = (instr.opcode == TACOp::LABEL || instr.opcode == TACOp::GOTO ||
                             instr.opcode == TACOp::IF_GOTO || instr.opcode == TACOp::IF_FALSE_GOTO ||
                             instr.opcode == TACOp::FUNC_BEGIN || instr.opcode == TACOp::FUNC_END);
        
        // Check result operand (variables and temps that will be stored)
        if (!instr.result.empty() && !isImmediate(instr.result) && !isControlFlow) {
            if (tempOffsets.find(instr.result) == tempOffsets.end()) {
                // Check if this is an array variable and allocate proper space
                int varSize = 4;  // Default size
                // Strip SSA suffix to lookup in symbol table
                string baseName = instr.result;
                size_t hashPos = instr.result.find('#');
                if (hashPos != string::npos) {
                    baseName = instr.result.substr(0, hashPos);
                }
                Symbol* sym = symbolTable->lookuph(baseName);
                if (sym && sym->isArray && !sym->arrayDimensions.empty()) {
                    // Calculate total array size
                    varSize = 4;  // Base element size (assuming int/float = 4 bytes)
                    for (int dim : sym->arrayDimensions) {
                        if (dim > 0) {
                            varSize *= dim;
                        }
                    }
                }
                tempOffset -= varSize;
                tempOffsets[instr.result] = tempOffset;
                if (tempOffset < minOffset) {
                    minOffset = tempOffset;
                }
            }
        }
        
        // Check operand1 (if it's a variable/temp that will be loaded)
        if (!instr.operand1.empty() && !isImmediate(instr.operand1) && !isControlFlow) {
            if (tempOffsets.find(instr.operand1) == tempOffsets.end()) {
                // Check if this is an array variable and allocate proper space
                int varSize = 4;  // Default size
                // Strip SSA suffix to lookup in symbol table
                string baseName = instr.operand1;
                size_t hashPos = instr.operand1.find('#');
                if (hashPos != string::npos) {
                    baseName = instr.operand1.substr(0, hashPos);
                }
                Symbol* sym = symbolTable->lookuph(baseName);
                if (sym && sym->isArray && !sym->arrayDimensions.empty()) {
                    // Calculate total array size
                    varSize = 4;  // Base element size
                    for (int dim : sym->arrayDimensions) {
                        if (dim > 0) {
                            varSize *= dim;
                        }
                    }
                }
                tempOffset -= varSize;
                tempOffsets[instr.operand1] = tempOffset;
                if (tempOffset < minOffset) {
                    minOffset = tempOffset;
                }
            }
        }
        
        // Check operand2
        if (!instr.operand2.empty() && !isImmediate(instr.operand2)) {
            if (tempOffsets.find(instr.operand2) == tempOffsets.end()) {
                // Check if this is an array variable and allocate proper space
                int varSize = 4;  // Default size
                // Strip SSA suffix to lookup in symbol table
                string baseName = instr.operand2;
                size_t hashPos = instr.operand2.find('#');
                if (hashPos != string::npos) {
                    baseName = instr.operand2.substr(0, hashPos);
                }
                Symbol* sym = symbolTable->lookuph(baseName);
                if (sym && sym->isArray && !sym->arrayDimensions.empty()) {
                    // Calculate total array size
                    varSize = 4;  // Base element size
                    for (int dim : sym->arrayDimensions) {
                        if (dim > 0) {
                            varSize *= dim;
                        }
                    }
                }
                tempOffset -= varSize;
                tempOffsets[instr.operand2] = tempOffset;
                if (tempOffset < minOffset) {
                    minOffset = tempOffset;
                }
            }
        }
    }
    
    // Calculate frame size: space for locals + 8 bytes for $ra/$fp + parameter space
    int localsSpace = -minOffset;  // Convert negative offset to positive size
    int paramSpace = (functionParamCount > 4 ? 4 : functionParamCount) * 4;
    
    // ✅ FIX: Add safety margin for temporaries created during code generation
    // printf and complex expressions create many temporaries not visible in TAC
    // Formula: 3x the calculated space to be safe (triples available stack)
    int safetyMultiplier = 3;
    
    int totalSpace = (localsSpace + 8 + paramSpace) * safetyMultiplier;
    
    // Ensure minimum frame size and round up to 8-byte alignment for MIPS
    if (totalSpace < 128) totalSpace = 128;  // Higher minimum for functions with loops/printf
    if (totalSpace % 8 != 0) {
        totalSpace = ((totalSpace / 8) + 1) * 8;
    }
    
    return totalSpace;
}

//============================================================================
// Float Type Tracking Helpers
//============================================================================

bool CodeGenerator::isFloatType(const string& varName) {
    string type = getVarType(varName);
    return (type == "float" || type == "double");
}

string CodeGenerator::getVarType(const string& varName) {
    // Strip SSA suffix
    string baseName = varName;
    size_t hashPos = varName.find('#');
    if (hashPos != string::npos) {
        baseName = varName.substr(0, hashPos);
    }
    
    // Check varTypes map first (for temporaries and explicit tracking)
    if (varTypes.find(varName) != varTypes.end()) {
        return varTypes[varName];
    }
    if (varTypes.find(baseName) != varTypes.end()) {
        return varTypes[baseName];
    }
    
    // Look up in symbol table
    Symbol* sym = symbolTable->lookuph(baseName);
    if (sym) {
        return sym->type;
    }
    
    // Default to int
    return "int";
}

void CodeGenerator::setVarType(const string& varName, const string& type) {
    varTypes[varName] = type;
}

void CodeGenerator::loadFloatOperand(const string& operand, const string& targetReg) {
    // targetReg should be a float register like $f0, $f2, etc.
    if (isFloatLiteral(operand)) {
        // Load float literal from .data section
        string label = getOrCreateFloatLabel(operand);
        emit("la $t9, " + label);
        emit("lwc1 " + targetReg + ", 0($t9)");  // Load word coprocessor 1 (FPU)
    }
    else if (isImmediate(operand)) {
        // Integer immediate - need to convert to float
        emit("li $t9, " + operand);
        emit("mtc1 $t9, " + targetReg);  // Move to coprocessor 1
        emit("cvt.s.w " + targetReg + ", " + targetReg);  // Convert int to float
    }
    else if (isTemp(operand) || isVariable(operand)) {
        // Load from stack
        int offset = getVarOffset(operand);
        if (offset == -1) {
            // Global variable
            string baseName = operand;
            size_t hashPos = operand.find('#');
            if (hashPos != string::npos) {
                baseName = operand.substr(0, hashPos);
            }
            if (staticVars.find(baseName) != staticVars.end()) {
                emit("la $t9, " + staticVars[baseName]);
                emit("lwc1 " + targetReg + ", 0($t9)");
            }
        } else {
            emit("lwc1 " + targetReg + ", " + to_string(offset) + "($fp)");
        }
    }
}

void CodeGenerator::storeFloatToVar(const string& var, const string& sourceReg) {
    // sourceReg should be a float register like $f0, $f2, etc.
    int offset = getVarOffset(var);
    if (offset == -1) {
        // Global variable
        string baseName = var;
        size_t hashPos = var.find('#');
        if (hashPos != string::npos) {
            baseName = var.substr(0, hashPos);
        }
        if (staticVars.find(baseName) != staticVars.end()) {
            emit("la $t9, " + staticVars[baseName]);
            emit("swc1 " + sourceReg + ", 0($t9)");
        }
    } else {
        emit("swc1 " + sourceReg + ", " + to_string(offset) + "($fp)");
    }
}

//============================================================================
// Operand Loading/Storing
//============================================================================

void CodeGenerator::loadOperand(const string& operand, const string& targetReg) {
    if (isFloatLiteral(operand)) {
        // Float literal - create a label in .data and load address
        // Note: For proper float handling, this should use l.s into FPU register
        // For now, we store the bits as integer (TEMPORARY WORKAROUND)
        emitComment("WARNING: Float literal " + operand + " treated as integer bits");
        string label = getOrCreateFloatLabel(operand);
        emit("la $t9, " + label);
        emit("lw " + targetReg + ", 0($t9)");
    }
    else if (isImmediate(operand)) {
        emit("li " + targetReg + ", " + operand);
    }
    // FIXED: Check if operand is a string literal label (from IR: "str1", "str2", etc.)
    // These look like identifiers but are actually labels that need 'la' not 'li'
    else if (operand.size() >= 3 && operand.substr(0, 3) == "str" && isdigit(operand[3])) {
        // String literal label from IR (e.g., str1, str2)
        emit("la " + targetReg + ", " + operand);
    }
    else if (registers.isAllocated(operand)) {
        string srcReg = registers.getReg(operand);
        if (srcReg != targetReg) {
            emit("move " + targetReg + ", " + srcReg);
        }
    }
    else if (staticVars.find(operand) != staticVars.end()) {
        // Load from global/static variable in data section
        string label = staticVars[operand];
        emit("lw " + targetReg + ", " + label);
    }
    else {
        // Load from stack
        int offset = getVarOffset(operand);
        emit("lw " + targetReg + ", " + to_string(offset) + "($fp)");
    }
}

void CodeGenerator::storeToVar(const string& var, const string& sourceReg) {
    if (registers.isAllocated(var)) {
        string destReg = registers.getReg(var);
        if (destReg != sourceReg) {
            emit("move " + destReg + ", " + sourceReg);
        }
    }
    else if (staticVars.find(var) != staticVars.end()) {
        // Store to global/static variable in data section
        string label = staticVars[var];
        emit("sw " + sourceReg + ", " + label);
    }
    else {
        // Store to stack
        int offset = getVarOffset(var);
        emit("sw " + sourceReg + ", " + to_string(offset) + "($fp)");
    }
}

//============================================================================
// Main Generation
//============================================================================

void CodeGenerator::generate() {
    asmCode.clear();
    // FIXED: Don't clear dataSection - it contains string literals from constructor
    // dataSection.clear();
    dynamicDataSection.clear();
    
    // Don't indent initial comments - Venus compatibility
    asmCode.push_back("# Generated MIPS Assembly");
    asmCode.push_back("# Compiler: Custom C Compiler");
    asmCode.push_back("");
    
    generateDataSection();
    emitBlankLine();
    generateTextSection();
    
    // Insert dynamic data section (printf strings) before .text
    if (!dynamicDataSection.empty()) {
        // Find the .text line
        auto it = asmCode.begin();
        for (; it != asmCode.end(); ++it) {
            if (it->find(".text") != string::npos) {
                break;
            }
        }
        
        // Insert dynamic data before .text with comment
        if (it != asmCode.end()) {
            it = asmCode.insert(it, "");
            ++it;
            it = asmCode.insert(it, "    # Dynamic string literals (from printf)");
            ++it;
            for (const auto& line : dynamicDataSection) {
                it = asmCode.insert(it, "    " + line);
                ++it;  // Move past inserted element
            }
        }
    }
}

void CodeGenerator::generateDataSection() {
    asmCode.push_back(".data");
    
    // Predefined strings
    dataSection.push_back("_newline: .asciiz \"\\n\"");
    dataSection.push_back("_int_fmt: .asciiz \"%d\"");
    dataSection.push_back("_str_fmt: .asciiz \"%s\"");
    dataSection.push_back("_char_fmt: .asciiz \"%c\"");
    
    // FIRST PASS: Identify truly global variables
    // Strategy 1: Variables accessed via ADDRESS (&var) - arrays and address-taken vars
    // Strategy 2: Scalar globals used directly - check symbol table for !isLocal
    std::map<std::string, int> globalInits;  // variable -> initial value
    std::set<std::string> parameterNames;    // Track parameters to exclude them
    
    // Collect parameter names from FUNC_BEGIN blocks
    for (size_t i = 0; i < tacInstructions.size(); i++) {
        if (tacInstructions[i].opcode == TACOp::FUNC_BEGIN) {
            // Next instructions will be GET_PARAM operations
            for (size_t j = i + 1; j < tacInstructions.size(); j++) {
                const auto& instr = tacInstructions[j];
                // Check if this is a GET_PARAM operation
                if (instr.opcode == TACOp::GET_PARAM) {
                    // This is a parameter: n#1 = param[0]
                    string varName = instr.result;
                    string baseName = varName;
                    size_t hashPos = baseName.find('#');
                    if (hashPos != string::npos) {
                        baseName = baseName.substr(0, hashPos);
                    }
                    parameterNames.insert(baseName);
                } else {
                    // Hit a non-parameter instruction, stop scanning this function
                    break;
                }
            }
        }
    }
    
    // PASS 1: Find globals via ADDRESS operations (arrays, address-taken vars)
    for (const auto& instr : tacInstructions) {
        if (instr.opcode == TACOp::ADDRESS && !instr.operand1.empty()) {
            string varName = instr.operand1;
            
            // Skip temps and literals
            if (varName.empty() || varName[0] == 't' || varName[0] == '"' || 
                isdigit(varName[0]) || varName.find('.') != string::npos) {
                continue;
            }
            
            string baseName = varName;
            size_t hashPos = baseName.find('#');
            if (hashPos != string::npos) {
                baseName = baseName.substr(0, hashPos);
            }
            
            // Skip parameters
            if (parameterNames.find(baseName) != parameterNames.end()) {
                continue;
            }
            
            // Check symbol table: must be not local, not static, and NOT a function
            Symbol* sym = symbolTable->lookuph(baseName);
            if (sym && !sym->isLocal && !sym->isStatic && !sym->isFunction) {
                string sanitizedName = varName;
                std::replace(sanitizedName.begin(), sanitizedName.end(), '#', '_');
                staticVars[varName] = sanitizedName;
                if (globalInits.find(varName) == globalInits.end()) {
                    globalInits[varName] = 0;
                }
            }
        }
    }
    
    // PASS 2: Find scalar globals used directly (not via ADDRESS)
    // These are globals that are read/written directly like: hanoi_moves = hanoi_moves + 1
    for (const auto& instr : tacInstructions) {
        auto checkScalarGlobal = [&](const string& varName) {
            if (varName.empty() || varName[0] == 't' || varName[0] == '"' || 
                isdigit(varName[0]) || varName.find('.') != string::npos) {
                return;
            }
            
            // Already detected in PASS 1?
            if (staticVars.find(varName) != staticVars.end()) {
                return;
            }
            
            string baseName = varName;
            size_t hashPos = baseName.find('#');
            if (hashPos != string::npos) {
                baseName = baseName.substr(0, hashPos);
            }
            
            // Skip parameters
            if (parameterNames.find(baseName) != parameterNames.end()) {
                return;
            }
            
            // Check symbol table: must be global (not local, not static, not function)
            Symbol* sym = symbolTable->lookuph(baseName);
            if (sym && !sym->isLocal && !sym->isStatic && !sym->isFunction) {
                string sanitizedName = varName;
                std::replace(sanitizedName.begin(), sanitizedName.end(), '#', '_');
                staticVars[varName] = sanitizedName;
                if (globalInits.find(varName) == globalInits.end()) {
                    globalInits[varName] = 0;
                }
            }
        };
        
        // Check operands and results for potential scalar globals
        checkScalarGlobal(instr.operand1);
        checkScalarGlobal(instr.operand2);
        checkScalarGlobal(instr.result);
    }
    
    // Collect string and float literals from TAC
    // Also collect global variable initializations (before any function)
    // And function-local static variables (those with __inited guard)
    bool inGlobalScope = true;
    std::string pendingConstValue = "";
    std::string lastLabel = "";  // Track labels for detecting static init patterns
    
    for (const auto& instr : tacInstructions) {
        // Check if we've entered a function
        if (instr.opcode == TACOp::FUNC_BEGIN) {
            inGlobalScope = false;
        }
        
        // Track labels to detect static initialization guards
        if (instr.opcode == TACOp::LABEL) {
            lastLabel = instr.result;
        }
        
        // FIXED: Detect ADDRESS operations on potential globals - scan ALL TAC, not just global scope
        // But we need to be careful: only treat as global if it's TRULY a global (not a parameter or local)
        if (instr.opcode == TACOp::ADDRESS && !instr.operand1.empty()) {
            // Extract base name without SSA suffix
            string baseName = instr.operand1;
            size_t hashPos = baseName.find('#');
            if (hashPos != string::npos) {
                baseName = baseName.substr(0, hashPos);
            }
            
            // Check if this is a global variable - was it identified in ADDRESS pre-scan?
            bool isTrueGlobal = (staticVars.find(instr.operand1) != staticVars.end());
            
            if (isTrueGlobal) {
                // This is a global - already in staticVars from ADDRESS scan
                string sanitizedName = instr.operand1;
                std::replace(sanitizedName.begin(), sanitizedName.end(), '#', '_');
                staticVars[instr.operand1] = sanitizedName;
                if (globalInits.find(instr.operand1) == globalInits.end()) {
                    globalInits[instr.operand1] = 0;  // Default init to 0
                }
            }
        }
        
        // FIXED: Detect direct variable access in GLOBAL SCOPE ONLY
        if (inGlobalScope) {
            auto checkOperandForGlobal = [&](const string& operand) {
                if (operand.empty() || operand[0] == '"' || isdigit(operand[0]) || 
                    operand[0] == '-' || operand[0] == 't' || operand.find('.') != string::npos) {
                    return;  // Skip temps, literals, and strings
                }
                
                // Extract base name
                string baseName = operand;
                size_t hashPos = baseName.find('#');
                if (hashPos != string::npos) {
                    baseName = baseName.substr(0, hashPos);
                }
                
                // Check if global via symbol table
                Symbol* sym = symbolTable->lookuph(baseName);
                if (sym && !sym->isLocal && !sym->isStatic) {
                    string sanitizedName = operand;
                    std::replace(sanitizedName.begin(), sanitizedName.end(), '#', '_');
                    staticVars[operand] = sanitizedName;
                    if (globalInits.find(operand) == globalInits.end()) {
                        globalInits[operand] = 0;
                    }
                }
            };
            
            // Check all operands and result (only in global scope before first function)
            checkOperandForGlobal(instr.operand1);
            checkOperandForGlobal(instr.operand2);
            if (instr.opcode == TACOp::ASSIGN || instr.opcode == TACOp::ADD || 
                instr.opcode == TACOp::SUB || instr.opcode == TACOp::MUL || instr.opcode == TACOp::DIV) {
                checkOperandForGlobal(instr.result);
            }
        }
        
        // Detect function-local static variables by their __inited guard pattern
        if (instr.opcode == TACOp::IF_GOTO && instr.result.find("static_init_end") != std::string::npos) {
            // This is a static init guard: if var__inited goto label
            std::string initedVar = instr.operand1;
            if (initedVar.find("__inited") != std::string::npos) {
                // Extract the base variable name (remove __inited suffix)
                std::string baseVar = initedVar.substr(0, initedVar.find("__inited"));
                // Sanitize names for MIPS (replace # with _)
                std::string baseLabel = baseVar;
                std::string initedLabel = initedVar;
                std::replace(baseLabel.begin(), baseLabel.end(), '#', '_');
                std::replace(initedLabel.begin(), initedLabel.end(), '#', '_');
                
                // Add both the variable and its guard flag to static vars
                staticVars[baseVar] = baseLabel;
                staticVars[initedVar] = initedLabel;
                globalInits[baseVar] = 0;  // Default to 0, will be updated if we find actual init
                globalInits[initedVar] = 0;  // Guard flag starts at 0
            }
        }
        
        if (inGlobalScope) {
            // Track global initializations: t1 = CONST 100, g#1 = t1
            if (instr.opcode == TACOp::CONST && !instr.operand1.empty()) {
                if (instr.operand1[0] == '"') {
                    // String literal
                    getStringLabel(instr.operand1);
                } else if (isFloatLiteral(instr.operand1)) {
                    // Float literal
                    getOrCreateFloatLabel(instr.operand1);
                } else {
                    // Integer constant - might be for global init
                    pendingConstValue = instr.operand1;
                }
            } else if (instr.opcode == TACOp::ASSIGN && !pendingConstValue.empty()) {
                // This is a global initialization: variable = constant
                try {
                    int value = std::stoi(pendingConstValue);
                    globalInits[instr.result] = value;
                    staticVars[instr.result] = instr.result;  // Add to static vars
                } catch (...) {}
                pendingConstValue = "";
            }
        } else {
            // In function scope - still collect string/float literals
            if (instr.opcode == TACOp::CONST && !instr.operand1.empty()) {
                if (instr.operand1[0] == '"') {
                    getStringLabel(instr.operand1);
                } else if (isFloatLiteral(instr.operand1)) {
                    getOrCreateFloatLabel(instr.operand1);
                }
            }
        }
    }
    
    // === ORGANIZED DATA SECTION ===
    // Section 1: String literals (read-only data)
    asmCode.push_back("");
    asmCode.push_back("    # String literals");
    for (const auto& line : dataSection) {
        asmCode.push_back("    " + line);
    }
    
    // Section 2: Initialized global/static variables (scalars with initial values)
    bool hasInitializedGlobals = false;
    for (const auto& entry : staticVars) {
        string baseName = entry.first;
        size_t hashPos = baseName.find('#');
        if (hashPos != string::npos) {
            baseName = baseName.substr(0, hashPos);
        }
        
        Symbol* sym = symbolTable->lookuph(baseName);
        // Only process scalar variables here (not arrays)
        if (!sym || !sym->isArray || sym->arrayDimensions.empty()) {
            if (!hasInitializedGlobals) {
                asmCode.push_back("");
                asmCode.push_back("    # Initialized global variables");
                hasInitializedGlobals = true;
            }
            
            int initValue = 0;
            auto it = globalInits.find(entry.first);
            if (it != globalInits.end()) {
                initValue = it->second;
            }
            
            string label = entry.second;
            for (char& c : label) {
                if (c == '#') c = '_';
            }
            
            asmCode.push_back("    .align 2");
            asmCode.push_back("    " + label + ": .word " + std::to_string(initValue));
            
            // Update the staticVars map with sanitized name
            const_cast<std::map<std::string, std::string>&>(staticVars)[entry.first] = label;
        }
    }
    
    // Section 3: Uninitialized global/static variables (arrays)
    bool hasUninitializedGlobals = false;
    for (const auto& entry : staticVars) {
        string baseName = entry.first;
        size_t hashPos = baseName.find('#');
        if (hashPos != string::npos) {
            baseName = baseName.substr(0, hashPos);
        }
        
        Symbol* sym = symbolTable->lookuph(baseName);
        if (sym && sym->isArray && !sym->arrayDimensions.empty()) {
            if (!hasUninitializedGlobals) {
                asmCode.push_back("");
                asmCode.push_back("    # Uninitialized global variables (arrays)");
                hasUninitializedGlobals = true;
            }
            
            // Calculate total array size in bytes
            int totalSize = 4;  // Base element size (int/float = 4 bytes)
            for (int dim : sym->arrayDimensions) {
                if (dim > 0) {
                    totalSize *= dim;
                }
            }
            
            string label = entry.second;
            for (char& c : label) {
                if (c == '#') c = '_';
            }
            
            asmCode.push_back("    .align 2");
            asmCode.push_back("    " + label + ": .space " + std::to_string(totalSize));
            
            // Update the staticVars map with sanitized name
            const_cast<std::map<std::string, std::string>&>(staticVars)[entry.first] = label;
        }
    }
}

void CodeGenerator::generateTextSection() {
    asmCode.push_back(".text");
    asmCode.push_back(".globl main");
    emitBlankLine();
    
    // PRE-PASS: Calculate frame sizes for all functions
    for (size_t i = 0; i < tacInstructions.size(); i++) {
        if (tacInstructions[i].opcode == TACOp::FUNC_BEGIN) {
            string funcName = tacInstructions[i].result;
            int paramCount = tacInstructions[i].paramCount;
            
            // Find the end of this function
            size_t funcEnd = i + 1;
            for (size_t j = i + 1; j < tacInstructions.size(); j++) {
                if (tacInstructions[j].opcode == TACOp::FUNC_END) {
                    funcEnd = j;
                    break;
                }
            }
            
            // Temporarily set function param count for calculation
            int savedParamCount = functionParamCount;
            functionParamCount = paramCount;
            
            // Calculate frame size for this function
            int fsize = calculateFrameSize(i + 1, funcEnd);
            functionFrameSizes[funcName] = fsize;
            
            functionParamCount = savedParamCount;
            
            emitComment("Function " + funcName + " frame size: " + to_string(fsize) + " bytes");
        }
    }
    
    emitBlankLine();
    
    // MAIN PASS: Generate code (skip global initialization instructions before first function)
    bool inFunction = false;
    for (const auto& instr : tacInstructions) {
        if (instr.opcode == TACOp::FUNC_BEGIN) {
            inFunction = true;
        }
        
        // Skip global-scope instructions (they're handled in data section)
        if (!inFunction && instr.opcode != TACOp::FUNC_BEGIN) {
            continue;
        }
        
        generateInstruction(instr);
    }
    
    // Add exit syscall
    emitBlankLine();
    emitLabel("_program_exit");
    emit("li $v0, 10");
    emit("syscall");
}

//============================================================================
// Instruction Dispatcher
//============================================================================

void CodeGenerator::generateInstruction(const TACInstruction& instr) {
    // Skip comments and nops
    if (instr.opcode == TACOp::COMMENT || instr.opcode == TACOp::NOP) {
        emitComment(instr.toString());
        return;
    }
    
    emitComment(instr.toString());
    
    switch (instr.opcode) {
        // Arithmetic
        case TACOp::ADD: generateAdd(instr); break;
        case TACOp::SUB: generateSub(instr); break;
        case TACOp::MUL: generateMul(instr); break;
        case TACOp::DIV: generateDiv(instr); break;
        case TACOp::MOD: generateMod(instr); break;
        case TACOp::NEG: generateNeg(instr); break;
        
        // Logical (short-circuit)
        case TACOp::AND: generateLogicalAnd(instr); break;
        case TACOp::OR: generateLogicalOr(instr); break;
        case TACOp::NOT: generateLogicalNot(instr); break;
        
        // Bitwise
        case TACOp::BIT_AND: generateBitAnd(instr); break;
        case TACOp::BIT_OR: generateBitOr(instr); break;
        case TACOp::BIT_XOR: generateBitXor(instr); break;
        case TACOp::BIT_NOT: generateBitNot(instr); break;
        case TACOp::SHL: generateShl(instr); break;
        case TACOp::SHR: generateShr(instr); break;
        
        // Comparison
        case TACOp::EQ: generateEq(instr); break;
        case TACOp::NE: generateNe(instr); break;
        case TACOp::LT: generateLt(instr); break;
        case TACOp::LE: generateLe(instr); break;
        case TACOp::GT: generateGt(instr); break;
        case TACOp::GE: generateGe(instr); break;
        
        // Memory
        case TACOp::ASSIGN: generateAssign(instr); break;
        case TACOp::LOAD: generateLoad(instr); break;
        case TACOp::STORE: generateStore(instr); break;
        case TACOp::ADDRESS: generateAddress(instr); break;
        case TACOp::ARRAY_LOAD: generateArrayLoad(instr); break;
        case TACOp::ARRAY_STORE: generateArrayStore(instr); break;
        
        // Control flow
        case TACOp::LABEL: generateLabel(instr); break;
        case TACOp::GOTO: generateGoto(instr); break;
        case TACOp::IF_GOTO: generateIfGoto(instr); break;
        case TACOp::IF_FALSE_GOTO: generateIfFalseGoto(instr); break;
        
        // Functions
        case TACOp::FUNC_BEGIN: generateFuncBegin(instr); break;
        case TACOp::FUNC_END: generateFuncEnd(instr); break;
        case TACOp::PARAM: generateParam(instr); break;
        case TACOp::CALL: generateCall(instr); break;
        case TACOp::RETURN: generateReturn(instr); break;
        
        // Type conversion
        case TACOp::CAST: generateCast(instr); break;
        
        case TACOp::CONST:
            // result = constant
            if (!instr.operand1.empty() && instr.operand1[0] == '"') {
                // String constant - load address
                string label = getStringLabel(instr.operand1);
                emit("la $t0, " + label);
                // Track which variable holds this string literal (strip quotes)
                string strContent = instr.operand1.substr(1, instr.operand1.length() - 2);
                varToStringLiteral[instr.result] = strContent;
                storeToVar(instr.result, "$t0");
            } 
            // FIXED: Check if operand is a string literal label (from IR: "str1", "str2", etc.)
            else if (instr.operand1.size() >= 3 && instr.operand1.substr(0, 3) == "str" && 
                     isdigit(instr.operand1[3])) {
                // String literal label - load address not immediate
                emit("la $t0, " + instr.operand1);
                // FIXED: Track that result is a char* (string literals are char arrays)
                varTypes[instr.result] = "char*";
                storeToVar(instr.result, "$t0");
            }
            else if (isFloatLiteral(instr.operand1)) {
                // Float constant - load into FPU register
                emitComment("Float constant: " + instr.result + " = " + instr.operand1);
                loadFloatOperand(instr.operand1, "$f0");
                storeFloatToVar(instr.result, "$f0");
                // Mark as float type
                setVarType(instr.result, "float");
            } else {
                // Integer constant
                emit("li $t0, " + instr.operand1);
                storeToVar(instr.result, "$t0");
            }
            break;
        
        case TACOp::GET_PARAM:
            // result = param[index]
            // Parameters 0-3: in $a0-$a3
            // Parameters 4+: on stack ABOVE the frame (caller's responsibility)
            {
                int paramIdx = 0;
                if (!instr.operand1.empty()) {
                    paramIdx = stoi(instr.operand1);
                }
                
                if (paramIdx < 4) {
                    // Parameter is in register $a0-$a3
                    string argReg = "$a" + to_string(paramIdx);
                    emit("move $t0, " + argReg);
                } else {
                    // Parameter on stack (beyond first 4)
                    // Stack args are ABOVE our frame: frameSize + (paramIdx-4)*4
                    // The caller pushed them before calling us
                    int offset = currentFunctionFrameSize + ((paramIdx - 4) * 4);
                    emit("lw $t0, " + to_string(offset) + "($fp)");
                }
                storeToVar(instr.result, "$t0");
                
                // Track parameter type in varTypes
                string baseName = instr.result;
                size_t hashPos = instr.result.find('#');
                if (hashPos != string::npos) {
                    baseName = instr.result.substr(0, hashPos);
                }
                Symbol* sym = symbolTable->lookuph(baseName);
                if (sym && sym->pointerDepth > 0) {
                    // Build type string: type + '*' * pointerDepth
                    string typeStr = sym->type;
                    for (int i = 0; i < sym->pointerDepth; i++) {
                        typeStr += "*";
                    }
                    varTypes[instr.result] = typeStr;
                }
            }
            break;
            
        default:
            emitComment("TODO: Unimplemented operation");
    }
}

//============================================================================
// ARITHMETIC OPERATIONS
//============================================================================

void CodeGenerator::generateAdd(const TACInstruction& instr) {
    // Check if we need float arithmetic
    if (isFloatType(instr.operand1) || isFloatType(instr.operand2) || isFloatType(instr.result)) {
        generateFloatAdd(instr);
        return;
    }
    
    // result = op1 + op2
    loadOperand(instr.operand1, "$t0");
    
    if (isImmediate(instr.operand2)) {
        emit("addi $t0, $t0, " + instr.operand2);
    } else {
        loadOperand(instr.operand2, "$t1");
        emit("add $t0, $t0, $t1");
    }
    
    storeToVar(instr.result, "$t0");
    
    // FIXED: Propagate pointer type information through ADD operations
    // If op1 is a pointer type, result is also that pointer type
    if (varTypes.find(instr.operand1) != varTypes.end()) {
        string type1 = varTypes[instr.operand1];
        if (type1.find('*') != string::npos) {
            // op1 is a pointer, propagate its type to result
            varTypes[instr.result] = type1;
        }
    }
}

void CodeGenerator::generateSub(const TACInstruction& instr) {
    // Check if we need float arithmetic
    if (isFloatType(instr.operand1) || isFloatType(instr.operand2) || isFloatType(instr.result)) {
        generateFloatSub(instr);
        return;
    }
    
    // result = op1 - op2
    loadOperand(instr.operand1, "$t0");
    
    if (isImmediate(instr.operand2)) {
        emit("subi $t0, $t0, " + instr.operand2);
    } else {
        loadOperand(instr.operand2, "$t1");
        emit("sub $t0, $t0, $t1");
    }
    
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateMul(const TACInstruction& instr) {
    // Check if we need float arithmetic
    if (isFloatType(instr.operand1) || isFloatType(instr.operand2) || isFloatType(instr.result)) {
        generateFloatMul(instr);
        return;
    }
    
    // result = op1 * op2
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("mul $t0, $t0, $t1");
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateDiv(const TACInstruction& instr) {
    // Check if we need float arithmetic
    if (isFloatType(instr.operand1) || isFloatType(instr.operand2) || isFloatType(instr.result)) {
        generateFloatDiv(instr);
        return;
    }
    
    // result = op1 / op2
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("div $t0, $t1");
    emit("mflo $t0");  // Quotient in lo
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateMod(const TACInstruction& instr) {
    // result = op1 % op2
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("div $t0, $t1");
    emit("mfhi $t0");  // Remainder in hi
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateNeg(const TACInstruction& instr) {
    // Check if we need float arithmetic
    if (isFloatType(instr.operand1) || isFloatType(instr.result)) {
        generateFloatNeg(instr);
        return;
    }
    
    // result = -op1
    loadOperand(instr.operand1, "$t0");
    emit("neg $t0, $t0");
    storeToVar(instr.result, "$t0");
}

//============================================================================
// FLOAT ARITHMETIC OPERATIONS
//============================================================================

void CodeGenerator::generateFloatAdd(const TACInstruction& instr) {
    // result = op1 + op2 (float)
    emitComment("Float addition: " + instr.result + " = " + instr.operand1 + " + " + instr.operand2);
    
    loadFloatOperand(instr.operand1, "$f0");
    loadFloatOperand(instr.operand2, "$f2");
    emit("add.s $f0, $f0, $f2");  // Single precision float add
    storeFloatToVar(instr.result, "$f0");
    
    // Mark result as float type
    setVarType(instr.result, "float");
}

void CodeGenerator::generateFloatSub(const TACInstruction& instr) {
    // result = op1 - op2 (float)
    emitComment("Float subtraction: " + instr.result + " = " + instr.operand1 + " - " + instr.operand2);
    
    loadFloatOperand(instr.operand1, "$f0");
    loadFloatOperand(instr.operand2, "$f2");
    emit("sub.s $f0, $f0, $f2");  // Single precision float subtract
    storeFloatToVar(instr.result, "$f0");
    
    // Mark result as float type
    setVarType(instr.result, "float");
}

void CodeGenerator::generateFloatMul(const TACInstruction& instr) {
    // result = op1 * op2 (float)
    emitComment("Float multiplication: " + instr.result + " = " + instr.operand1 + " * " + instr.operand2);
    
    loadFloatOperand(instr.operand1, "$f0");
    loadFloatOperand(instr.operand2, "$f2");
    emit("mul.s $f0, $f0, $f2");  // Single precision float multiply
    storeFloatToVar(instr.result, "$f0");
    
    // Mark result as float type
    setVarType(instr.result, "float");
}

void CodeGenerator::generateFloatDiv(const TACInstruction& instr) {
    // result = op1 / op2 (float)
    emitComment("Float division: " + instr.result + " = " + instr.operand1 + " / " + instr.operand2);
    
    loadFloatOperand(instr.operand1, "$f0");
    loadFloatOperand(instr.operand2, "$f2");
    emit("div.s $f0, $f0, $f2");  // Single precision float divide
    storeFloatToVar(instr.result, "$f0");
    
    // Mark result as float type
    setVarType(instr.result, "float");
}

void CodeGenerator::generateFloatNeg(const TACInstruction& instr) {
    // result = -op1 (float)
    emitComment("Float negation: " + instr.result + " = -" + instr.operand1);
    
    loadFloatOperand(instr.operand1, "$f0");
    emit("neg.s $f0, $f0");  // Single precision float negate
    storeFloatToVar(instr.result, "$f0");
    
    // Mark result as float type
    setVarType(instr.result, "float");
}

//============================================================================
// LOGICAL OPERATIONS (Short-circuit evaluation)
//============================================================================

void CodeGenerator::generateLogicalAnd(const TACInstruction& instr) {
    // result = op1 && op2 (short-circuit)
    string falseLabel = newLabel("and_false");
    string endLabel = newLabel("and_end");
    
    loadOperand(instr.operand1, "$t0");
    emit("beqz $t0, " + falseLabel);
    
    loadOperand(instr.operand2, "$t0");
    emit("beqz $t0, " + falseLabel);
    
    // Both true
    emit("li $t0, 1");
    emit("j " + endLabel);
    
    emitLabel(falseLabel);
    emit("li $t0, 0");
    
    emitLabel(endLabel);
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateLogicalOr(const TACInstruction& instr) {
    // result = op1 || op2 (short-circuit)
    string trueLabel = newLabel("or_true");
    string endLabel = newLabel("or_end");
    
    loadOperand(instr.operand1, "$t0");
    emit("bnez $t0, " + trueLabel);
    
    loadOperand(instr.operand2, "$t0");
    emit("bnez $t0, " + trueLabel);
    
    // Both false
    emit("li $t0, 0");
    emit("j " + endLabel);
    
    emitLabel(trueLabel);
    emit("li $t0, 1");
    
    emitLabel(endLabel);
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateLogicalNot(const TACInstruction& instr) {
    // result = !op1
    loadOperand(instr.operand1, "$t0");
    emit("seq $t0, $t0, $zero");  // Set if equal to zero
    storeToVar(instr.result, "$t0");
}

//============================================================================
// BITWISE OPERATIONS
//============================================================================

void CodeGenerator::generateBitAnd(const TACInstruction& instr) {
    // result = op1 & op2
    loadOperand(instr.operand1, "$t0");
    
    if (isImmediate(instr.operand2)) {
        emit("andi $t0, $t0, " + instr.operand2);
    } else {
        loadOperand(instr.operand2, "$t1");
        emit("and $t0, $t0, $t1");
    }
    
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateBitOr(const TACInstruction& instr) {
    // result = op1 | op2
    loadOperand(instr.operand1, "$t0");
    
    if (isImmediate(instr.operand2)) {
        emit("ori $t0, $t0, " + instr.operand2);
    } else {
        loadOperand(instr.operand2, "$t1");
        emit("or $t0, $t0, $t1");
    }
    
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateBitXor(const TACInstruction& instr) {
    // result = op1 ^ op2
    loadOperand(instr.operand1, "$t0");
    
    if (isImmediate(instr.operand2)) {
        emit("xori $t0, $t0, " + instr.operand2);
    } else {
        loadOperand(instr.operand2, "$t1");
        emit("xor $t0, $t0, $t1");
    }
    
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateBitNot(const TACInstruction& instr) {
    // result = ~op1
    loadOperand(instr.operand1, "$t0");
    emit("nor $t0, $t0, $zero");  // NOR with zero = NOT
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateShl(const TACInstruction& instr) {
    // result = op1 << op2
    loadOperand(instr.operand1, "$t0");
    
    if (isImmediate(instr.operand2)) {
        emit("sll $t0, $t0, " + instr.operand2);
    } else {
        loadOperand(instr.operand2, "$t1");
        emit("sllv $t0, $t0, $t1");
    }
    
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateShr(const TACInstruction& instr) {
    // result = op1 >> op2
    loadOperand(instr.operand1, "$t0");
    
    if (isImmediate(instr.operand2)) {
        emit("sra $t0, $t0, " + instr.operand2);  // Arithmetic shift
    } else {
        loadOperand(instr.operand2, "$t1");
        emit("srav $t0, $t0, $t1");
    }
    
    storeToVar(instr.result, "$t0");
}

//============================================================================
// COMPARISON OPERATIONS
//============================================================================

void CodeGenerator::generateEq(const TACInstruction& instr) {
    // result = (op1 == op2)
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("seq $t0, $t0, $t1");
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateNe(const TACInstruction& instr) {
    // result = (op1 != op2)
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("sne $t0, $t0, $t1");
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateLt(const TACInstruction& instr) {
    // result = (op1 < op2)
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("slt $t0, $t0, $t1");
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateLe(const TACInstruction& instr) {
    // result = (op1 <= op2)
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("sle $t0, $t0, $t1");
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateGt(const TACInstruction& instr) {
    // result = (op1 > op2)
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("sgt $t0, $t0, $t1");
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateGe(const TACInstruction& instr) {
    // result = (op1 >= op2)
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("sge $t0, $t0, $t1");
    storeToVar(instr.result, "$t0");
}

//============================================================================
// MEMORY OPERATIONS
//============================================================================

void CodeGenerator::generateAssign(const TACInstruction& instr) {
    // result = op1
    
    // Check if we're assigning a float
    if (isFloatType(instr.operand1)) {
        loadFloatOperand(instr.operand1, "$f0");
        storeFloatToVar(instr.result, "$f0");
        setVarType(instr.result, "float");
        return;
    }
    
    loadOperand(instr.operand1, "$t0");
    storeToVar(instr.result, "$t0");
    
    // FIXED: Propagate type information for pointer assignments
    if (varTypes.find(instr.operand1) != varTypes.end()) {
        varTypes[instr.result] = varTypes[instr.operand1];
    }
}

void CodeGenerator::generateLoad(const TACInstruction& instr) {
    // result = *op1 (pointer dereference)
    loadOperand(instr.operand1, "$t0");  // Get address
    
    // FIXED: Check if this is loading a float
    if (varTypes.find(instr.operand1) != varTypes.end()) {
        string ptrType = varTypes[instr.operand1];
        // Count pointer levels (number of '*' characters)
        int ptrLevel = 0;
        for (char c : ptrType) {
            if (c == '*') ptrLevel++;
        }
        
        // If pointer level is 2 or more (e.g., char**, int**), we're loading a pointer (4 bytes)
        if (ptrLevel >= 2) {
            emit("lw $t0, 0($t0)");  // Load word for pointer-to-pointer
            storeToVar(instr.result, "$t0");
            // Result is one level less pointer (e.g., char** -> char*)
            string resultType = ptrType.substr(0, ptrType.length() - 1);
            varTypes[instr.result] = resultType;
            return;
        }
        
        // Single level pointer: check what it points to
        // Remove the '*' to get base type (e.g., "float*" -> "float")
        size_t starPos = ptrType.find('*');
        if (starPos != string::npos) {
            string baseType = ptrType.substr(0, starPos);
            if (baseType == "float" || baseType == "double") {
                // Load float
                emit("lwc1 $f0, 0($t0)");  // Load word coprocessor 1
                storeFloatToVar(instr.result, "$f0");
                setVarType(instr.result, "float");
                return;
            } else if (baseType == "char") {
                emit("lb $t0, 0($t0)");  // Load byte for char*
                storeToVar(instr.result, "$t0");
                return;
            } else if (baseType == "short") {
                emit("lh $t0, 0($t0)");  // Load halfword for short*
                storeToVar(instr.result, "$t0");
                return;
            }
        }
    }
    
    // Fallback: Check if operand1 is a pointer symbol
    string baseName = instr.operand1;
    size_t hashPos = instr.operand1.find('#');
    if (hashPos != string::npos) {
        baseName = instr.operand1.substr(0, hashPos);
    }
    
    Symbol* sym = symbolTable->lookuph(baseName);
    if (sym && sym->pointerDepth > 0) {
        // It's a pointer - check what it points to
        if (sym->type == "float") {
            emit("lwc1 $f0, 0($t0)");
            storeFloatToVar(instr.result, "$f0");
            setVarType(instr.result, "float");
            return;
        } else if (sym->type == "char") {
            emit("lb $t0, 0($t0)");  // Load byte for char*
        } else if (sym->type == "short") {
            emit("lh $t0, 0($t0)");  // Load halfword for short*
        } else {
            emit("lw $t0, 0($t0)");  // Load word for int*, pointers
        }
    } else {
        // Default to word load
        emit("lw $t0, 0($t0)");
    }
    
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateStore(const TACInstruction& instr) {
    // *result = op1 (store through pointer)
    
    // FIXED: Check if storing a float
    bool isFloatStore = false;
    if (varTypes.find(instr.result) != varTypes.end()) {
        string ptrType = varTypes[instr.result];
        // Remove the '*' to get base type (e.g., "float*" -> "float")
        size_t starPos = ptrType.find('*');
        if (starPos != string::npos) {
            string baseType = ptrType.substr(0, starPos);
            if (baseType == "float" || baseType == "double") {
                isFloatStore = true;
            }
        }
    }
    
    if (isFloatStore || isFloatType(instr.operand1)) {
        // Store float
        loadFloatOperand(instr.operand1, "$f0");  // Get value
        loadOperand(instr.result, "$t1");          // Get address
        emit("swc1 $f0, 0($t1)");  // Store word coprocessor 1
        return;
    }
    
    loadOperand(instr.operand1, "$t0");  // Get value
    loadOperand(instr.result, "$t1");    // Get address
    
    // FIXED: Use proper type-aware store instruction
    // First check varTypes map which tracks pointer types from ADDRESS and ADD operations
    if (varTypes.find(instr.result) != varTypes.end()) {
        string ptrType = varTypes[instr.result];
        // Count pointer levels (number of '*' characters)
        int ptrLevel = 0;
        for (char c : ptrType) {
            if (c == '*') ptrLevel++;
        }
        
        // If pointer level is 2 or more (e.g., char**, int**), we're storing a pointer (4 bytes)
        if (ptrLevel >= 2) {
            emit("sw $t0, 0($t1)");  // Store word for pointer-to-pointer
            return;
        }
        
        // Single level pointer: check what it points to
        size_t starPos = ptrType.find('*');
        if (starPos != string::npos) {
            string baseType = ptrType.substr(0, starPos);
            if (baseType == "char") {
                emit("sb $t0, 0($t1)");  // Store byte for char*
                return;
            } else if (baseType == "short") {
                emit("sh $t0, 0($t1)");  // Store halfword for short*
                return;
            }
        }
    }
    
    // Fallback: Check if result is a pointer symbol
    string baseName = instr.result;
    size_t hashPos = instr.result.find('#');
    if (hashPos != string::npos) {
        baseName = instr.result.substr(0, hashPos);
    }
    
    Symbol* sym = symbolTable->lookuph(baseName);
    if (sym && sym->pointerDepth > 0) {
        // It's a pointer - check what it points to
        // BUT: if it's an array-of-pointers, we're storing pointers (4 bytes), not base type elements
        if (sym->isArray && sym->pointerDepth > 0) {
            // Array-of-pointers: elements are pointers (4 bytes)
            emit("sw $t0, 0($t1)");  // Store word for pointer
        } else if (sym->type == "char") {
            emit("sb $t0, 0($t1)");  // Store byte for char*
        } else if (sym->type == "short") {
            emit("sh $t0, 0($t1)");  // Store halfword for short*
        } else {
            emit("sw $t0, 0($t1)");  // Store word for int*, float*, pointers
        }
    } else {
        // Default to word store
        emit("sw $t0, 0($t1)");
    }
}

void CodeGenerator::generateAddress(const TACInstruction& instr) {
    // result = &op1
    // FIXED: Check if operand is a global/static variable
    if (staticVars.find(instr.operand1) != staticVars.end()) {
        // Global variable - load its address
        string label = staticVars[instr.operand1];
        emit("la $t0, " + label);
        storeToVar(instr.result, "$t0");
    } else {
        // Local variable - calculate stack address
        int offset = getVarOffset(instr.operand1);
        emit("addi $t0, $fp, " + to_string(offset));
        storeToVar(instr.result, "$t0");
    }
    
    // FIXED: Track type information for address operations
    // result is now a pointer to operand1's type
    string baseName = instr.operand1;
    size_t hashPos = instr.operand1.find('#');
    if (hashPos != string::npos) {
        baseName = instr.operand1.substr(0, hashPos);
    }
    
    Symbol* sym = symbolTable->lookuph(baseName);
    if (sym) {
        // Store "type*" to indicate this is a pointer to that type
        // For array-of-pointers (e.g., char* arr[3]), the element is already a pointer (char*),
        // so taking the array's address gives us char** (pointer to char*)
        if (sym->isArray && sym->pointerDepth > 0) {
            // Array-of-pointers: element type is type* → address gives type**
            string elemType = sym->type;
            for (int i = 0; i < sym->pointerDepth + 1; i++) {
                elemType += "*";
            }
            varTypes[instr.result] = elemType;
        } else {
            varTypes[instr.result] = sym->type + "*";
        }
    }
}

void CodeGenerator::generateArrayLoad(const TACInstruction& instr) {
    // result = arr[index]
    // operand1 = base address, operand2 = index
    loadOperand(instr.operand1, "$t0");  // Base address
    loadOperand(instr.operand2, "$t1");  // Index
    
    // FIXED: Check if this is a float array
    string baseName = instr.operand1;
    size_t hashPos = instr.operand1.find('#');
    if (hashPos != string::npos) {
        baseName = instr.operand1.substr(0, hashPos);
    }
    
    Symbol* sym = symbolTable->lookuph(baseName);
    bool isFloatArray = (sym && sym->type == "float");
    
    // FIXED: Use proper element size scaling based on array type
    TypeInfo typeInfo = getTypeInfo(instr.operand1);
    int elementSize = typeInfo.size;
    
    if (elementSize == 1) {
        // char array - no scaling needed
        emit("add $t0, $t0, $t1");           // Address = base + index
        emit(typeInfo.loadInstr + " $t0, 0($t0)");  // Load byte
        storeToVar(instr.result, "$t0");
    } else if (elementSize == 2) {
        // short array - scale by 2
        emit("sll $t1, $t1, 1");             // Index * 2
        emit("add $t0, $t0, $t1");           // Address = base + offset
        emit(typeInfo.loadInstr + " $t0, 0($t0)");  // Load halfword
        storeToVar(instr.result, "$t0");
    } else if (isFloatArray) {
        // float array - scale by 4, use lwc1
        emit("sll $t1, $t1, 2");             // Index * 4
        emit("add $t0, $t0, $t1");           // Address = base + offset
        emit("lwc1 $f0, 0($t0)");            // Load float
        storeFloatToVar(instr.result, "$f0");
        setVarType(instr.result, "float");
    } else {
        // int/pointer array - scale by 4
        emit("sll $t1, $t1, 2");             // Index * 4 (word size)
        emit("add $t0, $t0, $t1");           // Address = base + offset
        emit(typeInfo.loadInstr + " $t0, 0($t0)");  // Load word
        storeToVar(instr.result, "$t0");
    }
}

void CodeGenerator::generateArrayStore(const TACInstruction& instr) {
    // arr[index] = value
    // result = base address, operand1 = index, operand2 = value
    loadOperand(instr.result, "$t0");    // Base address
    loadOperand(instr.operand1, "$t1");  // Index
    
    // FIXED: Check if this is a float array
    string baseName = instr.result;
    size_t hashPos = instr.result.find('#');
    if (hashPos != string::npos) {
        baseName = instr.result.substr(0, hashPos);
    }
    
    Symbol* sym = symbolTable->lookuph(baseName);
    bool isFloatArray = (sym && sym->type == "float");
    
    // FIXED: Use proper element size scaling based on array type
    TypeInfo typeInfo = getTypeInfo(instr.result);
    int elementSize = typeInfo.size;
    
    if (elementSize == 1) {
        // char array - no scaling needed
        emit("add $t0, $t0, $t1");           // Address = base + index
        loadOperand(instr.operand2, "$t2");  // Value
        emit(typeInfo.storeInstr + " $t2, 0($t0)");  // Store byte
    } else if (elementSize == 2) {
        // short array - scale by 2
        emit("sll $t1, $t1, 1");             // Index * 2
        emit("add $t0, $t0, $t1");           // Address = base + offset
        loadOperand(instr.operand2, "$t2");  // Value
        emit(typeInfo.storeInstr + " $t2, 0($t0)");  // Store halfword
    } else if (isFloatArray) {
        // float array - scale by 4, use swc1
        emit("sll $t1, $t1, 2");             // Index * 4
        emit("add $t0, $t0, $t1");           // Address = base + offset
        loadFloatOperand(instr.operand2, "$f0");  // Value
        emit("swc1 $f0, 0($t0)");            // Store float
    } else {
        // int/pointer array - scale by 4
        emit("sll $t1, $t1, 2");             // Index * 4
        emit("add $t0, $t0, $t1");           // Address = base + offset
        loadOperand(instr.operand2, "$t2");  // Value
        emit(typeInfo.storeInstr + " $t2, 0($t0)");  // Store word
    }
}

//============================================================================
// CONTROL FLOW
//============================================================================

void CodeGenerator::generateLabel(const TACInstruction& instr) {
    // Skip function entry labels (func_XXX:) since FUNC_BEGIN handles them
    if (instr.result.find("func_") == 0 && instr.result.find("_epilogue") == std::string::npos &&
        instr.result.find("_start") == std::string::npos && instr.result.find("_end") == std::string::npos &&
        instr.result.find("_cond") == std::string::npos && instr.result.find("_update") == std::string::npos) {
        // This looks like a function entry label, skip it
        return;
    }
    emitLabel(instr.result);
}

void CodeGenerator::generateGoto(const TACInstruction& instr) {
    emit("j " + instr.result);
}

void CodeGenerator::generateIfGoto(const TACInstruction& instr) {
    // if (condition) goto label
    loadOperand(instr.operand1, "$t0");
    emit("bnez $t0, " + instr.result);
}

void CodeGenerator::generateIfFalseGoto(const TACInstruction& instr) {
    // if (!condition) goto label
    loadOperand(instr.operand1, "$t0");
    emit("beqz $t0, " + instr.result);
}

//============================================================================
// FUNCTION OPERATIONS
//============================================================================

void CodeGenerator::generateFuncBegin(const TACInstruction& instr) {
    currentFunction = instr.result;
    inFunction = true;
    functionParamCount = instr.paramCount;  // Get parameter count from instruction
    varOffsets.clear();
    currentOffset = 0;  // Will be set after frame setup
    registers.spillAll();
    
    // Get pre-calculated frame size
    currentFunctionFrameSize = 64;  // Default minimum
    if (functionFrameSizes.find(currentFunction) != functionFrameSizes.end()) {
        currentFunctionFrameSize = functionFrameSizes[currentFunction];
    }
    
    emitBlankLine();
    // Add func_ prefix to avoid conflicts with MIPS instructions (add, sub, etc.)
    // Exception: keep 'main' as-is since SPIM expects it
    string funcLabel = (instr.result == "main") ? instr.result : "func_" + instr.result;
    emitLabel(funcLabel);
    
    // Function prologue - CORRECT ORDER WITH PROPER FRAME SIZE
    // 1. Allocate FULL frame (includes locals, temporaries, saved regs, params)
    emit("addi $sp, $sp, -" + to_string(currentFunctionFrameSize));
    
    // 2. Save return address and frame pointer at TOP of frame
    emit("sw $ra, " + to_string(currentFunctionFrameSize - 4) + "($sp)");  // $ra at top
    emit("sw $fp, " + to_string(currentFunctionFrameSize - 8) + "($sp)");  // $fp below $ra
    
    // 3. Set new frame pointer
    emit("move $fp, $sp");
    
    // NOTE: We do NOT save $a0-$a3 here automatically.
    // They will be accessed directly when needed via GET_PARAM.
    // Stack parameters (5+) are already on stack at 16($fp), 20($fp), etc.
    currentOffset = -16;  // Start after $ra and $fp
}

void CodeGenerator::generateFuncEnd(const TACInstruction& instr) {
    // Function epilogue - CORRECT ORDER WITH PROPER FRAME SIZE
    string funcLabel = (instr.result == "main") ? instr.result : "func_" + instr.result;
    emitLabel(funcLabel + "_epilogue");
    
    // Use the same frame size as prologue
    int frameSpace = currentFunctionFrameSize;
    
    // 1. Restore frame pointer and return address from TOP of frame
    emit("lw $fp, " + to_string(frameSpace - 8) + "($sp)");  // Restore $fp
    emit("lw $ra, " + to_string(frameSpace - 4) + "($sp)");  // Restore $ra
    
    // 2. Deallocate FULL stack frame
    emit("addi $sp, $sp, " + to_string(frameSpace));
    
    // 3. Return
    emit("jr $ra");
    
    inFunction = false;
    emitBlankLine();
}

void CodeGenerator::generateParam(const TACInstruction& instr) {
    // Queue parameter for function call
    // Parameter value is in operand1, not result!
    paramQueue.push_back(instr.operand1);
}

void CodeGenerator::generateCall(const TACInstruction& instr) {
    // Special handling for printf - convert to SPIM syscalls
    if (instr.operand1 == "printf") {
        if (paramQueue.size() >= 2) {
            // Parse the format string at compile time
            string formatParam = paramQueue[0];
            
            // Extract the actual string literal if it's a string constant
            string formatStr = "";
            if (varToStringLiteral.find(formatParam) != varToStringLiteral.end()) {
                formatStr = varToStringLiteral[formatParam];
            }
            
            // Parse format string and emit appropriate syscalls
            if (!formatStr.empty()) {
                size_t pos = 0;
                int argIndex = 1; // Start from paramQueue[1]
                
                while (pos < formatStr.length()) {
                    // Find next format specifier
                    size_t specPos = formatStr.find('%', pos);
                    
                    if (specPos == string::npos) {
                        // No more format specifiers - print remaining text
                        string remaining = formatStr.substr(pos);
                        if (!remaining.empty()) {
                            string label = "_str_" + to_string(labelCounter++);
                            dynamicDataSection.push_back(label + ": .asciiz \"" + remaining + "\"");
                            emit("la $a0, " + label);
                            emit("li $v0, 4");       // print_string
                            emit("syscall");
                        }
                        break;
                    }
                    
                    // Print text before %
                    if (specPos > pos) {
                        string before = formatStr.substr(pos, specPos - pos);
                        string label = "_str_" + to_string(labelCounter++);
                        dynamicDataSection.push_back(label + ": .asciiz \"" + before + "\"");
                        emit("la $a0, " + label);
                        emit("li $v0, 4");       // print_string
                        emit("syscall");
                    }
                    
                    // Handle format specifier
                    if (specPos + 1 < formatStr.length()) {
                        char spec = formatStr[specPos + 1];
                        // Check for precision specifier like %.1f
                        size_t formatEnd = specPos + 2;
                        if (specPos + 2 < formatStr.length() && formatStr[specPos + 1] == '.') {
                            // Skip precision digits
                            while (formatEnd < formatStr.length() && isdigit(formatStr[formatEnd])) {
                                formatEnd++;
                            }
                            if (formatEnd < formatStr.length()) {
                                spec = formatStr[formatEnd];
                                formatEnd++;
                            }
                        }
                        
                        if (spec == 'd' && argIndex < static_cast<int>(paramQueue.size())) {
                            // Print integer
                            loadOperand(paramQueue[argIndex], "$a0");
                            emit("li $v0, 1");       // print_int
                            emit("syscall");
                            argIndex++;
                            pos = formatEnd;
                        } else if ((spec == 'f' || spec == 'e' || spec == 'g') && argIndex < static_cast<int>(paramQueue.size())) {
                            // Print float - load into $f12 and use syscall 2
                            // NOTE: SPIM syscall 2 always prints with default precision (8 decimals)
                            // and does not respect format specifiers like %.1f
                            string floatArg = paramQueue[argIndex];
                            if (isFloatType(floatArg)) {
                                // Load float directly into $f12
                                loadFloatOperand(floatArg, "$f12");
                            } else {
                                // Load as integer and convert
                                loadOperand(floatArg, "$t0");
                                emit("mtc1 $t0, $f12");       // Move to coprocessor 1 (FPU)
                                emit("cvt.s.w $f12, $f12");   // Convert int to float
                            }
                            emit("li $v0, 2");            // print_float syscall
                            emit("syscall");
                            argIndex++;
                            pos = formatEnd;
                        } else if (spec == 's' && argIndex < static_cast<int>(paramQueue.size())) {
                            // Print string
                            loadOperand(paramQueue[argIndex], "$a0");
                            emit("li $v0, 4");       // print_string
                            emit("syscall");
                            argIndex++;
                            pos = formatEnd;
                        } else if (spec == 'c' && argIndex < static_cast<int>(paramQueue.size())) {
                            // Print character
                            loadOperand(paramQueue[argIndex], "$a0");
                            emit("li $v0, 11");      // print_char
                            emit("syscall");
                            argIndex++;
                            pos = formatEnd;
                        } else if (spec == '%') {
                            // Literal % - print it
                            string label = "_str_" + to_string(labelCounter++);
                            dynamicDataSection.push_back(label + ": .asciiz \"%\"");
                            emit("la $a0, " + label);
                            emit("li $v0, 4");       // print_string
                            emit("syscall");
                            pos = formatEnd;
                        } else {
                            // Unknown format specifier - skip it
                            pos = formatEnd;
                        }
                    } else {
                        break;
                    }
                }
            } else {
                // Fallback: just print integer and newline
                loadOperand(paramQueue[1], "$a0");
                emit("li $v0, 1");       // print_int
                emit("syscall");
                emit("la $a0, _newline");
                emit("li $v0, 4");       // print_string
                emit("syscall");
            }
        } else if (paramQueue.size() == 1) {
            // Just format string, no arguments - print it
            loadOperand(paramQueue[0], "$a0");
            emit("li $v0, 4");       // print_string syscall
            emit("syscall");
        }
        
        paramQueue.clear();
        return;
    }
    
    // Special handling for scanf - convert to SPIM syscalls
    if (instr.operand1 == "scanf") {
        if (paramQueue.size() >= 2) {
            // Get format string
            string formatParam = paramQueue[0];
            string formatStr = "";
            if (varToStringLiteral.find(formatParam) != varToStringLiteral.end()) {
                formatStr = varToStringLiteral[formatParam];
            }
            
            // Parse format string and read values
            if (!formatStr.empty()) {
                size_t pos = 0;
                int argIndex = 1;
                
                while (pos < formatStr.length()) {
                    size_t specPos = formatStr.find('%', pos);
                    
                    if (specPos == string::npos) {
                        break;
                    }
                    
                    // Handle format specifier
                    if (specPos + 1 < formatStr.length()) {
                        char spec = formatStr[specPos + 1];
                        if (spec == 'd' && argIndex < static_cast<int>(paramQueue.size())) {
                            // Read integer
                            emit("li $v0, 5");       // read_int syscall
                            emit("syscall");
                            // Store to address in parameter (scanf uses pointers)
                            loadOperand(paramQueue[argIndex], "$t0");
                            emit("sw $v0, 0($t0)");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == 'f' && argIndex < static_cast<int>(paramQueue.size())) {
                            // Read float
                            emit("li $v0, 6");       // read_float syscall
                            emit("syscall");
                            // Store to address
                            loadOperand(paramQueue[argIndex], "$t0");
                            emit("swc1 $f0, 0($t0)");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == 's' && argIndex < static_cast<int>(paramQueue.size())) {
                            // Read string
                            loadOperand(paramQueue[argIndex], "$a0");  // Buffer address
                            emit("li $a1, 256");     // Max length
                            emit("li $v0, 8");       // read_string syscall
                            emit("syscall");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == 'c' && argIndex < static_cast<int>(paramQueue.size())) {
                            // Read character (read_int and take low byte)
                            emit("li $v0, 12");      // read_char syscall
                            emit("syscall");
                            loadOperand(paramQueue[argIndex], "$t0");
                            emit("sb $v0, 0($t0)");
                            argIndex++;
                            pos = specPos + 2;
                        } else {
                            pos = specPos + 2;
                        }
                    } else {
                        break;
                    }
                }
            } else {
                // Fallback - just read one integer
                emit("li $v0, 5");       // read_int
                emit("syscall");
                if (paramQueue.size() >= 2) {
                    loadOperand(paramQueue[1], "$t0");
                    emit("sw $v0, 0($t0)");
                }
            }
        }
        
        paramQueue.clear();
        return;
    }
    
    // MIPS calling convention for other functions:
    // First 4 params in $a0-$a3
    // Remaining params on stack ABOVE the return address
    
    // Use the parameter count from the CALL instruction, not the queue size
    // (The queue may have extra params from nested calls)
    int numParams = instr.paramCount;
    
    // Sanity check: make sure we have enough params in queue
    if (numParams > (int)paramQueue.size()) {
        numParams = paramQueue.size();
    }
    
    // Use the LAST numParams from the queue (for nested calls)
    int startIdx = paramQueue.size() - numParams;
    
    int stackParams = (numParams > 4) ? (numParams - 4) : 0;
    
    // 1. Reserve space for stack parameters (if any)
    if (stackParams > 0) {
        int stackSpace = stackParams * 4;
        emit("addi $sp, $sp, -" + to_string(stackSpace));
    }
    
    // 2. Pass parameters (use only the last numParams from queue)
    for (int i = 0; i < numParams; i++) {
        if (i < 4) {
            // Use $a0-$a3 for first 4 parameters
            string argReg = "$a" + to_string(i);
            loadOperand(paramQueue[startIdx + i], argReg);
        } else {
            // Push to stack for parameters beyond 4
            loadOperand(paramQueue[startIdx + i], "$t0");
            int offset = (i - 4) * 4;
            emit("sw $t0, " + to_string(offset) + "($sp)");
        }
    }
    
    // 3. Call function (add func_ prefix to avoid conflicts with MIPS instructions)
    // Exception: keep 'main' as-is
    string callTarget = (instr.operand1 == "main") ? instr.operand1 : "func_" + instr.operand1;
    emit("jal " + callTarget);
    
    // 4. Clean up stack parameters (if any)
    if (stackParams > 0) {
        int stackSpace = stackParams * 4;
        emit("addi $sp, $sp, " + to_string(stackSpace));
    }
    
    // 5. Get return value
    if (!instr.result.empty()) {
        storeToVar(instr.result, "$v0");
    }
    
    // Remove only the parameters we used from the queue
    // (Keep any params for outer function calls)
    for (int i = 0; i < numParams; i++) {
        if (!paramQueue.empty()) {
            paramQueue.pop_back();
        }
    }
}

void CodeGenerator::generateReturn(const TACInstruction& instr) {
    // Return value is in operand1, not result!
    if (!instr.operand1.empty()) {
        loadOperand(instr.operand1, "$v0");
    }
    string funcLabel = (currentFunction == "main") ? currentFunction : "func_" + currentFunction;
    emit("j " + funcLabel + "_epilogue");
}

//============================================================================
// TYPE CONVERSION
//============================================================================

void CodeGenerator::generateCast(const TACInstruction& instr) {
    // Simple cast: just copy value (more complex casts could be added)
    loadOperand(instr.operand1, "$t0");
    storeToVar(instr.result, "$t0");
}

//============================================================================
// OUTPUT
//============================================================================

void CodeGenerator::writeToFile(const string& filename) {
    ofstream out(filename);
    if (!out) {
        cerr << "Error: Cannot open file " << filename << " for writing" << endl;
        return;
    }
    
    for (const auto& line : asmCode) {
        out << line << endl;
    }
    
    out.close();
}

void CodeGenerator::printAssembly(ostream& out) const {
    for (const auto& line : asmCode) {
        out << line << endl;
    }
}
