#include "code_generator.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cctype>

using namespace std;

CodeGenerator::CodeGenerator(const vector<TACInstruction>& instructions, SymbolTable* symTab)
    : tacInstructions(instructions), symbolTable(symTab), 
      frameSize(0), currentOffset(0), stringCounter(0), floatCounter(0),
      functionParamCount(0), currentFunctionFrameSize(0), inFunction(false), labelCounter(0) {
    registers.init();
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

int CodeGenerator::getVarOffset(const string& varName) {
    if (varOffsets.find(varName) != varOffsets.end()) {
        return varOffsets[varName];
    }
    // Allocate new offset - check if it's an array
    int varSize = 4;  // Default size for scalar variables
    
    // Strip SSA suffix (e.g., arr#2 -> arr) to look up in symbol table
    string baseName = varName;
    size_t hashPos = varName.find('#');
    if (hashPos != string::npos) {
        baseName = varName.substr(0, hashPos);
    }
    
    Symbol* sym = symbolTable->lookuph(baseName);  // Use lookuph to search scope history
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
    varOffsets[varName] = currentOffset;
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
    int totalSpace = localsSpace + 8 + paramSpace;
    
    // Ensure minimum frame size and round up to 8-byte alignment for MIPS
    if (totalSpace < 16) totalSpace = 16;  // Minimum frame
    if (totalSpace % 8 != 0) {
        totalSpace = ((totalSpace / 8) + 1) * 8;
    }
    
    return totalSpace;
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
    dataSection.clear();
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
        
        // Insert dynamic data before .text
        if (it != asmCode.end()) {
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
    
    // Collect string and float literals from TAC (first pass)
    // Also collect global variable initializations (before any function)
    // And function-local static variables (those with __inited guard)
    std::map<std::string, int> globalInits;  // variable -> initial value
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
    
    // Emit all data
    for (const auto& line : dataSection) {
        asmCode.push_back("    " + line);
    }
    
    // Global/static variables with their initial values
    for (const auto& entry : staticVars) {
        int initValue = 0;
        auto it = globalInits.find(entry.first);
        if (it != globalInits.end()) {
            initValue = it->second;
        }
        // Sanitize label name (replace # with _)
        string label = entry.second;
        for (char& c : label) {
            if (c == '#') c = '_';
        }
        asmCode.push_back("    " + label + ": .word " + std::to_string(initValue));
        // Update the staticVars map with sanitized name
        const_cast<std::map<std::string, std::string>&>(staticVars)[entry.first] = label;
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
            } else if (isFloatLiteral(instr.operand1)) {
                // Float constant - must load from .data section
                emitComment("WARNING: Float " + instr.operand1 + " loaded as integer bits (proper FPU support needed)");
                string label = getOrCreateFloatLabel(instr.operand1);
                emit("la $t0, " + label);
                emit("lw $t0, 0($t0)");  // Load the float bits as int
            } else {
                // Integer constant
                emit("li $t0, " + instr.operand1);
            }
            storeToVar(instr.result, "$t0");
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
    // result = op1 + op2
    loadOperand(instr.operand1, "$t0");
    
    if (isImmediate(instr.operand2)) {
        emit("addi $t0, $t0, " + instr.operand2);
    } else {
        loadOperand(instr.operand2, "$t1");
        emit("add $t0, $t0, $t1");
    }
    
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateSub(const TACInstruction& instr) {
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
    // result = op1 * op2
    loadOperand(instr.operand1, "$t0");
    loadOperand(instr.operand2, "$t1");
    emit("mul $t0, $t0, $t1");
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateDiv(const TACInstruction& instr) {
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
    // result = -op1
    loadOperand(instr.operand1, "$t0");
    emit("neg $t0, $t0");
    storeToVar(instr.result, "$t0");
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
    loadOperand(instr.operand1, "$t0");
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateLoad(const TACInstruction& instr) {
    // result = *op1 (pointer dereference)
    loadOperand(instr.operand1, "$t0");  // Get address
    emit("lw $t0, 0($t0)");              // Load value at address
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateStore(const TACInstruction& instr) {
    // *result = op1 (store through pointer)
    loadOperand(instr.operand1, "$t0");  // Get value
    loadOperand(instr.result, "$t1");    // Get address
    
    // For now, always use sb (store byte) to avoid alignment issues with char arrays
    // TODO: Use proper type information to decide between sb/sh/sw
    emit("sb $t0, 0($t1)");  // Store byte (works for char, handles unaligned addresses)
}

void CodeGenerator::generateAddress(const TACInstruction& instr) {
    // result = &op1
    int offset = getVarOffset(instr.operand1);
    emit("addi $t0, $fp, " + to_string(offset));
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateArrayLoad(const TACInstruction& instr) {
    // result = arr[index]
    // operand1 = base address, operand2 = index
    loadOperand(instr.operand1, "$t0");  // Base address
    loadOperand(instr.operand2, "$t1");  // Index
    emit("sll $t1, $t1, 2");             // Index * 4 (word size)
    emit("add $t0, $t0, $t1");           // Address = base + offset
    emit("lw $t0, 0($t0)");              // Load value
    storeToVar(instr.result, "$t0");
}

void CodeGenerator::generateArrayStore(const TACInstruction& instr) {
    // arr[index] = value
    // result = base address, operand1 = index, operand2 = value
    loadOperand(instr.result, "$t0");    // Base address
    loadOperand(instr.operand1, "$t1");  // Index
    emit("sll $t1, $t1, 2");             // Index * 4
    emit("add $t0, $t0, $t1");           // Address = base + offset
    loadOperand(instr.operand2, "$t2");  // Value
    emit("sw $t2, 0($t0)");              // Store value
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
                        if (spec == 'd' && argIndex < paramQueue.size()) {
                            // Print integer
                            loadOperand(paramQueue[argIndex], "$a0");
                            emit("li $v0, 1");       // print_int
                            emit("syscall");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == 's' && argIndex < paramQueue.size()) {
                            // Print string
                            loadOperand(paramQueue[argIndex], "$a0");
                            emit("li $v0, 4");       // print_string
                            emit("syscall");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == 'c' && argIndex < paramQueue.size()) {
                            // Print character
                            loadOperand(paramQueue[argIndex], "$a0");
                            emit("li $v0, 11");      // print_char
                            emit("syscall");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == '%') {
                            // Literal % - print it
                            string label = "_str_" + to_string(labelCounter++);
                            dynamicDataSection.push_back(label + ": .asciiz \"%\"");
                            emit("la $a0, " + label);
                            emit("li $v0, 4");       // print_string
                            emit("syscall");
                            pos = specPos + 2;
                        } else {
                            // Unknown format specifier - skip it
                            pos = specPos + 2;
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
                        if (spec == 'd' && argIndex < paramQueue.size()) {
                            // Read integer
                            emit("li $v0, 5");       // read_int syscall
                            emit("syscall");
                            // Store to address in parameter (scanf uses pointers)
                            loadOperand(paramQueue[argIndex], "$t0");
                            emit("sw $v0, 0($t0)");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == 'f' && argIndex < paramQueue.size()) {
                            // Read float
                            emit("li $v0, 6");       // read_float syscall
                            emit("syscall");
                            // Store to address
                            loadOperand(paramQueue[argIndex], "$t0");
                            emit("swc1 $f0, 0($t0)");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == 's' && argIndex < paramQueue.size()) {
                            // Read string
                            loadOperand(paramQueue[argIndex], "$a0");  // Buffer address
                            emit("li $a1, 256");     // Max length
                            emit("li $v0, 8");       // read_string syscall
                            emit("syscall");
                            argIndex++;
                            pos = specPos + 2;
                        } else if (spec == 'c' && argIndex < paramQueue.size()) {
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
