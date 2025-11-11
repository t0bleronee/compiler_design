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
    // Allocate new offset
    currentOffset -= 4;  // Assuming 4-byte integers
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
                tempOffset -= 4;
                tempOffsets[instr.result] = tempOffset;
                if (tempOffset < minOffset) {
                    minOffset = tempOffset;
                }
            }
        }
        
        // Check operand1 (if it's a variable/temp that will be loaded)
        if (!instr.operand1.empty() && !isImmediate(instr.operand1) && !isControlFlow) {
            if (tempOffsets.find(instr.operand1) == tempOffsets.end()) {
                tempOffset -= 4;
                tempOffsets[instr.operand1] = tempOffset;
                if (tempOffset < minOffset) {
                    minOffset = tempOffset;
                }
            }
        }
        
        // Check operand2
        if (!instr.operand2.empty() && !isImmediate(instr.operand2)) {
            if (tempOffsets.find(instr.operand2) == tempOffsets.end()) {
                tempOffset -= 4;
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
    for (const auto& instr : tacInstructions) {
        if (instr.opcode == TACOp::CONST && !instr.operand1.empty()) {
            if (instr.operand1[0] == '"') {
                // String literal
                getStringLabel(instr.operand1);
            } else if (isFloatLiteral(instr.operand1)) {
                // Float literal
                getOrCreateFloatLabel(instr.operand1);
            }
        }
    }
    
    // Emit all data
    for (const auto& line : dataSection) {
        asmCode.push_back("    " + line);
    }
    
    // Global/static variables
    for (const auto& entry : staticVars) {
        asmCode.push_back("    " + entry.second + ": .word 0");
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
    
    // MAIN PASS: Generate code
    for (const auto& instr : tacInstructions) {
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
            // Parameters 4+: on stack at 16($fp), 20($fp), 24($fp), ...
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
                    // Stack args start at 16($fp): above saved $ra and $fp
                    int offset = 16 + ((paramIdx - 4) * 4);
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
    emit("sw $t0, 0($t1)");              // Store value at address
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
    emitLabel(instr.result);
    
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
    emitLabel(instr.result + "_epilogue");
    
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
    
    // MIPS calling convention for other functions:
    // First 4 params in $a0-$a3
    // Remaining params on stack ABOVE the return address
    
    int numParams = paramQueue.size();
    int stackParams = (numParams > 4) ? (numParams - 4) : 0;
    
    // 1. Reserve space for stack parameters (if any)
    if (stackParams > 0) {
        int stackSpace = stackParams * 4;
        emit("addi $sp, $sp, -" + to_string(stackSpace));
    }
    
    // 2. Pass parameters
    for (int i = 0; i < numParams; i++) {
        if (i < 4) {
            // Use $a0-$a3 for first 4 parameters
            string argReg = "$a" + to_string(i);
            loadOperand(paramQueue[i], argReg);
        } else {
            // Push to stack for parameters beyond 4
            loadOperand(paramQueue[i], "$t0");
            int offset = (i - 4) * 4;
            emit("sw $t0, " + to_string(offset) + "($sp)");
        }
    }
    
    // 3. Call function
    emit("jal " + instr.operand1);
    
    // 4. Clean up stack parameters (if any)
    if (stackParams > 0) {
        int stackSpace = stackParams * 4;
        emit("addi $sp, $sp, " + to_string(stackSpace));
    }
    
    // 5. Get return value
    if (!instr.result.empty()) {
        storeToVar(instr.result, "$v0");
    }
    
    // Clear parameter queue
    paramQueue.clear();
}

void CodeGenerator::generateReturn(const TACInstruction& instr) {
    // Return value is in operand1, not result!
    if (!instr.operand1.empty()) {
        loadOperand(instr.operand1, "$v0");
    }
    emit("j " + currentFunction + "_epilogue");
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
