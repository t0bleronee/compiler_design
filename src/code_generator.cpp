#include "code_generator.h"
#include <fstream>
#include <sstream>
#include <algorithm>
#include <iostream>

CodeGenerator::CodeGenerator(const vector<TACInstruction>& tacInstructions, SymbolTable* symbolTable)
    : instructions(tacInstructions), symTab(symbolTable), currentStackOffset(0), stringLabelCounter(0) {
    initializeRegisters();
}

void CodeGenerator::initializeRegisters() {
    // Temporary registers ($t0-$t9)
    for (int i = 0; i <= 9; i++) {
        tempRegisters.push_back("$t" + to_string(i));
    }
    
    // Saved registers ($s0-$s7)
    for (int i = 0; i <= 7; i++) {
        savedRegisters.push_back("$s" + to_string(i));
    }
}

bool CodeGenerator::isTemporary(const string& var) {
    return var.length() > 0 && var[0] == 't' && isdigit(var[1]);
}

bool CodeGenerator::isConstant(const string& var) {
    if (var.empty()) return false;
    if (var[0] == '-' || isdigit(var[0])) return true;
    if (var[0] == '\'' && var[var.length()-1] == '\'') return true; // Character literal
    return false;
}

bool CodeGenerator::isStringLiteral(const string& var) {
    return var.length() >= 2 && var[0] == '"' && var[var.length()-1] == '"';
}

string CodeGenerator::allocateRegister(const string& var) {
    if (varToReg.find(var) != varToReg.end()) {
        return varToReg[var];
    }
    
    // Try to find a free temporary register
    for (const string& reg : tempRegisters) {
        bool inUse = false;
        for (const auto& pair : varToReg) {
            if (pair.second == reg) {
                inUse = true;
                break;
            }
        }
        if (!inUse) {
            varToReg[var] = reg;
            return reg;
        }
    }
    
    // If all temp registers are used, spill to stack
    // For simplicity, we'll just use the first available temp register
    // and assume previous value was already used
    string reg = tempRegisters[0];
    varToReg[var] = reg;
    return reg;
}

void CodeGenerator::freeRegister(const string& var) {
    varToReg.erase(var);
}

string CodeGenerator::getRegister(const string& var) {
    if (varToReg.find(var) != varToReg.end()) {
        return varToReg[var];
    }
    return allocateRegister(var);
}

int CodeGenerator::allocateStackSpace(const string& var, int size) {
    currentStackOffset += size;
    varToStackOffset[var] = -currentStackOffset;
    return varToStackOffset[var];
}

int CodeGenerator::getStackOffset(const string& var) {
    if (varToStackOffset.find(var) != varToStackOffset.end()) {
        return varToStackOffset[var];
    }
    // Default allocation for non-arrays
    return allocateStackSpace(var, 4);
}

int CodeGenerator::getArraySize(const string& varName) {
    // Extract base name from unique name (e.g., "arr#2" -> "arr")
    string baseName = varName;
    size_t hashPos = varName.find('#');
    if (hashPos != string::npos) {
        baseName = varName.substr(0, hashPos);
    }
    
    cerr << "DEBUG getArraySize: varName=" << varName << ", baseName=" << baseName << endl;
    
    // Look up in symbol table
    Symbol* sym = symTab->lookup(baseName);
    if (sym) {
        cerr << "  Found symbol: isArray=" << sym->isArray << ", arrayDimensions.size()=" << sym->arrayDimensions.size() << endl;
        if (sym->isArray && !sym->arrayDimensions.empty()) {
            // Calculate total size: element_size * dim[0] * dim[1] * ...
            int totalSize = 4; // Assume 4 bytes per element (int)
            for (int dim : sym->arrayDimensions) {
                cerr << "    dimension: " << dim << endl;
                if (dim > 0) {
                    totalSize *= dim;
                }
            }
            cerr << "  Total array size: " << totalSize << " bytes" << endl;
            return totalSize;
        }
    } else {
        cerr << "  Symbol not found in table" << endl;
    }
    
    // Not an array or size unknown
    return 4;
}

void CodeGenerator::computeFunctionStackFrames() {
    string currentFunc = "";
    set<string> localVars;
    
    for (const auto& instr : instructions) {
        if (instr.opcode == TACOp::FUNC_BEGIN) {
            currentFunc = instr.operand1;
            localVars.clear();
        } else if (instr.opcode == TACOp::FUNC_END) {
            functionLocalVars[currentFunc] = vector<string>(localVars.begin(), localVars.end());
            // Calculate stack size: locals + saved registers + return address
            int stackSize = localVars.size() * 4 + 8; // +8 for $ra and $fp
            functionStackSize[currentFunc] = stackSize;
        } else if (!currentFunc.empty()) {
            // Track local variables (non-temporaries)
            if (!instr.result.empty() && !isTemporary(instr.result) && !isConstant(instr.result)) {
                localVars.insert(instr.result);
            }
        }
    }
}

void CodeGenerator::emit(const string& instruction) {
    textSection.push_back(instruction);
}

void CodeGenerator::emitData(const string& directive) {
    dataSection.push_back(directive);
}

void CodeGenerator::emitComment(const string& comment) {
    emit("    # " + comment);
}

void CodeGenerator::loadOperand(const string& operand, const string& reg) {
    if (isConstant(operand)) {
        // Handle numeric constants
        if (operand[0] == '\'') {
            // Character literal
            int charValue = (int)operand[1];
            emit("    li " + reg + ", " + to_string(charValue));
        } else {
            emit("    li " + reg + ", " + operand);
        }
    } else if (isStringLiteral(operand)) {
        // Load address of string literal
        if (stringLabels.find(operand) == stringLabels.end()) {
            string label = "str_" + to_string(stringLabelCounter++);
            stringLabels[operand] = label;
            emitData(label + ": .asciiz " + operand);
        }
        emit("    la " + reg + ", " + stringLabels[operand]);
    } else {
        // Load from stack (both temporaries and variables)
        int offset = getStackOffset(operand);
        emit("    lw " + reg + ", " + to_string(offset) + "($fp)");
    }
}

void CodeGenerator::storeResult(const string& result, const string& reg) {
    // Always store results (both temporaries and variables) to stack
    // This ensures values are preserved across complex expressions
    int offset = getStackOffset(result);
    emit("    sw " + reg + ", " + to_string(offset) + "($fp)");
}

void CodeGenerator::genArithmetic(const TACInstruction& instr) {
    string resultReg = "$t0";
    string op1Reg = "$t8";
    string op2Reg = "$t9";
    
    // Load operands
    loadOperand(instr.operand1, op1Reg);
    
    if (instr.opcode != TACOp::NEG) {
        loadOperand(instr.operand2, op2Reg);
    }
    
    switch (instr.opcode) {
        case TACOp::ADD:
            emit("    add " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        case TACOp::SUB:
            emit("    sub " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        case TACOp::MUL:
            emit("    mul " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        case TACOp::DIV:
            emit("    div " + op1Reg + ", " + op2Reg);
            emit("    mflo " + resultReg);
            break;
        case TACOp::MOD:
            emit("    div " + op1Reg + ", " + op2Reg);
            emit("    mfhi " + resultReg);
            break;
        case TACOp::NEG:
            emit("    neg " + resultReg + ", " + op1Reg);
            break;
        default:
            break;
    }
    
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genComparison(const TACInstruction& instr) {
    string resultReg = "$t0";
    string op1Reg = "$t8";
    string op2Reg = "$t9";
    
    loadOperand(instr.operand1, op1Reg);
    loadOperand(instr.operand2, op2Reg);
    
    switch (instr.opcode) {
        case TACOp::EQ:
            emit("    seq " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        case TACOp::NE:
            emit("    sne " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        case TACOp::LT:
            emit("    slt " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        case TACOp::LE:
            emit("    sle " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        case TACOp::GT:
            emit("    sgt " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        case TACOp::GE:
            emit("    sge " + resultReg + ", " + op1Reg + ", " + op2Reg);
            break;
        default:
            break;
    }
    
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genLogical(const TACInstruction& instr) {
    string resultReg = "$t0";
    string op1Reg = "$t8";
    string op2Reg = "$t9";
    
    if (instr.opcode == TACOp::NOT) {
        loadOperand(instr.operand1, op1Reg);
        emit("    seq " + resultReg + ", " + op1Reg + ", $zero");
    } else {
        loadOperand(instr.operand1, op1Reg);
        loadOperand(instr.operand2, op2Reg);
        
        if (instr.opcode == TACOp::AND) {
            emit("    and " + resultReg + ", " + op1Reg + ", " + op2Reg);
            emit("    sltu " + resultReg + ", $zero, " + resultReg);
        } else if (instr.opcode == TACOp::OR) {
            emit("    or " + resultReg + ", " + op1Reg + ", " + op2Reg);
            emit("    sltu " + resultReg + ", $zero, " + resultReg);
        }
    }
    
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genBitwise(const TACInstruction& instr) {
    string resultReg = "$t0";
    string op1Reg = "$t8";
    string op2Reg = "$t9";
    
    loadOperand(instr.operand1, op1Reg);
    
    if (instr.opcode == TACOp::BIT_NOT) {
        emit("    not " + resultReg + ", " + op1Reg);
    } else {
        loadOperand(instr.operand2, op2Reg);
        
        switch (instr.opcode) {
            case TACOp::BIT_AND:
                emit("    and " + resultReg + ", " + op1Reg + ", " + op2Reg);
                break;
            case TACOp::BIT_OR:
                emit("    or " + resultReg + ", " + op1Reg + ", " + op2Reg);
                break;
            case TACOp::BIT_XOR:
                emit("    xor " + resultReg + ", " + op1Reg + ", " + op2Reg);
                break;
            case TACOp::SHL:
                emit("    sllv " + resultReg + ", " + op1Reg + ", " + op2Reg);
                break;
            case TACOp::SHR:
                emit("    srlv " + resultReg + ", " + op1Reg + ", " + op2Reg);
                break;
            default:
                break;
        }
    }
    
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genAssign(const TACInstruction& instr) {
    string resultReg = "$t0";
    
    // Check if operand1 is a variable that has never been assigned/initialized
    // This typically means it's an array name that needs its address taken
    if (!isTemporary(instr.operand1) && !isConstant(instr.operand1) && !isStringLiteral(instr.operand1)) {
        // Check if this variable has ever been written to
        if (varToStackOffset.find(instr.operand1) == varToStackOffset.end()) {
            // This is likely an array - allocate space and take its address
            int offset = allocateStackSpace(instr.operand1, 20);  // Allocate space for array
            emit("    addi " + resultReg + ", $fp, " + to_string(offset));
            storeResult(instr.result, resultReg);
            return;
        }
    }
    
    loadOperand(instr.operand1, resultReg);
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genLoad(const TACInstruction& instr) {
    string ptrReg = "$t8";
    string resultReg = getRegister(instr.result);
    
    loadOperand(instr.operand1, ptrReg);
    emit("    lw " + resultReg + ", 0(" + ptrReg + ")");
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genStore(const TACInstruction& instr) {
    string valueReg = "$t8";
    string ptrReg = "$t9";
    
    loadOperand(instr.operand1, valueReg);
    loadOperand(instr.result, ptrReg);
    emit("    sw " + valueReg + ", 0(" + ptrReg + ")");
}

void CodeGenerator::genAddress(const TACInstruction& instr) {
    string resultReg = getRegister(instr.result);
    
    // Check if this is an array and allocate proper space if not already allocated
    if (varToStackOffset.find(instr.operand1) == varToStackOffset.end()) {
        int arraySize = getArraySize(instr.operand1);
        allocateStackSpace(instr.operand1, arraySize);
    }
    
    int offset = getStackOffset(instr.operand1);
    emit("    addi " + resultReg + ", $fp, " + to_string(offset));
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genArrayLoad(const TACInstruction& instr) {
    string baseReg = "$t7";
    string indexReg = "$t8";
    string resultReg = getRegister(instr.result);
    
    loadOperand(instr.operand1, baseReg);
    loadOperand(instr.operand2, indexReg);
    
    // Calculate address: base + index
    emit("    add " + resultReg + ", " + baseReg + ", " + indexReg);
    emit("    lw " + resultReg + ", 0(" + resultReg + ")");
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genArrayStore(const TACInstruction& instr) {
    string baseReg = "$t7";
    string indexReg = "$t8";
    string valueReg = "$t9";
    string addrReg = "$t6";
    
    loadOperand(instr.result, baseReg);
    loadOperand(instr.operand1, indexReg);
    loadOperand(instr.operand2, valueReg);
    
    // Calculate address: base + index
    emit("    add " + addrReg + ", " + baseReg + ", " + indexReg);
    emit("    sw " + valueReg + ", 0(" + addrReg + ")");
}

void CodeGenerator::genGoto(const TACInstruction& instr) {
    emit("    j " + instr.result);
}

void CodeGenerator::genIfGoto(const TACInstruction& instr) {
    string condReg = "$t8";
    loadOperand(instr.operand1, condReg);
    emit("    bne " + condReg + ", $zero, " + instr.result);
}

void CodeGenerator::genIfFalseGoto(const TACInstruction& instr) {
    string condReg = "$t8";
    loadOperand(instr.operand1, condReg);
    emit("    beq " + condReg + ", $zero, " + instr.result);
}

void CodeGenerator::genLabel(const TACInstruction& instr) {
    // Skip function labels (func_*) as they're handled by FUNC_BEGIN
    string label = instr.result;
    if (label.substr(0, 5) == "func_") {
        return;  // Skip function labels, handled in FUNC_BEGIN
    }
    emit(label + ":");
}

void CodeGenerator::genParam(const TACInstruction& instr) {
    // Parameters are pushed onto the stack before a call
    string paramReg = "$t8";
    loadOperand(instr.operand1, paramReg);
    emit("    addi $sp, $sp, -4");
    emit("    sw " + paramReg + ", 0($sp)");
}

void CodeGenerator::genGetParam(const TACInstruction& instr) {
    // Load parameter from stack frame
    // Parameters are passed on the stack, starting at offset 8 from $fp (after $ra and old $fp)
    // param[0] is at 8($fp), param[1] at 12($fp), etc.
    
    // operand1 contains just the index as a string: "0", "1", "2", etc.
    int paramIndex = stoi(instr.operand1);
    
    // Calculate offset: 8 + (paramIndex * 4)
    int offset = 8 + (paramIndex * 4);
    
    string resultReg = getRegister(instr.result);
    emit("    lw " + resultReg + ", " + to_string(offset) + "($fp)");
    storeResult(instr.result, resultReg);
}

void CodeGenerator::genCall(const TACInstruction& instr) {
    // Call the function
    string funcLabel = instr.operand1;
    if (funcLabel.substr(0, 5) == "func_") {
        funcLabel = funcLabel.substr(5); // Remove "func_" prefix
    }
    
    emit("    jal " + funcLabel);
    
    // Clean up parameters from stack
    int paramCount = instr.paramCount;
    if (paramCount > 0) {
        emit("    addi $sp, $sp, " + to_string(paramCount * 4));
    }
    
    // Store return value if needed
    // CRITICAL: For temporaries, we must store to stack to avoid being overwritten
    if (!instr.result.empty()) {
        if (isTemporary(instr.result)) {
            // Always store call results to stack first to preserve them
            int offset = getStackOffset(instr.result);
            emit("    sw $v0, " + to_string(offset) + "($fp)");
        } else {
            string resultReg = getRegister(instr.result);
            emit("    move " + resultReg + ", $v0");
            storeResult(instr.result, resultReg);
        }
    }
}

void CodeGenerator::genReturn(const TACInstruction& instr) {
    if (!instr.operand1.empty()) {
        loadOperand(instr.operand1, "$v0");
    }
    // Jump to function epilogue (handled in FUNC_END)
    emit("    j " + currentFunction + "_epilogue");
}

void CodeGenerator::genFunctionBegin(const TACInstruction& instr) {
    currentFunction = instr.result;  // Function name is in result field
    varToReg.clear();
    varToStackOffset.clear();
    currentStackOffset = 0;
    
    // Function label
    emit("");
    emitComment("Function: " + currentFunction);
    
    // If the function is main, make it the entry point
    if (currentFunction == "main") {
        emit("main:");
    } else {
        emit(currentFunction + ":");
    }
    
    // Prologue
    emit("    # Prologue");
    emit("    addi $sp, $sp, -8");
    emit("    sw $ra, 4($sp)");
    emit("    sw $fp, 0($sp)");
    emit("    move $fp, $sp");
    
    // Reserve space for locals - will be patched later
    emit("    # Reserve space for local variables");
    
    // Placeholder - we'll track the actual size as we generate code
    // For now, don't allocate anything; we'll do it dynamically
    emit("    # STACK_ALLOC_PLACEHOLDER");
}

void CodeGenerator::genFunctionEnd(const TACInstruction& instr) {
    // Calculate actual stack space needed (round up to 16-byte alignment for safety)
    int stackSize = ((abs(currentStackOffset) + 15) / 16) * 16;
    
    // Find and replace the placeholder with actual allocation
    for (size_t i = 0; i < textSection.size(); i++) {
        if (textSection[i].find("# STACK_ALLOC_PLACEHOLDER") != string::npos) {
            if (stackSize > 0) {
                textSection[i] = "    addi $sp, $sp, -" + to_string(stackSize);
            } else {
                textSection[i] = "    # No local variables";
            }
            break;
        }
    }
    
    // Epilogue
    emit("");
    emit(currentFunction + "_epilogue:");
    emit("    # Epilogue");
    if (stackSize > 0) {
        emit("    addi $sp, $sp, " + to_string(stackSize));
    }
    emit("    move $sp, $fp");
    emit("    lw $fp, 0($sp)");
    emit("    lw $ra, 4($sp)");
    emit("    addi $sp, $sp, 8");
    emit("    jr $ra");
}

void CodeGenerator::genCast(const TACInstruction& instr) {
    // For basic types, cast is essentially a move
    string resultReg = getRegister(instr.result);
    loadOperand(instr.operand1, resultReg);
    storeResult(instr.result, resultReg);
}

void CodeGenerator::generateInstruction(const TACInstruction& instr) {
    switch (instr.opcode) {
        case TACOp::CONST:
            genAssign(instr);  // Handle CONST like assignment: result = operand1
            break;
            
        case TACOp::ADD:
        case TACOp::SUB:
        case TACOp::MUL:
        case TACOp::DIV:
        case TACOp::MOD:
        case TACOp::NEG:
            genArithmetic(instr);
            break;
            
        case TACOp::EQ:
        case TACOp::NE:
        case TACOp::LT:
        case TACOp::LE:
        case TACOp::GT:
        case TACOp::GE:
            genComparison(instr);
            break;
            
        case TACOp::AND:
        case TACOp::OR:
        case TACOp::NOT:
            genLogical(instr);
            break;
            
        case TACOp::BIT_AND:
        case TACOp::BIT_OR:
        case TACOp::BIT_XOR:
        case TACOp::SHL:
        case TACOp::SHR:
        case TACOp::BIT_NOT:
            genBitwise(instr);
            break;
            
        case TACOp::ASSIGN:
            genAssign(instr);
            break;
            
        case TACOp::LOAD:
            genLoad(instr);
            break;
            
        case TACOp::STORE:
            genStore(instr);
            break;
            
        case TACOp::ADDRESS:
            genAddress(instr);
            break;
            
        case TACOp::ARRAY_LOAD:
            genArrayLoad(instr);
            break;
            
        case TACOp::ARRAY_STORE:
            genArrayStore(instr);
            break;
            
        case TACOp::GOTO:
            genGoto(instr);
            break;
            
        case TACOp::IF_GOTO:
            genIfGoto(instr);
            break;
            
        case TACOp::IF_FALSE_GOTO:
            genIfFalseGoto(instr);
            break;
            
        case TACOp::LABEL:
            genLabel(instr);
            break;
            
        case TACOp::CALL:
            genCall(instr);
            break;
            
        case TACOp::RETURN:
            genReturn(instr);
            break;
            
        case TACOp::PARAM:
            genParam(instr);
            break;
            
        case TACOp::GET_PARAM:
            genGetParam(instr);
            break;
            
        case TACOp::FUNC_BEGIN:
            genFunctionBegin(instr);
            break;
            
        case TACOp::FUNC_END:
            genFunctionEnd(instr);
            break;
            
        case TACOp::CAST:
            genCast(instr);
            break;
            
        default:
            emitComment("Unhandled opcode");
            break;
    }
}

void CodeGenerator::generateDataSection() {
    emitData(".data");
    
    // Add any global string literals
    for (const auto& pair : stringLabels) {
        emitData(pair.second + ": .asciiz " + pair.first);
    }
    
    // Add newline for output
    emitData("newline: .asciiz \"\\n\"");
}

void CodeGenerator::generateTextSection() {
    emit(".text");
    emit(".globl main");
    emit("");
    
    // Generate code for each TAC instruction
    for (const auto& instr : instructions) {
        generateInstruction(instr);
    }
    
    // Add exit syscall at the end of main if needed
    emit("");
    emit("# Program exit");
    emit("_exit:");
    emit("    li $v0, 10");
    emit("    syscall");
}

void CodeGenerator::generate() {
    computeFunctionStackFrames();
    
    // First pass: collect string literals
    for (const auto& instr : instructions) {
        if (isStringLiteral(instr.operand1)) {
            if (stringLabels.find(instr.operand1) == stringLabels.end()) {
                string label = "str_" + to_string(stringLabelCounter++);
                stringLabels[instr.operand1] = label;
            }
        }
        if (isStringLiteral(instr.operand2)) {
            if (stringLabels.find(instr.operand2) == stringLabels.end()) {
                string label = "str_" + to_string(stringLabelCounter++);
                stringLabels[instr.operand2] = label;
            }
        }
    }
    
    generateDataSection();
    generateTextSection();
}

string CodeGenerator::getAssemblyCode() {
    stringstream ss;
    
    // Output data section
    for (const string& line : dataSection) {
        ss << line << endl;
    }
    
    ss << endl;
    
    // Output text section
    for (const string& line : textSection) {
        ss << line << endl;
    }
    
    return ss.str();
}

void CodeGenerator::writeToFile(const string& filename) {
    ofstream outFile(filename);
    if (!outFile) {
        cerr << "Error: Could not open file " << filename << " for writing" << endl;
        return;
    }
    
    outFile << getAssemblyCode();
    outFile.close();
    
    cout << "MIPS assembly code written to " << filename << endl;
}
