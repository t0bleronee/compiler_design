#include "assembly_generator.h"
#include <iostream>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <cctype>

AssemblyGenerator::AssemblyGenerator(vector<TACInstruction>& instrs, 
                                     SymbolTable* symTab, 
                                     ofstream& out)
    : instructions(instrs), symbolTable(symTab), output(out), 
      currentStackOffset(0), maxStackSize(0), labelCounter(0), stringCounter(0),
      hasReturnStatement(false) {
    
    // Initialize available x86 registers (32-bit)
    // We reserve some registers for special purposes:
    // eax - general purpose, return values, arithmetic
    // ebx - general purpose
    // ecx - general purpose, loop counter, shift operations
    // edx - general purpose, division operations
    // esi - source index
    // edi - destination index
    // ebp - base pointer (frame pointer) - RESERVED
    // esp - stack pointer - RESERVED
    
    availableRegisters = {"eax", "ebx", "ecx", "edx", "esi", "edi"};
}

// ============================================================================
// MAIN GENERATION ENTRY POINT
// ============================================================================

void AssemblyGenerator::generate() {
    emitComment("=======================================================");
    emitComment("Generated x86 Assembly Code");
    emitComment("Architecture: x86 (32-bit)");
    emitComment("=======================================================");
    emit("");
    
    // Generate data section first (if any global data exists)
    generateDataSection();
    
    // Generate code section
    emit(".section .text");
    emit(".globl main");
    emit("");
    
    // Process each TAC instruction
    for (size_t i = 0; i < instructions.size(); i++) {
        const TACInstruction& instr = instructions[i];
        
        // Add comment showing original TAC instruction
        emitComment("TAC: " + instr.toString());
        
        switch (instr.opcode) {
            case TACOp::FUNC_BEGIN:
                currentFunction = instr.result;
                variableToStackOffset.clear();
                tempToRegister.clear();
                currentStackOffset = 0;
                paramList.clear();
                hasReturnStatement = false;  // Reset for new function
                generatePrologue(instr.result);
                break;
                
            case TACOp::FUNC_END:
                if (!hasReturnStatement) {
                    // Only generate epilogue if there was no explicit return
                    generateEpilogue();
                }
                currentFunction = "";
                emit("");
                break;
                
            case TACOp::ADD:
            case TACOp::SUB:
            case TACOp::MUL:
            case TACOp::DIV:
            case TACOp::MOD:
            case TACOp::NEG:
                generateArithmetic(instr);
                break;
                
            case TACOp::EQ:
            case TACOp::NE:
            case TACOp::LT:
            case TACOp::LE:
            case TACOp::GT:
            case TACOp::GE:
                generateComparison(instr);
                break;
                
            case TACOp::AND:
            case TACOp::OR:
            case TACOp::NOT:
                generateLogical(instr);
                break;
                
            case TACOp::BIT_AND:
            case TACOp::BIT_OR:
            case TACOp::BIT_XOR:
            case TACOp::SHL:
            case TACOp::SHR:
            case TACOp::BIT_NOT:
                generateBitwise(instr);
                break;
                
            case TACOp::ASSIGN:
                generateAssignment(instr);
                break;
                
            case TACOp::LOAD:
                generateLoad(instr);
                break;
                
            case TACOp::STORE:
                generateStore(instr);
                break;
                
            case TACOp::GOTO:
                generateGoto(instr);
                break;
                
            case TACOp::IF_GOTO:
            case TACOp::IF_FALSE_GOTO:
                generateConditionalJump(instr);
                break;
                
            case TACOp::LABEL:
                generateLabel(instr);
                break;
                
            case TACOp::CALL:
                generateFunctionCall(instr);
                break;
                
            case TACOp::RETURN:
                generateReturn(instr);
                break;
                
            case TACOp::PARAM:
                generateParam(instr);
                break;
                
            case TACOp::CAST:
                generateCast(instr);
                break;
                
            case TACOp::CONST:
                generateConst(instr);
                break;
                
            case TACOp::ADDRESS:
                generateAddress(instr);
                break;
                
            default:
                emitComment("Warning: Unhandled TAC operation");
                break;
        }
        
        emit("");
    }
}

// ============================================================================
// DATA SECTION GENERATION
// ============================================================================

void AssemblyGenerator::generateDataSection() {
    if (stringLiterals.empty() && dataSection.empty()) {
        return;
    }
    
    emit(".section .data");
    
    // Add string literals
    for (const auto& pair : stringLiterals) {
        emit(pair.second + ":");
        emit("    .string " + pair.first);
    }
    
    // Add other data
    for (const string& data : dataSection) {
        emit(data);
    }
    
    emit("");
}

// ============================================================================
// FUNCTION PROLOGUE AND EPILOGUE
// ============================================================================

int AssemblyGenerator::calculateFunctionStackSize(const string& funcName) {
    set<string> uniqueVars;
    bool inFunction = false;
    
    for (const auto& instr : instructions) {
        if (instr.opcode == TACOp::FUNC_BEGIN && instr.result == funcName) {
            inFunction = true;
            continue;
        } else if (instr.opcode == TACOp::FUNC_END) {
            if (inFunction) break;
        }
        
        if (!inFunction) continue;
        
        // Collect all variables and temporaries that need stack space
        if (!instr.result.empty() && !isConstant(instr.result) && !isRegister(instr.result)) {
            uniqueVars.insert(instr.result);
        }
        if (!instr.operand1.empty() && !isConstant(instr.operand1) && !isRegister(instr.operand1)) {
            uniqueVars.insert(instr.operand1);
        }
        if (!instr.operand2.empty() && !isConstant(instr.operand2) && !isRegister(instr.operand2)) {
            uniqueVars.insert(instr.operand2);
        }
    }
    
    return uniqueVars.size() * 4; // Each variable takes 4 bytes (32-bit)
}

void AssemblyGenerator::generatePrologue(const string& funcName) {
    string asmLabel = sanitizeLabel(funcName);
    
    emit(asmLabel + ":");
    emit("    push ebp");
    emit("    mov ebp, esp");
    
    // Calculate stack space needed
    int stackSize = calculateFunctionStackSize(funcName);
    functionStackSize[funcName] = stackSize;
    
    if (stackSize > 0) {
        emitComment("Allocate " + to_string(stackSize) + " bytes for local variables");
        emit("    sub esp, " + to_string(stackSize));
        maxStackSize = stackSize;
    }
    
    // Assign stack offsets to variables
    int offset = -4;
    bool inFunction = false;
    set<string> assignedVars;
    
    for (const auto& instr : instructions) {
        if (instr.opcode == TACOp::FUNC_BEGIN && instr.result == funcName) {
            inFunction = true;
            continue;
        } else if (instr.opcode == TACOp::FUNC_END) {
            if (inFunction) break;
        }
        
        if (!inFunction) continue;
        
        // Assign offsets to result variables
        if (!instr.result.empty() && !isConstant(instr.result) && 
            !isRegister(instr.result) && assignedVars.find(instr.result) == assignedVars.end()) {
            variableToStackOffset[instr.result] = offset;
            assignedVars.insert(instr.result);
            offset -= 4;
        }
    }
    
    emit("");
}

void AssemblyGenerator::generateEpilogue() {
    // Only generate epilogue if needed (for functions without explicit return)
    // In most cases, the return statement will have already generated the epilogue
    emitComment("Function epilogue (fallthrough - should not reach here)");
    emit("    mov esp, ebp");
    emit("    pop ebp");
    emit("    ret");
}

// ============================================================================
// ARITHMETIC OPERATIONS
// ============================================================================

void AssemblyGenerator::generateArithmetic(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string op1 = getOperandLocation(instr.operand1);
    string op2;
    
    if (!instr.operand2.empty()) {
        op2 = getOperandLocation(instr.operand2);
    }
    
    switch (instr.opcode) {
        case TACOp::ADD:
            emit("    mov eax, " + op1);
            if (!instr.operand2.empty()) {
                if (isConstant(instr.operand2)) {
                    emit("    add eax, " + op2);
                } else {
                    emit("    mov edx, " + op2);
                    emit("    add eax, edx");
                }
            }
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::SUB:
            emit("    mov eax, " + op1);
            if (!instr.operand2.empty()) {
                if (isConstant(instr.operand2)) {
                    emit("    sub eax, " + op2);
                } else {
                    emit("    mov edx, " + op2);
                    emit("    sub eax, edx");
                }
            }
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::MUL:
            emit("    mov eax, " + op1);
            if (isConstant(instr.operand2)) {
                emit("    mov edx, " + op2);
                emit("    imul eax, edx");
            } else {
                emit("    mov edx, " + op2);
                emit("    imul eax, edx");
            }
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::DIV:
            emit("    mov eax, " + op1);
            emit("    cdq");  // Sign extend eax to edx:eax
            emit("    mov ecx, " + op2);  // Use ecx to avoid clobbering edx
            emit("    idiv ecx");
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::MOD:
            emit("    mov eax, " + op1);
            emit("    cdq");
            emit("    mov ecx, " + op2);  // Use ecx to avoid clobbering edx
            emit("    idiv ecx");
            emit("    mov " + dest + ", edx");  // Remainder is in edx
            break;
            
        case TACOp::NEG:
            emit("    mov eax, " + op1);
            emit("    neg eax");
            emit("    mov " + dest + ", eax");
            break;
            
        default:
            break;
    }
}

// ============================================================================
// COMPARISON OPERATIONS
// ============================================================================

void AssemblyGenerator::generateComparison(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string op1 = getOperandLocation(instr.operand1);
    string op2 = getOperandLocation(instr.operand2);
    
    emit("    mov eax, " + op1);
    if (isConstant(instr.operand2)) {
        emit("    cmp eax, " + op2);
    } else {
        emit("    mov edx, " + op2);
        emit("    cmp eax, edx");
    }
    
    string setInstr;
    switch (instr.opcode) {
        case TACOp::EQ: setInstr = "sete"; break;
        case TACOp::NE: setInstr = "setne"; break;
        case TACOp::LT: setInstr = "setl"; break;
        case TACOp::LE: setInstr = "setle"; break;
        case TACOp::GT: setInstr = "setg"; break;
        case TACOp::GE: setInstr = "setge"; break;
        default: setInstr = "sete"; break;
    }
    
    emit("    " + setInstr + " al");
    emit("    movzx eax, al");  // Zero-extend al to eax
    emit("    mov " + dest + ", eax");
}

// ============================================================================
// LOGICAL OPERATIONS
// ============================================================================

void AssemblyGenerator::generateLogical(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string op1 = getOperandLocation(instr.operand1);
    string op2;
    
    if (!instr.operand2.empty()) {
        op2 = getOperandLocation(instr.operand2);
    }
    
    switch (instr.opcode) {
        case TACOp::AND:
            emit("    mov eax, " + op1);
            emit("    cmp eax, 0");
            emit("    je .L_and_false_" + to_string(labelCounter));
            emit("    mov eax, " + op2);
            emit("    cmp eax, 0");
            emit("    je .L_and_false_" + to_string(labelCounter));
            emit("    mov DWORD PTR " + dest + ", 1");
            emit("    jmp .L_and_end_" + to_string(labelCounter));
            emit(".L_and_false_" + to_string(labelCounter) + ":");
            emit("    mov DWORD PTR " + dest + ", 0");
            emit(".L_and_end_" + to_string(labelCounter) + ":");
            labelCounter++;
            break;
            
        case TACOp::OR:
            emit("    mov eax, " + op1);
            emit("    cmp eax, 0");
            emit("    jne .L_or_true_" + to_string(labelCounter));
            emit("    mov eax, " + op2);
            emit("    cmp eax, 0");
            emit("    jne .L_or_true_" + to_string(labelCounter));
            emit("    mov DWORD PTR " + dest + ", 0");
            emit("    jmp .L_or_end_" + to_string(labelCounter));
            emit(".L_or_true_" + to_string(labelCounter) + ":");
            emit("    mov DWORD PTR " + dest + ", 1");
            emit(".L_or_end_" + to_string(labelCounter) + ":");
            labelCounter++;
            break;
            
        case TACOp::NOT:
            emit("    mov eax, " + op1);
            emit("    cmp eax, 0");
            emit("    sete al");
            emit("    movzx eax, al");
            emit("    mov " + dest + ", eax");
            break;
            
        default:
            break;
    }
}

// ============================================================================
// BITWISE OPERATIONS
// ============================================================================

void AssemblyGenerator::generateBitwise(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string op1 = getOperandLocation(instr.operand1);
    string op2;
    
    if (!instr.operand2.empty()) {
        op2 = getOperandLocation(instr.operand2);
    }
    
    switch (instr.opcode) {
        case TACOp::BIT_AND:
            emit("    mov eax, " + op1);
            emit("    mov edx, " + op2);
            emit("    and eax, edx");
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::BIT_OR:
            emit("    mov eax, " + op1);
            emit("    mov edx, " + op2);
            emit("    or eax, edx");
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::BIT_XOR:
            emit("    mov eax, " + op1);
            emit("    mov edx, " + op2);
            emit("    xor eax, edx");
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::SHL:
            emit("    mov eax, " + op1);
            emit("    mov ecx, " + op2);
            emit("    shl eax, cl");
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::SHR:
            emit("    mov eax, " + op1);
            emit("    mov ecx, " + op2);
            emit("    shr eax, cl");
            emit("    mov " + dest + ", eax");
            break;
            
        case TACOp::BIT_NOT:
            emit("    mov eax, " + op1);
            emit("    not eax");
            emit("    mov " + dest + ", eax");
            break;
            
        default:
            break;
    }
}

// ============================================================================
// MEMORY OPERATIONS
// ============================================================================

void AssemblyGenerator::generateAssignment(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string src = getOperandLocation(instr.operand1);
    
    if (isConstant(instr.operand1)) {
        emit("    mov DWORD PTR " + dest + ", " + src);
    } else {
        emit("    mov eax, " + src);
        emit("    mov " + dest + ", eax");
    }
}

void AssemblyGenerator::generateLoad(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string addr = getOperandLocation(instr.operand1);
    
    emit("    mov eax, " + addr);
    emit("    mov edx, DWORD PTR [eax]");
    emit("    mov " + dest + ", edx");
}

void AssemblyGenerator::generateStore(const TACInstruction& instr) {
    string addr = getOperandLocation(instr.result);
    string value = getOperandLocation(instr.operand1);
    
    emit("    mov eax, " + addr);
    if (isConstant(instr.operand1)) {
        emit("    mov DWORD PTR [eax], " + value);
    } else {
        emit("    mov edx, " + value);
        emit("    mov DWORD PTR [eax], edx");
    }
}

void AssemblyGenerator::generateAddress(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string var = instr.operand1;
    
    // Remove any # suffix from variable name
    size_t hashPos = var.find('#');
    if (hashPos != string::npos) {
        var = var.substr(0, hashPos);
    }
    
    // Check if variable has a stack offset
    if (variableToStackOffset.find(instr.operand1) != variableToStackOffset.end()) {
        int offset = variableToStackOffset[instr.operand1];
        string offsetStr = (offset < 0) ? to_string(offset) : ("+" + to_string(offset));
        emit("    lea eax, [ebp" + offsetStr + "]");
        emit("    mov " + dest + ", eax");
    } else {
        // Global variable or array
        emit("    lea eax, " + var);
        emit("    mov " + dest + ", eax");
    }
}

void AssemblyGenerator::generatePointerOp(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string op = getOperandLocation(instr.operand1);
    
    // For now, treat pointer operations as load/store
    // LOAD handles dereferencing: *ptr
    emit("    mov eax, " + op);
    emit("    mov edx, DWORD PTR [eax]");
    emit("    mov " + dest + ", edx");
}

// ============================================================================
// CONTROL FLOW OPERATIONS
// ============================================================================

void AssemblyGenerator::generateGoto(const TACInstruction& instr) {
    string label = getAssemblyLabel(instr.result);
    emit("    jmp " + label);
}

void AssemblyGenerator::generateConditionalJump(const TACInstruction& instr) {
    string condition = getOperandLocation(instr.operand1);
    string label = getAssemblyLabel(instr.result);
    
    emit("    mov eax, " + condition);
    emit("    cmp eax, 0");
    
    if (instr.opcode == TACOp::IF_GOTO) {
        emit("    jne " + label);  // Jump if not equal to zero (true)
    } else if (instr.opcode == TACOp::IF_FALSE_GOTO) {
        emit("    je " + label);   // Jump if equal to zero (false)
    }
}

void AssemblyGenerator::generateLabel(const TACInstruction& instr) {
    string label = getAssemblyLabel(instr.result);
    emit(label + ":");
}

// ============================================================================
// FUNCTION CALL OPERATIONS
// ============================================================================

void AssemblyGenerator::generateParam(const TACInstruction& instr) {
    // Collect parameters for the upcoming function call
    paramList.push_back(instr.result);
}

void AssemblyGenerator::generateFunctionCall(const TACInstruction& instr) {
    string funcName = instr.operand1;
    string result = instr.result;
    
    // Push parameters in reverse order (right to left for x86 cdecl)
    for (int i = paramList.size() - 1; i >= 0; i--) {
        string param = getOperandLocation(paramList[i]);
        if (isConstant(paramList[i])) {
            emit("    push " + param);
        } else {
            emit("    mov eax, " + param);
            emit("    push eax");
        }
    }
    
    // Call the function
    emit("    call " + sanitizeLabel(funcName));
    
    // Clean up parameters from stack
    if (!paramList.empty()) {
        int paramBytes = paramList.size() * 4;
        emit("    add esp, " + to_string(paramBytes));
    }
    
    // Store return value if needed
    if (!result.empty() && result != "void") {
        string dest = getOperandLocation(result);
        emit("    mov " + dest + ", eax");
    }
    
    // Clear parameter list for next call
    paramList.clear();
}

void AssemblyGenerator::generateReturn(const TACInstruction& instr) {
    // Load return value into EAX register (if any)
    // Note: Return value is in operand1, not result!
    if (!instr.operand1.empty() && instr.operand1 != "void") {
        string retValue = getOperandLocation(instr.operand1);
        emit("    mov eax, " + retValue);
    }
    
    // Mark that we've seen a return statement
    hasReturnStatement = true;
    
    // Generate epilogue and return
    emit("    mov esp, ebp");
    emit("    pop ebp");
    emit("    ret");
}

// ============================================================================
// SPECIAL OPERATIONS
// ============================================================================

void AssemblyGenerator::generateCast(const TACInstruction& instr) {
    // For now, treat cast as simple assignment
    // TODO: Handle float/int conversions properly
    string dest = getOperandLocation(instr.result);
    string src = getOperandLocation(instr.operand1);
    
    emit("    mov eax, " + src);
    emit("    mov " + dest + ", eax");
}

void AssemblyGenerator::generateConst(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string value = instr.operand1;
    
    emit("    mov DWORD PTR " + dest + ", " + value);
}

// ============================================================================
// UTILITY METHODS
// ============================================================================

string AssemblyGenerator::getOperandLocation(const string& operand) {
    if (operand.empty()) {
        return "";
    }
    
    // Check if it's a constant
    if (isConstant(operand)) {
        return operand;
    }
    
    // Check if it's a register
    if (isRegister(operand)) {
        return operand;
    }
    
    // Check if it's a variable with stack offset
    if (variableToStackOffset.find(operand) != variableToStackOffset.end()) {
        int offset = variableToStackOffset[operand];
        if (offset < 0) {
            return "[ebp-" + to_string(-offset) + "]";
        } else {
            return "[ebp+" + to_string(offset) + "]";
        }
    }
    
    // Check if it's a temporary that's been mapped to a register
    if (tempToRegister.find(operand) != tempToRegister.end()) {
        return tempToRegister[operand];
    }
    
    // Otherwise, assume it's a global variable or needs stack allocation
    // For simplicity, allocate stack space
    int offset = currentStackOffset - 4;
    currentStackOffset = offset;
    variableToStackOffset[operand] = offset;
    
    if (offset < 0) {
        return "[ebp-" + to_string(-offset) + "]";
    } else {
        return "[ebp+" + to_string(offset) + "]";
    }
}

bool AssemblyGenerator::isConstant(const string& operand) {
    if (operand.empty()) return false;
    
    // Check if it's a number (integer or float)
    if (operand[0] == '-' || operand[0] == '+' || isdigit(operand[0])) {
        return true;
    }
    
    // Check for CONST prefix
    if (operand.find("CONST") == 0) {
        return true;
    }
    
    return false;
}

bool AssemblyGenerator::isTemporary(const string& operand) {
    if (operand.empty()) return false;
    return operand[0] == 't' && operand.length() > 1 && isdigit(operand[1]);
}

bool AssemblyGenerator::isRegister(const string& operand) {
    return operand == "eax" || operand == "ebx" || operand == "ecx" || 
           operand == "edx" || operand == "esi" || operand == "edi" ||
           operand == "ebp" || operand == "esp";
}

bool AssemblyGenerator::isGlobalVariable(const string& varName) {
    // Check if variable is in global scope
    // For simplicity, assume variables not in current function's stack are global
    Symbol* sym = symbolTable->lookuph(varName);
    return sym != nullptr; // Simplified check
}

string AssemblyGenerator::sanitizeLabel(const string& label) {
    string result = label;
    
    // Remove any # suffixes
    size_t hashPos = result.find('#');
    if (hashPos != string::npos) {
        result = result.substr(0, hashPos);
    }
    
    // Replace invalid characters
    for (char& c : result) {
        if (!isalnum(c) && c != '_') {
            c = '_';
        }
    }
    
    return result;
}

string AssemblyGenerator::getAssemblyLabel(const string& tacLabel) {
    // Check if we've already created a mapping
    if (labelMap.find(tacLabel) != labelMap.end()) {
        return labelMap[tacLabel];
    }
    
    // Create a new assembly label
    string asmLabel = sanitizeLabel(tacLabel);
    labelMap[tacLabel] = asmLabel;
    return asmLabel;
}

void AssemblyGenerator::emit(const string& instruction) {
    output << instruction << endl;
}

void AssemblyGenerator::emitComment(const string& comment) {
    output << "    # " << comment << endl;
}

string AssemblyGenerator::addStringLiteral(const string& str) {
    // Check if string already exists
    if (stringLiterals.find(str) != stringLiterals.end()) {
        return stringLiterals[str];
    }
    
    // Create new string label
    string label = ".STR" + to_string(stringCounter++);
    stringLiterals[str] = label;
    return label;
}

int AssemblyGenerator::getTypeSize(const string& type) {
    // For x86 32-bit, most types are 4 bytes
    if (type == "char") return 1;
    if (type == "short") return 2;
    if (type == "int" || type == "float" || type == "pointer") return 4;
    if (type == "double" || type == "long long") return 8;
    return 4; // Default
}

void AssemblyGenerator::writeToFile(const string& filename) {
    // Already writing to output stream in emit()
    // This method is for compatibility
}

// Placeholder methods for register allocation (can be enhanced later)
string AssemblyGenerator::allocateRegister(const string& temp) {
    // Simple register allocation - just return eax for now
    return "eax";
}

void AssemblyGenerator::freeRegister(const string& reg) {
    // Placeholder
}

int AssemblyGenerator::getVariableOffset(const string& varName) {
    if (variableToStackOffset.find(varName) != variableToStackOffset.end()) {
        return variableToStackOffset[varName];
    }
    return 0;
}

void AssemblyGenerator::spillRegister(const string& reg) {
    // Placeholder for register spilling
}

void AssemblyGenerator::loadToRegister(const string& operand, const string& reg) {
    // Placeholder
    string loc = getOperandLocation(operand);
    emit("    mov " + reg + ", " + loc);
}
