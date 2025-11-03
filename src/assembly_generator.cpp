#include "assembly_generator.h"
#include <iostream>
#include <set>

AssemblyGenerator::AssemblyGenerator(vector<TACInstruction>& instrs, 
                                     SymbolTable* symTab, 
                                     ofstream& out)
    : instructions(instrs), symbolTable(symTab), output(out), 
      currentStackOffset(0), labelCounter(0) {
}

void AssemblyGenerator::generate() {
    emitComment("Generated x86 Assembly Code");
    emit("");
    emit(".section .text");
    emit(".globl main");
    emit("");
    
    for (size_t i = 0; i < instructions.size(); i++) {
        const TACInstruction& instr = instructions[i];
        emitComment("TAC: " + instr.toString());
        
        switch (instr.opcode) {
            case TACOp::FUNC_BEGIN:
                currentFunction = instr.result;
                variableToStackOffset.clear();
                currentStackOffset = 0;
                generatePrologue(instr.result);
                break;
                
            case TACOp::FUNC_END:
                generateEpilogue();
                currentFunction = "";
                emit("");
                break;
                
            case TACOp::ADD:
            case TACOp::SUB:
            case TACOp::MUL:
            case TACOp::DIV:
            case TACOp::MOD:
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
                
            case TACOp::ASSIGN:
                generateAssignment(instr);
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
                
            case TACOp::RETURN:
                generateReturn(instr);
                break;
                
            case TACOp::CONST:
                generateConst(instr);
                break;
                
            default:
                emitComment("Warning: Unhandled TAC operation");
                break;
        }
        
        emit("");
    }
}

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
        
        if (!instr.result.empty() && !isConstant(instr.result)) {
            uniqueVars.insert(instr.result);
        }
        if (!instr.operand1.empty() && !isConstant(instr.operand1)) {
            uniqueVars.insert(instr.operand1);
        }
        if (!instr.operand2.empty() && !isConstant(instr.operand2)) {
            uniqueVars.insert(instr.operand2);
        }
    }
    
    return uniqueVars.size() * 4;
}

void AssemblyGenerator::generatePrologue(const string& funcName) {
    string asmLabel = sanitizeLabel(funcName);
    
    emit(asmLabel + ":");
    emit("    push ebp");
    emit("    mov ebp, esp");
    
    int stackSize = calculateFunctionStackSize(funcName);
    
    if (stackSize > 0) {
        emitComment("Allocate " + to_string(stackSize) + " bytes for local variables");
        emit("    sub esp, " + to_string(stackSize));
    }
    
    // Assign stack offsets
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
        
        if (!instr.result.empty() && !isConstant(instr.result) && 
            assignedVars.find(instr.result) == assignedVars.end()) {
            variableToStackOffset[instr.result] = offset;
            assignedVars.insert(instr.result);
            offset -= 4;
        }
    }
    
    emit("");
}

void AssemblyGenerator::generateEpilogue() {
    emitComment("Function epilogue");
    emit("    mov esp, ebp");
    emit("    pop ebp");
    emit("    ret");
}

void AssemblyGenerator::generateArithmetic(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string op1 = getOperandLocation(instr.operand1);
    string op2 = getOperandLocation(instr.operand2);
    
    emit("    mov eax, " + op1);
    
    switch (instr.opcode) {
        case TACOp::ADD:
            if (isConstant(instr.operand2)) {
                emit("    add eax, " + op2);
            } else {
                emit("    mov edx, " + op2);
                emit("    add eax, edx");
            }
            break;
            
        case TACOp::SUB:
            if (isConstant(instr.operand2)) {
                emit("    sub eax, " + op2);
            } else {
                emit("    mov edx, " + op2);
                emit("    sub eax, edx");
            }
            break;
            
        case TACOp::MUL:
            emit("    mov edx, " + op2);
            emit("    imul eax, edx");
            break;
            
        case TACOp::DIV:
            emit("    cdq");
            emit("    mov ecx, " + op2);
            emit("    idiv ecx");
            break;
            
        case TACOp::MOD:
            emit("    cdq");
            emit("    mov ecx, " + op2);
            emit("    idiv ecx");
            emit("    mov eax, edx");  // Remainder is in edx
            break;
            
        default:
            break;
    }
    
    emit("    mov " + dest + ", eax");
}

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
    emit("    movzx eax, al");
    emit("    mov " + dest + ", eax");
}

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
        emit("    jne " + label);
    } else {
        emit("    je " + label);
    }
}

void AssemblyGenerator::generateLabel(const TACInstruction& instr) {
    string label = getAssemblyLabel(instr.result);
    emit(label + ":");
}

void AssemblyGenerator::generateReturn(const TACInstruction& instr) {
    if (!instr.operand1.empty() && instr.operand1 != "void") {
        string retValue = getOperandLocation(instr.operand1);
        emit("    mov eax, " + retValue);
    }
    
    emit("    mov esp, ebp");
    emit("    pop ebp");
    emit("    ret");
}

void AssemblyGenerator::generateConst(const TACInstruction& instr) {
    string dest = getOperandLocation(instr.result);
    string value = instr.operand1;
    emit("    mov DWORD PTR " + dest + ", " + value);
}

string AssemblyGenerator::getOperandLocation(const string& operand) {
    if (operand.empty()) {
        return "";
    }
    
    if (isConstant(operand)) {
        return operand;
    }
    
    if (variableToStackOffset.find(operand) != variableToStackOffset.end()) {
        int offset = variableToStackOffset[operand];
        return "[ebp" + to_string(offset) + "]";
    }
    
    // Allocate new stack space
    int offset = currentStackOffset - 4;
    currentStackOffset = offset;
    variableToStackOffset[operand] = offset;
    return "[ebp" + to_string(offset) + "]";
}

bool AssemblyGenerator::isConstant(const string& operand) {
    if (operand.empty()) return false;
    return (operand[0] == '-' || operand[0] == '+' || isdigit(operand[0]));
}

string AssemblyGenerator::sanitizeLabel(const string& label) {
    string result = label;
    size_t hashPos = result.find('#');
    if (hashPos != string::npos) {
        result = result.substr(0, hashPos);
    }
    return result;
}

string AssemblyGenerator::getAssemblyLabel(const string& tacLabel) {
    if (labelMap.find(tacLabel) != labelMap.end()) {
        return labelMap[tacLabel];
    }
    
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
