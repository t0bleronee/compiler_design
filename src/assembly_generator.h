#ifndef ASSEMBLY_GENERATOR_H
#define ASSEMBLY_GENERATOR_H

#include <string>
#include <vector>
#include <map>
#include <fstream>
#include "ir_generator.h"
#include "symbol_table.h"

using namespace std;

class AssemblyGenerator {
private:
    vector<TACInstruction>& instructions;
    SymbolTable* symbolTable;
    ofstream& output;
    
    // Stack management
    map<string, int> variableToStackOffset;
    int currentStackOffset;
    string currentFunction;
    
    // Label management
    map<string, string> labelMap;
    int labelCounter;
    
    // Helper methods
    string getOperandLocation(const string& operand);
    int calculateFunctionStackSize(const string& funcName);
    
    // Code generation methods
    void generatePrologue(const string& funcName);
    void generateEpilogue();
    void generateArithmetic(const TACInstruction& instr);
    void generateComparison(const TACInstruction& instr);
    void generateAssignment(const TACInstruction& instr);
    void generateGoto(const TACInstruction& instr);
    void generateConditionalJump(const TACInstruction& instr);
    void generateLabel(const TACInstruction& instr);
    void generateReturn(const TACInstruction& instr);
    void generateConst(const TACInstruction& instr);
    
    // Utility methods
    void emit(const string& instruction);
    void emitComment(const string& comment);
    bool isConstant(const string& operand);
    string sanitizeLabel(const string& label);
    string getAssemblyLabel(const string& tacLabel);

public:
    AssemblyGenerator(vector<TACInstruction>& instrs, SymbolTable* symTab, ofstream& out);
    void generate();
};

#endif // ASSEMBLY_GENERATOR_H
