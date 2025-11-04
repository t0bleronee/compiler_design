#ifndef ASSEMBLY_GENERATOR_H
#define ASSEMBLY_GENERATOR_H

#include <string>
#include <vector>
#include <map>
#include <set>
#include <fstream>
#include "ir_generator.h"
#include "symbol_table.h"

using namespace std;

class AssemblyGenerator {
private:
    vector<TACInstruction>& instructions;
    SymbolTable* symbolTable;
    ofstream& output;
    
    // Register management
    vector<string> availableRegisters;
    map<string, string> tempToRegister;      // Maps temporaries to registers
    map<string, int> variableToStackOffset;   // Maps variables to stack offsets
    set<string> usedRegisters;
    
    // Stack frame management
    int currentStackOffset;
    int maxStackSize;
    string currentFunction;
    map<string, int> functionStackSize;      // Track stack size per function
    bool hasReturnStatement;                  // Track if current function has explicit return
    
    // Label management
    map<string, string> labelMap;            // Maps TAC labels to assembly labels
    int labelCounter;
    
    // Data section
    vector<string> dataSection;
    map<string, string> stringLiterals;      // Maps string constants to labels
    int stringCounter;
    
    // Parameter tracking
    vector<string> paramList;                // For function calls
    
    // Helper methods - Register management
    string allocateRegister(const string& temp);
    void freeRegister(const string& reg);
    string getOperandLocation(const string& operand);
    int getVariableOffset(const string& varName);
    int getTypeSize(const string& type);
    void spillRegister(const string& reg);
    void loadToRegister(const string& operand, const string& reg);
    
    // Code generation methods - Function structure
    void generatePrologue(const string& funcName);
    void generateEpilogue();
    
    // Code generation methods - Operations
    void generateArithmetic(const TACInstruction& instr);
    void generateComparison(const TACInstruction& instr);
    void generateLogical(const TACInstruction& instr);
    void generateBitwise(const TACInstruction& instr);
    
    // Code generation methods - Memory operations
    void generateAssignment(const TACInstruction& instr);
    void generateLoad(const TACInstruction& instr);
    void generateStore(const TACInstruction& instr);
    void generateAddress(const TACInstruction& instr);
    void generatePointerOp(const TACInstruction& instr);
    
    // Code generation methods - Control flow
    void generateGoto(const TACInstruction& instr);
    void generateConditionalJump(const TACInstruction& instr);
    void generateLabel(const TACInstruction& instr);
    
    // Code generation methods - Function calls
    void generateFunctionCall(const TACInstruction& instr);
    void generateReturn(const TACInstruction& instr);
    void generateParam(const TACInstruction& instr);
    
    // Code generation methods - Special operations
    void generateCast(const TACInstruction& instr);
    void generateConst(const TACInstruction& instr);
    
    // Utility methods
    void emit(const string& instruction);
    void emitComment(const string& comment);
    bool isTemporary(const string& operand);
    bool isConstant(const string& operand);
    bool isRegister(const string& operand);
    bool isGlobalVariable(const string& varName);
    string sanitizeLabel(const string& label);
    string getAssemblyLabel(const string& tacLabel);
    int calculateFunctionStackSize(const string& funcName);
    
    // Data section methods
    void generateDataSection();
    string addStringLiteral(const string& str);

public:
    AssemblyGenerator(vector<TACInstruction>& instrs, SymbolTable* symTab, ofstream& out);
    void generate();
    void writeToFile(const string& filename);
};

#endif // ASSEMBLY_GENERATOR_H
