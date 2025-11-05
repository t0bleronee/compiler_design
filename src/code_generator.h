#ifndef CODE_GENERATOR_H
#define CODE_GENERATOR_H

#include <string>
#include <vector>
#include <map>
#include <set>
#include "ir_generator.h"
#include "symbol_table.h"

using namespace std;

class CodeGenerator {
private:
    vector<TACInstruction> instructions;
    SymbolTable* symTab;
    
    // Register management
    vector<string> tempRegisters;     // $t0-$t9
    vector<string> savedRegisters;    // $s0-$s7
    map<string, string> varToReg;     // Variable/temp to register mapping
    map<string, int> varToStackOffset; // Variable to stack offset
    set<string> usedSavedRegs;        // Track which $s registers are used
    
    // Memory management
    int currentStackOffset;
    map<string, int> functionStackSize;
    map<string, vector<string>> functionLocalVars;
    
    // String literals
    map<string, string> stringLabels;  // String constant to label mapping
    int stringLabelCounter;
    
    // Label management
    string currentFunction;
    
    // Output
    vector<string> dataSection;
    vector<string> textSection;
    
    // Helper methods
    void initializeRegisters();
    string allocateRegister(const string& var);
    void freeRegister(const string& var);
    string getRegister(const string& var);
    bool isTemporary(const string& var);
    bool isConstant(const string& var);
    bool isStringLiteral(const string& var);
    
    // Stack management
    int allocateStackSpace(const string& var, int size = 4);
    int getStackOffset(const string& var);
    int getArraySize(const string& varName);
    void computeFunctionStackFrames();
    
    // Code generation for specific operations
    void generateDataSection();
    void generateTextSection();
    void generateInstruction(const TACInstruction& instr);
    
    // Instruction generators
    void genArithmetic(const TACInstruction& instr);
    void genComparison(const TACInstruction& instr);
    void genLogical(const TACInstruction& instr);
    void genBitwise(const TACInstruction& instr);
    void genAssign(const TACInstruction& instr);
    void genLoad(const TACInstruction& instr);
    void genStore(const TACInstruction& instr);
    void genAddress(const TACInstruction& instr);
    void genArrayLoad(const TACInstruction& instr);
    void genArrayStore(const TACInstruction& instr);
    void genGoto(const TACInstruction& instr);
    void genIfGoto(const TACInstruction& instr);
    void genIfFalseGoto(const TACInstruction& instr);
    void genLabel(const TACInstruction& instr);
    void genCall(const TACInstruction& instr);
    void genReturn(const TACInstruction& instr);
    void genParam(const TACInstruction& instr);
    void genGetParam(const TACInstruction& instr);
    void genFunctionBegin(const TACInstruction& instr);
    void genFunctionEnd(const TACInstruction& instr);
    void genCast(const TACInstruction& instr);
    
    // Utility methods
    void emit(const string& instruction);
    void emitData(const string& directive);
    void emitComment(const string& comment);
    void loadOperand(const string& operand, const string& reg);
    void storeResult(const string& result, const string& reg);
    string getSizeForType(const string& type);
    string getLoadInstructionForType(const string& type);
    string getStoreInstructionForType(const string& type);
    
public:
    CodeGenerator(const vector<TACInstruction>& tacInstructions, SymbolTable* symbolTable);
    void generate();
    void writeToFile(const string& filename);
    string getAssemblyCode();
};

#endif // CODE_GENERATOR_H
