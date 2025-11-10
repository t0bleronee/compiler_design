#ifndef CODE_GENERATOR_H
#define CODE_GENERATOR_H

#include "ir_generator.h"
#include "symbol_table.h"
#include <vector>
#include <string>
#include <map>
#include <set>
#include <deque>

class CodeGenerator {
public:
    CodeGenerator(const std::vector<TACInstruction>& instructions, SymbolTable* symTab);
    
    void generate();
    void writeToFile(const std::string& filename);
    void printAssembly(std::ostream& out = std::cout) const;

private:
    const std::vector<TACInstruction>& tacInstructions;
    SymbolTable* symbolTable;
    std::vector<std::string> asmCode;
    std::vector<std::string> dataSection;
    
    // Register allocation
    struct RegisterPool {
        std::set<std::string> tempRegs;      // $t0-$t9
        std::set<std::string> savedRegs;     // $s0-$s7
        std::map<std::string, std::string> varToReg;   // variable/temp -> register
        std::map<std::string, std::string> regToVar;   // register -> variable/temp
        
        void init();
        std::string allocateTemp(const std::string& var);
        std::string allocateSaved(const std::string& var);
        void freeReg(const std::string& reg);
        void spillAll();
        bool isAllocated(const std::string& var);
        std::string getReg(const std::string& var);
    } registers;
    
    // Memory layout
    int frameSize;
    int currentOffset;
    std::map<std::string, int> varOffsets;        // variable -> stack offset from $fp
    std::map<std::string, std::string> staticVars; // static var -> label
    std::map<std::string, int> globalVars;        // global var -> address
    
    // String literals
    int stringCounter;
    std::map<std::string, std::string> stringLabels;  // content -> label
    
    // Function context
    std::string currentFunction;
    int functionParamCount;
    int currentFunctionFrameSize;
    std::deque<std::string> paramQueue;  // For function call parameters
    bool inFunction;
    std::map<std::string, int> functionFrameSizes;  // function name -> frame size
    
    // Label generation
    int labelCounter;
    
    // Code generation methods
    void generateDataSection();
    void generateTextSection();
    void generateInstruction(const TACInstruction& instr);
    
    // Operand handling
    void loadOperand(const std::string& operand, const std::string& targetReg);
    void storeToVar(const std::string& var, const std::string& sourceReg);
    bool isImmediate(const std::string& operand);
    bool isTemp(const std::string& operand);
    bool isVariable(const std::string& operand);
    int getImmediate(const std::string& operand);
    
    // Arithmetic operations
    void generateAdd(const TACInstruction& instr);
    void generateSub(const TACInstruction& instr);
    void generateMul(const TACInstruction& instr);
    void generateDiv(const TACInstruction& instr);
    void generateMod(const TACInstruction& instr);
    void generateNeg(const TACInstruction& instr);
    
    // Logical operations (short-circuit)
    void generateLogicalAnd(const TACInstruction& instr);
    void generateLogicalOr(const TACInstruction& instr);
    void generateLogicalNot(const TACInstruction& instr);
    
    // Bitwise operations
    void generateBitAnd(const TACInstruction& instr);
    void generateBitOr(const TACInstruction& instr);
    void generateBitXor(const TACInstruction& instr);
    void generateBitNot(const TACInstruction& instr);
    void generateShl(const TACInstruction& instr);
    void generateShr(const TACInstruction& instr);
    
    // Comparison operations
    void generateEq(const TACInstruction& instr);
    void generateNe(const TACInstruction& instr);
    void generateLt(const TACInstruction& instr);
    void generateLe(const TACInstruction& instr);
    void generateGt(const TACInstruction& instr);
    void generateGe(const TACInstruction& instr);
    
    // Memory operations
    void generateAssign(const TACInstruction& instr);
    void generateLoad(const TACInstruction& instr);
    void generateStore(const TACInstruction& instr);
    void generateAddress(const TACInstruction& instr);
    void generateArrayLoad(const TACInstruction& instr);
    void generateArrayStore(const TACInstruction& instr);
    
    // Control flow
    void generateLabel(const TACInstruction& instr);
    void generateGoto(const TACInstruction& instr);
    void generateIfGoto(const TACInstruction& instr);
    void generateIfFalseGoto(const TACInstruction& instr);
    
    // Function operations
    void generateFuncBegin(const TACInstruction& instr);
    void generateFuncEnd(const TACInstruction& instr);
    void generateParam(const TACInstruction& instr);
    void generateCall(const TACInstruction& instr);
    void generateReturn(const TACInstruction& instr);
    
    // Type conversion
    void generateCast(const TACInstruction& instr);
    
    // Helper utilities
    void emit(const std::string& instruction);
    void emitLabel(const std::string& label);
    void emitComment(const std::string& comment);
    void emitBlankLine();
    std::string newLabel(const std::string& prefix);
    std::string getStringLabel(const std::string& content);
    int getVarOffset(const std::string& varName);
    void saveRegisters();
    void restoreRegisters();
    int calculateFrameSize(size_t funcStartIdx, size_t funcEndIdx);
};

#endif // CODE_GENERATOR_H
