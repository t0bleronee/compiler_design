#ifndef IR_GENERATOR_H
#define IR_GENERATOR_H

#include "ast.h"
#include "symbol_table.h"
#include <vector>
#include <string>
#include <iostream>

struct CaseInfo {
    std::string value;      // Case constant value
    std::string label;      // Label for this case
    bool isDefault;         // Is this the default case?
    Node* node;             // AST node for this case
};

// TAC Operation Codes
enum class TACOp {
    // Arithmetic
    ADD, SUB, MUL, DIV, MOD,
    NEG, // Unary minus
    
    // Comparisons  
    EQ, NE, LT, LE, GT, GE,
    
    // Logical
    AND, OR, NOT,
    
    // Bitwise
    BIT_AND, BIT_OR, BIT_XOR, SHL, SHR, BIT_NOT,
  
    // Memory operations
    ASSIGN,       // t1 = x
    LOAD,         // t1 = *p  
    STORE,        // *p = t1
    ADDRESS,      // t1 = &x
    
    // Control flow
    GOTO,         // goto L1
    IF_GOTO,      // if t1 goto L1
    IF_FALSE_GOTO, // if_false t1 goto L1
    LABEL,        // L1:
    
    // Function operations
    CALL,         // t1 = call func
    RETURN,       // return t1
    PARAM,        // param t1
    
    // Special
    CONST         // t1 = CONST 5
};

// TAC Instruction
struct TACInstruction {
    TACOp opcode;
    std::string result;
    std::string operand1;
    std::string operand2;
    
    TACInstruction(TACOp op, const std::string& res = "", 
                   const std::string& op1 = "", const std::string& op2 = "")
        : opcode(op), result(res), operand1(op1), operand2(op2) {}
    
    void print(std::ostream& out = std::cout) const;
    std::string toString() const;
};

class IRGenerator {
private:
    std::vector<TACInstruction> instructions;
    SymbolTable& symbolTable;
    
    // Temporary and label counters
    int tempCounter;
    int labelCounter;
    
    // Current function context
    std::string currentFunction;
    std::string currentBreakLabel;
    std::string currentContinueLabel;

public:
    IRGenerator(SymbolTable& symTab);
    
    // Main generation function
    bool generateIR(Node* ast);
    
    // Output
    void printIR(std::ostream& out = std::cout) const;
    void writeToFile(const std::string& filename) const;
    
private:
    void generateParameterHandling(Node* paramList);
    std::string findParameterName(Node* paramDecl);
    
    // Logical operators with short-circuit
    std::string generateLogicalAnd(Node* node);
    std::string generateLogicalOr(Node* node);
    
    // AST traversal helpers
    void traverseAST(Node* node);
    bool hasStructBody(Node* node);
    std::string findIdentifier(Node* node);

    // Helper functions
    std::string createTemp();
    std::string createLabel(const std::string& prefix = "L");

    // Generation methods for different AST nodes
    std::string generateExpression(Node* node);
    void generateStatement(Node* node);
    void generateDeclaration(Node* node);
    void generateFunction(Node* node);
    
    // Specific expression generators
    std::string generateBinaryExpr(Node* node, TACOp op);
    std::string generateUnaryExpr(Node* node, TACOp op);
    std::string generateAssignment(Node* node);
    std::string generateFunctionCall(Node* node);
    
    // Control flow generators
    void generateIfStatement(Node* node);
    void generateWhileStatement(Node* node);
    void generateReturnStatement(Node* node);
    
    // New in Version 2: Advanced control flow
    void generateForStatement(Node* node);
    void generateDoWhileStatement(Node* node);
    void generateSwitchStatement(Node* node);
    void generateBreakStatement(Node* node);
    void generateContinueStatement(Node* node);
    
    // New in Version 2: Advanced expressions
    std::string generatePreIncrement(Node* node);
    std::string generatePreDecrement(Node* node);
    std::string generatePostIncrement(Node* node);
    std::string generatePostDecrement(Node* node);
    std::string generateCompoundAssignment(Node* node);
    std::string generateTernaryOperator(Node* node);
    
    // Utility
    TACOp getBinaryOp(const std::string& nodeName);
    TACOp getUnaryOp(const std::string& nodeName);
    
    // Helper for extracting names from declarators
    std::string getDeclaratorName(Node* node);
    
    // New in Version 2: Statement list handling
    void generateStatementList(Node* node);
};

#endif // IR_GENERATOR_H
