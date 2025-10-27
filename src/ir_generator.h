#ifndef IR_GENERATOR_H
#define IR_GENERATOR_H

#include "ast.h"
#include "symbol_table.h"
#include <vector>
#include <string>
#include <iostream>
#include<map>
#include <set>
#include <algorithm>  // Add this header
// Undefine parser token macros that may collide with TACOp enumerators
#ifdef EQ
#undef EQ
#endif
#ifdef NE
#undef NE
#endif
#ifdef LT
#undef LT
#endif
#ifdef LE
#undef LE
#endif
#ifdef GT
#undef GT
#endif
#ifdef GE
#undef GE
#endif
#ifdef AND
#undef AND
#endif
#ifdef OR
#undef OR
#endif
#ifdef NOT
#undef NOT
#endif
#ifdef ASSIGN
#undef ASSIGN
#endif
#ifdef GOTO
#undef GOTO
#endif
#ifdef RETURN
#undef RETURN
#endif
#ifdef CONST
#undef CONST
#endif
struct CaseInfo {
    std::string value;      // Case constant value
    std::string label;      // Label for this case
    bool isDefault;    // Is this the default case?
     Node* node;        // Label number for reference
     
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
    
    // Array operations
    ARRAY_LOAD,   // t1 = arr[i]
    ARRAY_STORE,  // arr[i] = t1
    
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
    CONST   ,      // t1 = CONST 5
    
    MEMBER_ACCESS,      // struct.member
    MEMBER_STORE,       // struct.member = value
    PTR_MEMBER_ACCESS,  // struct->member
    PTR_MEMBER_STORE,    // struct->member = value
    GET_PARAM,
    FUNC_BEGIN,  // func_begin factorial
    FUNC_END,    // func_end factorial
    
    CAST,
    COMMENT,     // # comment text
    NOP,         // no-op
};

// TAC Instruction
struct TACInstruction {
    TACOp opcode;
    std::string result;
    std::string operand1;
    std::string operand2;
    int paramCount;
    TACInstruction(TACOp op, const std::string& res = "", 
                   const std::string& op1 = "", const std::string& op2 = "",int pCount = 0)
        : opcode(op), result(res), operand1(op1), operand2(op2) , paramCount(pCount) {}
    
    void print(std::ostream& out = std::cout) const;
    std::string toString() const;
 
};

class IRGenerator {
private:
    std::vector<TACInstruction> instructions;
    SymbolTable& symbolTable;
     int currentCallDepth;  // Track recursion depth during generation
    std::map<std::string, int> functionCallCounts;  // Count calls to each function
    
    // Temporary and label counters
    int tempCounter;
    int labelCounter;
    
    // Current function context
    std::string currentFunction;
    std::string currentFunctionReturnType;  // Track return type of current function
    std::string currentBreakLabel;
std::string currentContinueLabel;
     std::map<std::string, int> enumConstants; 
         std::set<std::string> staticVariables;
    
    // Varargs support: track current parameter index for each va_list variable
    std::map<std::string, int> vaListIndices;
    // Unique naming for symbols to respect scope shadowing
    std::map<Symbol*, std::string> symbolUniqueNames;
    int symbolNameCounter = 0;
public:
    IRGenerator(SymbolTable& symTab);
    
    // Main generation function
    bool generateIR(Node* ast);
    
    // Output
    void printIR(std::ostream& out = std::cout) const;
    void writeToFile(const std::string& filename) const;
    
private:
    // String literal pooling: map label -> literal contents (raw)
    int stringLiteralCounter = 0;
    std::map<std::string, std::string> stringLiterals;
        // Resolve a stable, unique IR name for an identifier node using its attached Symbol
        std::string getUniqueNameFor(Node* idNode, const std::string& fallback);

  void generateParameterHandling(Node* paramList);
    std::string findParameterName(Node* paramDecl);
    
    // Logical operators with short-circuit
    std::string generateLogicalAnd(Node* node);
    std::string generateLogicalOr(Node* node);
    
    // AST traversal helpers
    void traverseAST(Node* node);
    bool hasStructBody(Node* node);
    bool hasEnumBody(Node* node);
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
    std::string generateArrayAccess(Node* node);
    
    // Control flow generators
    void generateIfStatement(Node* node);
    void generateWhileStatement(Node* node);
    void generateReturnStatement(Node* node);
    
    // Utility
    TACOp getBinaryOp(const std::string& nodeName);
    TACOp getUnaryOp(const std::string& nodeName);
    TACOp getComparisonOp(const std::string& nodeName);
    
    // Helper for extracting names from declarators
    std::string getDeclaratorName(Node* node);
    
    void generateForStatement(Node* node);
void generateDoWhileStatement(Node* node);

std::string generatePreIncrement(Node* node);
std::string generatePreDecrement(Node* node);
std::string generatePostIncrement(Node* node);
std::string generatePostDecrement(Node* node);
std::string generateCompoundAssignment(Node* node);

void generateSwitchStatement(Node* node);
void generateBreakStatement();
void generateContinueStatement();

std::string generateTernaryOperator(Node* node);
void generateStatementList(Node* node);
std::string generateArrayStore(Node* arrayNode, Node* valueNode);

std::string generateMemberAccess(Node* node, bool isPointer);
std::string generateMemberStore(Node* memberNode, Node* valueNode, bool isPointer);
void collectEnumConstants(Node* node);

void generateGotoStatement(Node* node);
void generateLabeledStatement(Node* node);

void generateUntilStatement(Node* node);

std::string generateSizeof(Node* node);
int getSizeofType(Node* typeNode);
int getSizeofExpression(Node* exprNode);
int getBaseTypeSize(const std::string& typeName);


// Add these in the private section of IRGenerator class
int getMemberOffset(Node* structNode, const std::string& memberName);

int getStructSize(Symbol* structSym);
int getMemberAlignment(const std::string& typeName);
    // Resolve the struct/union type string of an lvalue expression (IDENTIFIER, member access, ptr member, array element)
    std::string resolveStructTypeFromLValue(Node* node);

// Add to private section of IRGenerator class
std::string generateMultiDimArrayAddress(Node* arrayNode);
int getInnerElementSize(Node* arrayNode);
std::vector<int> getArrayDimensions(Symbol* arraySym);


int getDimensionIndex(Node* arrayAccessNode);
int getSubArraySizeForDimension(Node* arrayNode, int dimIndex);


// Add these after the existing helper function declarations
bool isPointerType(Node* node);
int getPointerDepth(Node* node);
int getPointedToSize(Node* ptrNode);
std::string generatePointerArithmetic(Node* ptrNode, const std::string& ptrTemp,const std::string& offsetTemp, TACOp op);
std::string generatePointerDifference(Node* ptr1Node, Node* ptr2Node,const std::string& ptr1Temp, const std::string& ptr2Temp);

std::string generateArrayAddress(Node* arrayNode);

void generatePrintfStatement(Node* node);
void generateScanfStatement(Node* node);

bool expressionReturnsAddress(Node* node);

    // Helpers to navigate ARRAY AST shapes
    Node* findIdentifierInArray(Node* arrayNode);

    // Helpers for general lvalue handling and ++/-- step calculation
    std::string generateLValueAddress(Node* lvalue);
    int getIncrementStepForLValue(Node* lvalue);
    
       bool isConstantValue(const std::string& val);
    std::string getConstantValue(const std::string& tempName);
    std::string normalizeTypeName(const std::string& typeName);
    std::string convertConstantValue(const std::string& value, const std::string& fromType, const std::string& toType);
    std::string handleImplicitConversion(const std::string& valueTemp, Node* valueNode, Node* targetVarNode);
std:: string getExpressionResultType(Node* exprNode);
std:: string extractTypeFromSpecQualList(Node* typeNode);
    std::string generateConstantCast(const std::string& valTemp, const std::string& targetType);
   
   std::string applyIntegerPromotion(const std::string& valueTemp, Node* valueNode);
};

#endif // IR_GENERATOR_H
