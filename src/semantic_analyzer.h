#ifndef SEMANTIC_ANALYZER_H
#define SEMANTIC_ANALYZER_H

#include "ast.h"
#include "symbol_table.h"
#include <vector>
#include <string>
#include<algorithm>

class SemanticAnalyzer {
private:
    SymbolTable& symbolTable;           // Reference to the symbol table
    std::vector<std::string> errors;    // Collected semantic errors
    std::string currentFunctionReturnType;  
    std::string currentFunctionName; 
    int loopDepth;       
    int switchDepth;      

public:
    SemanticAnalyzer(SymbolTable& symTab);

    // Main entry point: traverses AST and builds symbol table
    bool buildSymbolTable(Node* root);

    // Error reporting
    void addError(const std::string& error);
    void printErrors() const;
    bool hasErrors() const { return !errors.empty(); }

    // Debug
    void exploreAST(Node* node, int depth = 0) const;

private:
    // Core processing
    void processFunction(Node* node);       // Handles FUNCTION_DEFINITION
    void processVariable(Node* node);       // Handles DECLARATION nodes
    void processBlock(Node* node, bool isFunctionBody = false, Node* funcDeclNode = nullptr);
    void traverseAST(Node* node);           // Recursively traverse AST
    std::string extractTypeFromDeclSpecifiers(Node* declSpecifiersNode);
    std::vector<std::string> extractFunctionParameters(Node* funcDeclNode);
     void addParametersToScope(Node* funcDeclNode);
     
      void checkIdentifier(Node* node);
    void checkFunctionCall(Node* node);
    bool isDeclarationContext(Node* node);
    void checkReturnStatement(Node* node);
    void checkStaticKeyword(Node* declSpecNode, bool& hasStatic, bool& hasTypeSpec);
    void checkTypeSpecifier(Node* typeSpecNode);
void analyzeDeclarator(Node* node, std::string& name, int& pointerDepth, bool& isArray, std::vector<int>& arrayDims);
std::vector<int> extractArrayDimensions(Node* arrayNode, std::string& varName);
int evaluateConstantExpression(Node* node);
void checkArrayAccess(Node* node);
void checkUnaryOperation(Node* node);
std::string getExpressionType(Node* node);
void processStruct(Node* node);
void processEnum(Node* node);
void extractStructMembers(Node* declNode, std::map<std::string, std::string>& members);
bool isStructMemberDeclaration(Node* node);



// Type checking methods
void checkBinaryOperation(Node* node);
void checkArithmeticOperation(Node* node);
void checkComparisonOperation(Node* node);
void checkLogicalOperation(Node* node);
void checkBitwiseOperation(Node* node);
bool isNumericType(const std::string& type);
bool isIntegerType(const std::string& type);
bool isPointerType(const std::string& type);
std::string getResultType(const std::string& type1, const std::string& type2);
bool areTypesCompatible(const std::string& type1, const std::string& type2, const std::string& operation);
void checkAssignment(Node* node);

int countPointerLevel(const std::string& type);
void checkPointerAssignment(/*Node* node,*/ const std::string& lhsType, const std::string& rhsType);
// In the private section of SemanticAnalyzer class:
void checkPostfixOperation(Node* node);
void checkPrefixOperation(Node* node);
bool isLvalue(Node* node);

void checkMemberAccess(Node* node);
bool allPathsReturn(Node* node);

void processTypedef(Node* node);
std::string resolveTypedef(const std::string& type);  
};

#endif

