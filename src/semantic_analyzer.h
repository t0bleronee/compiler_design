#ifndef SEMANTIC_ANALYZER_H
#define SEMANTIC_ANALYZER_H

#include "ast.h"
#include "symbol_table.h"
#include <vector>
#include <string>
#include<algorithm>
#include <map>
#include <unordered_map>


class SemanticAnalyzer {
private:
    SymbolTable& symbolTable;           // Reference to the symbol table
    std::vector<std::string> errors;    // Collected semantic errors
    std::string currentFunctionReturnType;  
    std::string currentFunctionName; 
    int loopDepth;       
    int switchDepth;      
    std::map<std::string, Node*> currentFunctionLabels;  
    std::vector<std::pair<std::string, Node*>> pendingGotoStatements; 
    std::unordered_map<std::string, long long> enumConstants; 


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
    std::vector<std::string> extractFunctionParameters(Node* funcDeclNode, std::vector<bool>* outParamIsRef = nullptr);
     void addParametersToScope(Node* funcDeclNode);
     
      void checkIdentifier(Node* node);
    void checkFunctionCall(Node* node);
    // Strict function-argument compatibility (disallows narrowing like float->int, double->float)
    bool areFunctionArgCompatible(const std::string& argType, const std::string& paramType);
    bool isDeclarationContext(Node* node);
    void checkReturnStatement(Node* node);
    void checkReturnLocalAddress(Node* expr);
void analyzeDeclarator(Node* node, std::string& name, int& pointerDepth, bool& isArray, std::vector<int>& arrayDims, bool allowIncomplete = false);
std::vector<int> extractArrayDimensions(Node* arrayNode, std::string& varName, bool allowIncomplete = false);
int evaluateConstantExpression(Node* node);
bool tryEvaluateConstantExpression(Node* node, long long& value);
void checkArrayAccess(Node* node);
void checkUnaryOperation(Node* node);
std::string getExpressionType(Node* node);
    // Extract a canonical type string from a type_name or SPEC_QUAL_LIST node
    std::string extractTypeFromTypeName(Node* typeNode);
void processStruct(Node* node);
void processEnum(Node* node);
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
bool isArrayType(const std::string& type);
std::string getArrayElementType(const std::string& arrayType);
void validateArrayInitialization(Symbol* arraySym, Node* initList);
int countInitializerElements(Node* node);
void checkArrayInitializerSize(Node* initList, const std::vector<int>& dimensions, const std::string& arrayName, size_t dimIndex);
void checkStructInitializerSize(Node* initList, const std::string& structType, const std::string& varName);
std::string getResultType(const std::string& type1, const std::string& type2);
bool areTypesCompatible(const std::string& type1, const std::string& type2, const std::string& operation);
void checkAssignment(Node* node);

int countPointerLevel(const std::string& type);
void checkPointerAssignment(/*Node* node,*/ const std::string& lhsType, const std::string& rhsType);
// In the private section of SemanticAnalyzer class:
void checkPostfixOperation(Node* node);
void checkPrefixOperation(Node* node);
void checkSizeof(Node* node);
bool isLvalue(Node* node);

void checkMemberAccess(Node* node);
bool allPathsReturn(Node* node);

void processTypedef(Node* node);
std::string resolveTypedef(const std::string& type);  

// In private section, add:
bool isConstantIndex(Node* indexNode, int& value);

std::string getBaseTypeFromPointer(const std::string& type);
std::string getMemberType(const std::string& structTypeName, const std::string& memberName);
void checkConditionExpression(Node* node);

void processLabel(Node* node);
void processGoto(Node* node);
void verifyGotoLabels();

void checkSwitchStatement(Node* node);
void checkCaseLabels(Node* switchNode);
bool isConstantIntegerExpression(Node* node);

void checkStaticKeyword(Node* declSpecNode, bool& hasStatic, bool& hasTypeSpec);
void checkTypeSpecifier(Node* typeSpecNode);

bool isFunctionPrototype(Node* declNode);
bool hasFunctionDeclarator(Node* node);
void processFunctionPrototype(Node* declNode);
Node* findFunctionDeclarator(Node* node);
    // Helpers for detecting function pointer declarators
    Node* findFunctionDeclaratorInNode(Node* node);
    bool isFunctionPointerDeclarator(Node* initDeclNode);

Node* findIdentifierInArray(Node* arrayNode);

Node* findIdentifierInDeclarator(Node* declaratorNode);
void extractStructMembers(Node* declNode, std::map<std::string, std::string>& members, std::map<std::string, Node*>& memberNodes, std::vector<std::string>& memberOrder);


// Add these method declarations in the private section:
void checkPrintfStatement(Node* node);
void checkScanfStatement(Node* node);
bool validateFormatString(const std::string& format, const std::vector<std::string>& argTypes, bool isScanf);
int countFormatSpecifiers(const std::string& format);
bool isStringLiteral(Node* node);

bool isEnumDefinition(Node* enumSpecNode);
 void checkStaticRedeclaration(const std::string& name, bool isStatic);
  
};


#endif
