#include "semantic_analyzer.h"
#include <iostream>

using namespace std;

// Constructor
SemanticAnalyzer::SemanticAnalyzer(SymbolTable& symTab) 
    : symbolTable(symTab), currentFunctionReturnType(""), currentFunctionName("") , loopDepth(0), switchDepth(0) {}
// Main entry
bool SemanticAnalyzer::buildSymbolTable(Node* root) {
    if (!root) return false;

      cout << "DEBUG: Before enterScope, active scopes = " << symbolTable.getCurrentScopeLevel() << "\n";
    //symbolTable.enterScope();  // Global scope for functions
    

    cout << "\n=== Building Symbol Table ===\n";
    traverseAST(root);
   
symbolTable.exitScope();
    cout << "\n=== Semantic Analysis Completed ===\n";
    Symbol* modeSym = symbolTable.lookup("Mode");
    if (modeSym && modeSym->isEnum) {
        cout << "VERIFICATION: Mode enum has " << modeSym->enumValues.size() << " values in memory\n";
    }
    symbolTable.printAllScopes();
    printErrors();

    return !hasErrors();
}

void SemanticAnalyzer::addError(const string& error) {
    errors.push_back(error);
    cout << "ERROR: " << error << endl;
}

// Print all errors
void SemanticAnalyzer::printErrors() const {
    if (errors.empty()) {
        cout << "No semantic errors found.\n";
        return;
    }
    cout << "\n=== Semantic Errors ===\n";
    for (const auto& e : errors) {
        cout << "  ❌ " << e << endl;
    }
}

void SemanticAnalyzer::traverseAST(Node* node) {
    if (!node) return;
    
    // Handle function definitions
    if (node->name == "FUNCTION_DEFINITION") {
        bool hasNestedFunction = false;
        for (auto child : node->children) {
            if (child->name == "FUNCTION_DEFINITION") {
                hasNestedFunction = true;
                break;
            }
        }
        
        if (!hasNestedFunction) {
            processFunction(node);
            return;
        }
    } 
    // Handle blocks
    else if (node->name == "COMPOUND_STMT") {
        processBlock(node);
        return;
    } 
  else if (node->name == "DECLARATION") {
  
     // NEW: Check if this is a typedef declaration
    bool isTypedefDecl = false;
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
            for (auto specChild : child->children) {
                if (specChild->name == "STORAGE_CLASS_SPECIFIER" && specChild->lexeme == "typedef") {
                    isTypedefDecl = true;
                    break;
                }
            }
        }
    }
    
    if (isTypedefDecl) {
        processTypedef(node);
        return;
    }

    // Check if this is a struct/enum definition
    bool isStructOrEnumDef = false;
      bool isStructMember = isStructMemberDeclaration(node);
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
            for (auto declChild : child->children) {
                if (declChild->name == "STRUCT_OR_UNION_SPECIFIER") {
                    // Check if it has body (definition vs just usage)
                    for (auto structChild : declChild->children) {
                        if (structChild->name == "DECLARATION") {
                            processStruct(declChild);
                            isStructOrEnumDef = true;
                            break;
                        }
                    }
                }
                else if (declChild->name == "ENUM_SPECIFIER") {
                    // Process enum definition
                    processEnum(declChild);
                    isStructOrEnumDef = true;
                    break;  // Add this break
                }
            }
        }
    }
    
    // Only process as variable if it's not a struct/enum definition
   if (!isStructOrEnumDef && !isStructMember) {
            processVariable(node);
        }
}
 
    // Check identifier usage (not declarations)
    else if (node->name == "IDENTIFIER" && !isDeclarationContext(node)) {
        checkIdentifier(node);
    }
    // Check function calls
    else if (node->name == "FUNC_CALL") {
        checkFunctionCall(node);
    }
// Check return statements
else if (node->name == "RETURN_STMT") {
    checkReturnStatement(node);
}
// Track loop depth
else if (node->name == "WHILE_STMT" || node->name == "DO_WHILE_STMT" || 
         node->name == "FOR_STMT_2" || node->name == "FOR_STMT_3" || 
         node->name == "FOR_RANGE_STMT" || node->name == "UNTIL_STMT") {
    loopDepth++;
    for (auto child : node->children) {
        traverseAST(child);
    }
    loopDepth--;
    return;  
}
// Track switch depth
else if (node->name == "SWITCH_STMT") {
    switchDepth++;
    for (auto child : node->children) {
        traverseAST(child);
    }
    switchDepth--;
    return;
}
// Check break statement
else if (node->name == "BREAK_STMT") {
    if (loopDepth == 0 && switchDepth == 0) {
        addError("'break' statement not within loop or switch");
    }
}
// Check continue statement
else if (node->name == "CONTINUE_STMT") {
    if (loopDepth == 0) {
        addError("'continue' statement not within loop");
    }
}
else if (node->name == "ARRAY_ACCESS") {
    checkArrayAccess(node);
}
else if (node->name == "UNARY_OP") {
    checkUnaryOperation(node);
}
// Add these AFTER the existing UNARY_OP check and BEFORE the final for loop

// Check arithmetic operations
else if (node->name == "ADD_EXPR" || node->name == "SUB_EXPR" || 
         node->name == "MUL_EXPR" || node->name == "DIV_EXPR" || 
         node->name == "MOD_EXPR") {
    checkArithmeticOperation(node);
}
// Check comparison operations
else if (node->name == "LT_EXPR" || node->name == "GT_EXPR" || 
         node->name == "LE_EXPR" || node->name == "GE_EXPR" || 
         node->name == "EQ_EXPR" || node->name == "NEQ_EXPR") {
    checkComparisonOperation(node);
}
// Check logical operations
else if (node->name == "LOGICAL_OR" || node->name == "LOGICAL_AND") {
    checkLogicalOperation(node);
}
// Check bitwise operations
else if (node->name == "BIT_OR" || node->name == "BIT_AND" || 
         node->name == "BIT_XOR" || node->name == "LSHIFT_EXPR" || 
         node->name == "RSHIFT_EXPR") {
    checkBitwiseOperation(node);
}
else if (node->name == "ASSIGN_EXPR") {
    checkAssignment(node);
}
    else if (node->name == "POST_INC" || node->name == "POST_DEC") {
        checkPostfixOperation(node);
    }
    else if (node->name == "PRE_INC" || node->name == "PRE_DEC") {
        checkPrefixOperation(node);
    }

// Check member access (. and ->)
else if (node->name == "MEMBER_ACCESS" || node->name == "PTR_MEMBER_ACCESS") {
    checkMemberAccess(node);
}
    // Continue traversal for other children
    for (auto child : node->children) {
        traverseAST(child);
    }
}
void SemanticAnalyzer::processFunction(Node* node) {
    if (!node) return;

    string funcName;
    string returnType;
    vector<string> paramTypes;
    Node* funcDeclNode = nullptr;
    bool hasStatic = false;
    bool hasTypeSpec = false;

    // Extract function info from FUNCTION_DEFINITION children
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
            // FIRST: Check for static keyword and type specifier presence
            checkStaticKeyword(child, hasStatic, hasTypeSpec);
            
            // THEN: Extract the actual return type
            returnType = extractTypeFromDeclSpecifiers(child);
            
            // Check type specifiers (but don't call checkTypeSpecifier on every child)
            for (auto specChild : child->children) {
                if (specChild->name == "TYPE_SPECIFIER") {
                    checkTypeSpecifier(specChild);
                }
            }
        }
        else if (child->name == "FUNCTION_DECL") {
            funcDeclNode = child;
            // Get function name from first child (IDENTIFIER)
            if (!child->children.empty() && child->children[0]->name == "IDENTIFIER") {
                funcName = child->children[0]->lexeme;
            }
        }
    }
currentFunctionName = funcName;

    if (funcName.empty()) {
        addError("Function name missing");
        return;
    }

    // VALIDATE: Static function must have type specifier
    if (hasStatic && !hasTypeSpec) {
        addError("Function '" + funcName + "' with 'static' storage class requires explicit return type specifier");
        returnType = "int";  // Default for error recovery
    }

    // If no return type specified at all (and not static error case)
    if (returnType.empty()) {
        if (!hasStatic) {
            // Non-static function without type - assume int with warning
            addError("Function '" + funcName + "' declared without return type specifier");
            returnType = "int";
        } else {
            // This case already handled above, but for safety
            returnType = "int";
        }
    }

    currentFunctionReturnType = returnType;

    // Extract parameter types
    paramTypes = extractFunctionParameters(funcDeclNode);

    // Add function to symbol table with parameters
    if (!symbolTable.addSymbol(funcName, returnType, node, true, paramTypes)) {
        addError("Redeclaration of function: " + funcName);
    }

   
for (auto child : node->children) {
    if (child->name == "COMPOUND_STMT") {
        processBlock(child, true, funcDeclNode);
        
        // CHECK: Non-void functions must return on all paths
        if (returnType != "void") {
            if (!allPathsReturn(child)) {
                addError("Function '" + funcName + "' with non-void return type '" + 
                         returnType + "' does not return a value on all control paths");
            }
        }
        
        currentFunctionName = "";
        currentFunctionReturnType = "";
        return;
    }
}
currentFunctionName = "";
currentFunctionReturnType = "";
    
}


string SemanticAnalyzer::extractTypeFromDeclSpecifiers(Node* declSpecifiersNode) {
    if (!declSpecifiersNode) return "";
    
    for (auto child : declSpecifiersNode->children) {
    
      if (child->name == "STORAGE_CLASS_SPECIFIER") {
            continue;  // NEW: Skip these
        }
        
        if (child->name == "TYPE_SPECIFIER") {
            string type = child->lexeme;
            // NEW: Resolve if it's a typedef
            return resolveTypedef(type);
        }
    
    
        // Handle struct/union specifiers
        else if (child->name == "STRUCT_OR_UNION_SPECIFIER") {
            // Look for IDENTIFIER child (struct name)
            for (auto structChild : child->children) {
                if (structChild->name == "IDENTIFIER") {
                    return "struct " + structChild->lexeme;  // Return "struct Point"
                }
            }
            return "struct";  // Anonymous struct
        }
        else if (child->name == "DECL_SPECIFIERS") {
            // Recursively search in nested DECL_SPECIFIERS
            string type = extractTypeFromDeclSpecifiers(child);
            if (!type.empty()) {
                return type;
            }
        }
    }
    
    return "";
}

void SemanticAnalyzer::processVariable(Node* node) {
    if (!node) return;

    string varType;
    bool hasStatic = false;
    bool hasTypeSpec = false;
    bool hasStorageError = false;
    
    // Extract type
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
            // FIRST: Check for static keyword and type specifier presence
            checkStaticKeyword(child, hasStatic, hasTypeSpec);
            if (hasStorageError) {
                return;  // Don't process this declaration further
            }
            
            // THEN: Extract the actual type
            varType = extractTypeFromDeclSpecifiers(child);
            
            // Check type specifiers
            for (auto specChild : child->children) {
                if (specChild->name == "TYPE_SPECIFIER") {
                    checkTypeSpecifier(specChild);
                }
            }
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto declChild : child->children) {
                if (!declChild->children.empty()) {
                    Node* firstChild = declChild->children[0];
                    
                    int pointerDepth = 0;
                    string varName;
                    bool isArray = false;
                    vector<int> arrayDimensions; 
                    
                    // Direct identifier: int x;
                    if (firstChild->name == "IDENTIFIER") {
                        varName = firstChild->lexeme;
                    }
                    // Array: int a[10]; or int matrix[3][4];
                    else if (firstChild->name == "ARRAY") {
                        isArray = true;
                        arrayDimensions = extractArrayDimensions(firstChild, varName);
                    }
                    else if (firstChild->name == "DECLARATOR") {
                        analyzeDeclarator(firstChild, varName, pointerDepth, isArray, arrayDimensions);
                    }
                    
                    if (!varName.empty()) {

                        // VALIDATE: Static must have type specifier
                        if (hasStatic && !hasTypeSpec) {
                            addError("Variable '" + varName + "' with 'static' storage class requires explicit type specifier");
                            continue;  // Skip adding to symbol table
                        }
                        
                        // If varType is still empty and we have static, that's an error
                        if (hasStatic && varType.empty()) {
                            addError("Variable '" + varName + "' with 'static' storage class must have explicit type specifier");
                            continue;  // Skip adding to symbol table
                        }
                        
                        // If no type at all (not even static), default to int with warning
                        if (varType.empty() && !hasStatic) {
                            addError("Variable '" + varName + "' declared without type specifier");
                            varType = "int";  // Assume int for recovery
                        }
                        cout << "DEBUG: Adding variable " << varName << " [" << varType;
                        
                        // Print pointer depth
                        for (int i = 0; i < pointerDepth; i++) {
                            cout << "*";
                        }
                        
                        // Print array dimensions
                        if (isArray) {
                            for (int dim : arrayDimensions) {
                                cout << "[" << (dim == -1 ? "" : to_string(dim)) << "]";
                            }
                        }
                        cout << "]\n";
                        
                        if (!symbolTable.addSymbol(varName, varType, node, false, {}, 
                                                   isArray, arrayDimensions, pointerDepth)) {
                            addError("Redeclaration of variable: " + varName);
                        }
                    }
                }
            }
        }
    }
}

void SemanticAnalyzer::processBlock(Node* node, bool isFunctionBody, Node* funcDeclNode) {
    if (!node) return;

    // Enter new scope
    symbolTable.enterScope();
    
    // If this is a function body, add parameters to this scope
    if (isFunctionBody && funcDeclNode) {
        addParametersToScope(funcDeclNode);
    }

    // Process child nodes recursively
    for (auto child : node->children) {
        traverseAST(child);
    }

    // Exit scope
    symbolTable.exitScope();
}
vector<string> SemanticAnalyzer::extractFunctionParameters(Node* funcDeclNode) {
    vector<string> paramTypes;
    
    if (!funcDeclNode) return paramTypes;
    
    // FUNCTION_DECL → children[1] is PARAM_TYPE_LIST (if params exist)
    for (auto child : funcDeclNode->children) {
        if (child->name == "PARAM_TYPE_LIST") {
            // PARAM_TYPE_LIST → PARAM_LIST
            for (auto paramTypeListChild : child->children) {
                if (paramTypeListChild->name == "PARAM_LIST") {
                    // PARAM_LIST → multiple PARAM_DECL
                    for (auto paramDecl : paramTypeListChild->children) {
                        if (paramDecl->name == "PARAM_DECL") {
                            // PARAM_DECL → declaration_specifiers
                            for (auto paramChild : paramDecl->children) {
                                if (paramChild->name == "DECL_SPECIFIERS" || 
                                    paramChild->name == "declaration_specifiers") {
                                    string type = extractTypeFromDeclSpecifiers(paramChild);
                                    if (!type.empty()) {
                                        paramTypes.push_back(type);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    return paramTypes;
}

void SemanticAnalyzer::addParametersToScope(Node* funcDeclNode) {
    if (!funcDeclNode) return;
    
    for (auto child : funcDeclNode->children) {
        if (child->name == "PARAM_TYPE_LIST") {
            for (auto paramTypeListChild : child->children) {
                if (paramTypeListChild->name == "PARAM_LIST") {
                    for (auto paramDecl : paramTypeListChild->children) {
                        if (paramDecl->name == "PARAM_DECL") {
                            string paramType;
                            string paramName;
                            
                            for (auto paramChild : paramDecl->children) {
                                if (paramChild->name == "DECL_SPECIFIERS" || 
                                    paramChild->name == "declaration_specifiers") {
                                    paramType = extractTypeFromDeclSpecifiers(paramChild);
                                }
                                else if (paramChild->name == "DECLARATOR") {
                                    // DECLARATOR may wrap the IDENTIFIER
                                    for (auto declChild : paramChild->children) {
                                        if (declChild->name == "IDENTIFIER") {
                                            paramName = declChild->lexeme;
                                        }
                                    }
                                }
                                else if (paramChild->name == "IDENTIFIER") {
                                    paramName = paramChild->lexeme;
                                }
                                // Handle reference parameters (&)
                                else if (paramChild->name == "&") {
                                    // Could add ref tracking here if needed
                                }
                            }
                            
                            if (!paramName.empty() && !paramType.empty()) {
                                cout << "DEBUG: Adding parameter " << paramName 
                                     << " [" << paramType << "] to function scope\n";
                                if (!symbolTable.addSymbol(paramName, paramType, paramDecl)) {
                                    addError("Duplicate parameter name: " + paramName);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

void SemanticAnalyzer::checkFunctionCall(Node* node) {
    if (!node || node->children.empty()) return;
    
    Node* funcNode = node->children[0];
    if (funcNode->name != "IDENTIFIER") return;
    
    string funcName = funcNode->lexeme;
    Symbol* funcSym = symbolTable.lookup(funcName);
    
    if (!funcSym) {
        addError("Undeclared function: '" + funcName + "'");
        return;
    }
    
    if (!funcSym->isFunction) {
        addError("'" + funcName + "' is not a function");
        return;
    }
 
    // Extract actual arguments
vector<Node*> argNodes;
if (node->children.size() > 1 && node->children[1]->name == "ARG_LIST") {
    argNodes = node->children[1]->children;
}

size_t expectedParams = funcSym->paramTypes.size();
size_t actualArgs = argNodes.size();

// Check argument count
if (actualArgs != expectedParams) {
    addError("Function '" + funcName + "' expects " + 
             to_string(expectedParams) + " argument(s), but " + 
             to_string(actualArgs) + " provided");
    return;  // Don't check types if count is wrong
}

// Check argument types
for (size_t i = 0; i < actualArgs; i++) {
  if (funcSym->paramTypes.size() <= i) {
  cout<<"HI"<<endl;
        break;  // Already reported count mismatch above
    }
    string argType = getExpressionType(argNodes[i]);
    string paramType = funcSym->paramTypes[i];
    
    // Skip if we couldn't determine argument type
    if (argType.empty()) continue;
    
    // Check type compatibility
    if (!areTypesCompatible(argType, paramType, "=")) {
        addError("Type mismatch for argument " + to_string(i + 1) + 
                 " of function '" + funcName + "': expected '" + paramType + 
                 "', but got '" + argType + "'");
    }
}
}

void SemanticAnalyzer::checkIdentifier(Node* node) {
    if (!node || node->lexeme.empty()) return;
    
    Symbol* sym = symbolTable.lookup(node->lexeme);
    if (!sym) {
        addError("Undeclared identifier: '" + node->lexeme + "'");
    }
}
bool SemanticAnalyzer::isDeclarationContext(Node* node) {
    if (!node || !node->parent) return false;
    
    Node* parent = node->parent;
    
    // Check if this identifier is being declared (not used)
    if (parent->name == "PARAM_DECL" || 
        parent->name == "FUNCTION_DECL" ||
        parent->name == "ENUMERATOR") {
        return true;
    }
    
    // ADD THIS: If parent is member access, this is a member name (not standalone identifier)
    if (parent->name == "MEMBER_ACCESS" || parent->name == "PTR_MEMBER_ACCESS") {
        return true;  // Don't check as standalone identifier
    }
    
    // Check if it's the LHS of a declaration assignment
    if (parent->name == "ASSIGN_EXPR" && !parent->children.empty() && 
        parent->children[0] == node) {
        // Check if this ASSIGN_EXPR is part of INIT_DECL_LIST
        Node* grandparent = parent->parent;
        if (grandparent && grandparent->name == "INIT_DECL_LIST") {
            return true;
        }
    }
    
    return false;
}


void SemanticAnalyzer::checkReturnStatement(Node* node) {
    if (!node) return;
    
    // Check if we're inside a function
    if (currentFunctionReturnType.empty()) {
        addError("Return statement outside of function");
        return;
    }
    
    // Check if return has a value
    bool hasReturnValue = !node->children.empty();
    
    if (currentFunctionReturnType == "void") {
        if (hasReturnValue) {
            addError("Function '" + currentFunctionName + "' with void return type cannot return a value");
        }
    } else {
        if (!hasReturnValue) {
            addError("Function '" + currentFunctionName + "' with return type '" + 
                     currentFunctionReturnType + "' must return a value");
        }
        // Optional: Add type checking here later
    }
}


void SemanticAnalyzer::analyzeDeclarator(Node* node, string& name, int& pointerDepth,
                                         bool& isArray, vector<int>& arrayDims) {
    if (!node) return;
    
    if (node->name == "IDENTIFIER") {
        name = node->lexeme;
        return;
    }
    
    if (node->name == "POINTER") {
        pointerDepth++;
    }
    
    if (node->name == "ARRAY") {
        isArray = true;
        arrayDims = extractArrayDimensions(node, name);
        return;  // Don't recurse further, extractArrayDimensions handles it
    }
    
    for (auto child : node->children) {
        analyzeDeclarator(child, name, pointerDepth, isArray, arrayDims);
    }
}
vector<int> SemanticAnalyzer::extractArrayDimensions(Node* arrayNode, string& varName) {
    vector<int> dimensions;
    Node* current = arrayNode;
    
    while (current && current->name == "ARRAY") {
        int size = -1;
        Node* identifier = nullptr;
        
        // ARRAY has: [0] = base (IDENTIFIER or nested ARRAY), [1] = size (optional)
        if (!current->children.empty()) {
            identifier = current->children[0];
            
            if (current->children.size() > 1) {
                Node* sizeNode = current->children[1];
              
                 size = evaluateConstantExpression(sizeNode);
                
                if ( size <= 0) {
                    addError("Array dimension must be positive, got: " + to_string(size));
                }
            }
        }
        
        dimensions.push_back(size);
        current = identifier;
        
        // If we hit the identifier, extract it
        if (current && current->name == "IDENTIFIER") {
            varName = current->lexeme;
            break;
        }
    }
    
    // Reverse because we extracted from outermost to innermost
    reverse(dimensions.begin(), dimensions.end());
    return dimensions;
}

int SemanticAnalyzer::evaluateConstantExpression(Node* node) {
    if (!node) return -1;
    
    if (node->name == "INTEGER_CONSTANT") {
        return stoi(node->lexeme);
    }
    
    // Handle unary minus: -5
    if (node->name == "UNARY_OP" && node->children.size() == 2) {
        Node* op = node->children[0];
        Node* operand = node->children[1];
        
        if (op->name == "-") {
            int val = evaluateConstantExpression(operand);
            return (val == -1) ? -1 : -val;
        }
        else if (op->name == "+") {
            return evaluateConstantExpression(operand);
        }
    }
    
    // Handle binary operations: 3 + 2, 10 - 5, etc.
    if (node->name == "ADD_EXPR" && node->children.size() == 2) {
        int left = evaluateConstantExpression(node->children[0]);
        int right = evaluateConstantExpression(node->children[1]);
        return (left == -1 || right == -1) ? -1 : left + right;
    }
    
    if (node->name == "SUB_EXPR" && node->children.size() == 2) {
        int left = evaluateConstantExpression(node->children[0]);
        int right = evaluateConstantExpression(node->children[1]);
        return (left == -1 || right == -1) ? -1 : left - right;
    }
    
    if (node->name == "MUL_EXPR" && node->children.size() == 2) {
        int left = evaluateConstantExpression(node->children[0]);
        int right = evaluateConstantExpression(node->children[1]);
        return (left == -1 || right == -1) ? -1 : left * right;
    }
    
    if (node->name == "DIV_EXPR" && node->children.size() == 2) {
        int left = evaluateConstantExpression(node->children[0]);
        int right = evaluateConstantExpression(node->children[1]);
        return (left == -1 || right == -1 || right == 0) ? -1 : left / right;
    }
    
    // Can't evaluate - non-constant expression
    return -1;
}

void SemanticAnalyzer::checkArrayAccess(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    Node* arrayNode = node->children[0];
    Node* indexNode = node->children[1];
    
    // Check if the base is actually an array or pointer
    if (arrayNode->name == "IDENTIFIER") {
        Symbol* sym = symbolTable.lookup(arrayNode->lexeme);
        if (sym) {
            if (!sym->isArray && sym->pointerDepth == 0) {
                addError("Subscripted value '" + arrayNode->lexeme + "' is not an array or pointer");
            }
        }
    }
    
    // Check if index is integer type
    string indexType = getExpressionType(indexNode);
    
    if (!indexType.empty() && indexType != "int" && indexType != "long" && 
        indexType != "short" && indexType != "char") {
        addError("Array subscript is not an integer (got '" + indexType + "')");
    }
}



void SemanticAnalyzer::checkUnaryOperation(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    Node* opNode = node->children[0];
    Node* operandNode = node->children[1];
    
    string operandType = getExpressionType(operandNode);
    
    if (opNode->name == "*") {  // Dereference operator
        if (!isPointerType(operandType)) {
            addError("Invalid type argument of unary '*' (have '" + operandType + "')");
        }
    }
    else if (opNode->name == "&") {  // Address-of operator
        // Check if operand is an lvalue
        if (operandNode->name != "IDENTIFIER" && 
            operandNode->name != "ARRAY_ACCESS" &&
            operandNode->name != "MEMBER_ACCESS") {
            addError("Cannot take address of rvalue");
        }
    }
    // ADD THESE MISSING CASES:
    else if (opNode->name == "~") {  // Bitwise NOT
        if (!isIntegerType(operandType)) {
            addError("Invalid operand to unary ~ (have '" + operandType + "')");
        }
    }
    else if (opNode->name == "!") {  // Logical NOT
        // Can be applied to arithmetic or pointer types
        if (!isNumericType(operandType) && !isPointerType(operandType) && operandType != "bool") {
            addError("Invalid operand to unary ! (have '" + operandType + "')");
        }
    }
    else if (opNode->name == "+" || opNode->name == "-") {  // Unary plus/minus
        if (!isNumericType(operandType)) {
            addError("Invalid operand to unary " + opNode->name + " (have '" + operandType + "')");
        }
    }
  
}

string SemanticAnalyzer::getExpressionType(Node* node) {
    if (!node) return "";
    
  
     if (node->name == "IDENTIFIER") {
        Symbol* sym = symbolTable.lookup(node->lexeme);
        if (!sym) return "";
        
        //string type = sym->type;
        string type = resolveTypedef(sym->type);
        
        // If it's an array, treat it as a pointer in expressions
        if (sym->isArray) {
            return type + "*";
        }
        
        // If it has pointer depth, add asterisks
        for (int i = 0; i < sym->pointerDepth; i++) {
            type += "*";
        }
        
        return type;
    }
    else if (node->name == "INTEGER_CONSTANT") return "int";
    else if (node->name == "FLOAT_CONSTANT") return "float";
    else if (node->name == "CHAR_LITERAL") return "char";
    else if (node->name == "STRING_LITERAL") return "char*";
    else if (node->name == "BOOL_LITERAL") return "bool";
    else if (node->name == "HEX_INT_LITERAL" || 
             node->name == "OCTAL_INT_LITERAL" || 
             node->name == "BINARY_INT_LITERAL") return "int";
    else if (node->name == "HEX_FLOAT_LITERAL") return "float";
    else if (node->name == "FUNC_CALL") {
        if (!node->children.empty() && node->children[0]->name == "IDENTIFIER") {
            Symbol* funcSym = symbolTable.lookup(node->children[0]->lexeme);
            return funcSym ? funcSym->type : "";
        }
    }
    // Handle array access - returns element type
    else if (node->name == "ARRAY_ACCESS") {
        if (!node->children.empty()) {
            string baseType = getExpressionType(node->children[0]);
            // Remove one level of pointer/array
            if (baseType.length() > 0 && baseType.back() == '*') {
                return baseType.substr(0, baseType.length() - 1);
            }
            return baseType;
        }
    }
    // Handle arithmetic operations - result type matters
    else if (node->name == "ADD_EXPR" || node->name == "SUB_EXPR") {
        if (node->children.size() >= 2) {
            string leftType = getExpressionType(node->children[0]);
            string rightType = getExpressionType(node->children[1]);
            
            // pointer +/- int = pointer
            if (isPointerType(leftType) && isIntegerType(rightType)) {
                return leftType;
            }
            // int + pointer = pointer
            if (node->name == "ADD_EXPR" && isIntegerType(leftType) && isPointerType(rightType)) {
                return rightType;
            }
            // pointer - pointer = ptrdiff_t (treated as int)
            if (node->name == "SUB_EXPR" && isPointerType(leftType) && isPointerType(rightType)) {
                return "int";
            }
            
            // numeric operation
            return getResultType(leftType, rightType);
        }
    }
    else if (node->name == "MUL_EXPR" || node->name == "DIV_EXPR" || node->name == "MOD_EXPR") {
        if (node->children.size() >= 2) {
            string leftType = getExpressionType(node->children[0]);
            string rightType = getExpressionType(node->children[1]);
            return getResultType(leftType, rightType);
        }
    }
    // Comparison and logical operations return int/bool
    else if (node->name == "EQ_EXPR" || node->name == "NEQ_EXPR" || 
             node->name == "LT_EXPR" || node->name == "GT_EXPR" ||
             node->name == "LE_EXPR" || node->name == "GE_EXPR" ||
             node->name == "LOGICAL_AND" || node->name == "LOGICAL_OR") {
        return "bool";  // or "bool" depending on your language semantics
    }
    // Bitwise operations return integer type
    else if (node->name == "BIT_AND" || node->name == "BIT_OR" || 
             node->name == "BIT_XOR" || node->name == "LSHIFT_EXPR" || 
             node->name == "RSHIFT_EXPR") {
        if (node->children.size() >= 2) {
            string leftType = getExpressionType(node->children[0]);
            string rightType = getExpressionType(node->children[1]);
            return getResultType(leftType, rightType);
        }
    }
    // Handle dereference - removes pointer level
    else if (node->name == "UNARY_OP" && node->children.size() >= 2) {
        if (node->children[0]->name == "*") {
            string operandType = getExpressionType(node->children[1]);
            if (operandType.length() > 0 && operandType.back() == '*') {
                return operandType.substr(0, operandType.length() - 1);
            }
        }
        
        // Handle address-of - adds pointer level
        else if (node->children[0]->name == "&") {
          string operandType = getExpressionType(node->children[1]);
        
        // Handle address-of operator: add one pointer level
        if (!operandType.empty()) {
            // If operand is already a pointer, increment pointer level
            if (isPointerType(operandType)) {
                return operandType + "*";
            } else {
                // If operand is non-pointer, make it a pointer
                return operandType + "*";
            }
        }
        }
        // Unary +/- keeps the type
        else if (node->children[0]->name == "+" || node->children[0]->name == "-") {
            return getExpressionType(node->children[1]);
        }
    }
    // Assignment expression - returns type of LHS
    else if (node->name == "ASSIGN_EXPR" && !node->children.empty()) {
        return getExpressionType(node->children[0]);
    }
    // Handle member access - returns member's type
// Handle member access - returns member's type
else if (node->name == "MEMBER_ACCESS" || node->name == "PTR_MEMBER_ACCESS") {
    if (node->children.size() >= 2) {
        Node* baseNode = node->children[0];
        Node* memberNode = node->children[1];
        
        if (memberNode->name != "IDENTIFIER") return "";
        
        string baseType = getExpressionType(baseNode);
        string memberName = memberNode->lexeme;
        
        // Remove pointer level if arrow access (PTR_MEMBER_ACCESS)
        if (node->name == "PTR_MEMBER_ACCESS" && isPointerType(baseType)) {
            baseType = baseType.substr(0, baseType.length() - 1);
        }
        
        // Remove "struct " prefix
        if (baseType.find("struct ") == 0) {
            baseType = baseType.substr(7);
        }
        
        // Look up struct and get member type
        Symbol* structSym = symbolTable.lookup(baseType);
        if (structSym && structSym->isStruct) {
            auto it = structSym->structMembers.find(memberName);
            if (it != structSym->structMembers.end()) {
                return it->second;  // Return member's type
            }
        }
    }
}
    // For other expressions, recursively check first child
    else if (!node->children.empty()) {
        return getExpressionType(node->children[0]);
    }
    
    return "";
}

void SemanticAnalyzer::processStruct(Node* node) {
    if (!node) return;
    
    string structName;
    map<string, string> members;
    
    for (auto child : node->children) {
        if (child->name == "IDENTIFIER") {
            structName = child->lexeme;
        }
        else if (child->name == "DECLARATION") {
            extractStructMembers(child, members);
        }
    }
    
    if (structName.empty()) {
        return; // Anonymous struct
    }
    
    if (!members.empty()) {
        symbolTable.enterScope();
        
        // Add struct members to the struct scope
        for (const auto& member : members) {
            if (!symbolTable.addSymbol(member.first, member.second, node)) {
                addError("Duplicate struct member: " + member.first);
            } else {
                cout << "DEBUG: Added struct member " << member.first << " [" << member.second << "] to struct scope\n";
            }
        }
        
        // EXIT STRUCT MEMBER SCOPE (members are now in scope history)
        symbolTable.exitScope();
        
        // Now add the struct type itself to the GLOBAL scope
        if (symbolTable.addSymbol(structName, "struct", node, false, {}, false, {}, 0, true, false)) {
            Symbol* sym = symbolTable.lookupCurrentScope(structName);
            if (sym) {
                sym->structMembers = members;  // Store member info in the struct symbol
            }
            cout << "DEBUG: Defined struct " << structName << " with " << members.size() << " member(s)\n";
        } else {
            addError("Redefinition of struct '" + structName + "'");
        }
    }
}

void SemanticAnalyzer::extractStructMembers(Node* declNode, map<string, string>& members) {
    if (!declNode || declNode->name != "DECLARATION") return;
    
    string memberType;
    vector<string> memberNames;
    
    for (auto child : declNode->children) {
        if (child->name == "DECL_SPECIFIERS") {
            memberType = extractTypeFromDeclSpecifiers(child);
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto initDecl : child->children) {
                if (!initDecl->children.empty()) {
                    Node* firstChild = initDecl->children[0];
                    if (firstChild->name == "IDENTIFIER") {
                        memberNames.push_back(firstChild->lexeme);
                    }
                    // Handle declarators with pointers/arrays
                    else if (firstChild->name == "DECLARATOR") {
                        string memberName;
                        int pointerDepth = 0;
                        bool isArray = false;
                        vector<int> arrayDims;
                        analyzeDeclarator(firstChild, memberName, pointerDepth, isArray, arrayDims);
                        if (!memberName.empty()) {
                            // Append pointer notation to type if needed
                            string fullType = memberType;
                            for (int i = 0; i < pointerDepth; i++) {
                                fullType += "*";
                            }
                            members[memberName] = fullType;
                            cout << "DEBUG: Found struct member: " << memberName << " [" << fullType << "]\n";
                            continue;  // Skip the default add below
                        }
                    }
                }
            }
        }
        else if (child->name == "DECLARATION") {
            // Nested declaration - recursively extract
            extractStructMembers(child, members);
        }
    }
    
    // Add simple members (non-pointer, non-array)
    for (const string& name : memberNames) {
        if (members.find(name) == members.end()) {  // If not already added
            members[name] = memberType;
            cout << "DEBUG: Found struct member: " << name << " [" << memberType << "]\n";
        }
    }
}
void SemanticAnalyzer::processEnum(Node* node) {
    if (!node) return;
    
    string enumName;
    vector<Node*> enumerators;
    bool isDefinition = false;
    
    // Check if this is a definition (has enumerators) or declaration (no enumerators)
    for (auto child : node->children) {
        if (child->name == "IDENTIFIER") {
            enumName = child->lexeme;
        }
        else if (child->name == "ENUMERATOR_LIST") {
            isDefinition = true;
            // Extract all enumerators from ENUMERATOR_LIST
            for (auto enumeratorNode : child->children) {
                if (enumeratorNode->name == "ENUMERATOR") {
                    if (!enumeratorNode->children.empty() && 
                        enumeratorNode->children[0]->name == "IDENTIFIER") {
                        enumerators.push_back(enumeratorNode->children[0]);
                    }
                }
            }
        }
    }
    
    if (enumName.empty()) {
        return; // Anonymous enum
    }
    
    // Lookup if this enum was previously declared
    Symbol* existingSym = symbolTable.lookupCurrentScope(enumName);
    
    if (isDefinition) {
        // This is a definition (with enumerators)
        if (!enumerators.empty()) {
            map<string, int> values;
            int currentValue = 0;
            
            for (auto enumerator : enumerators) {
                values[enumerator->lexeme] = currentValue++;
            }
            
            if (existingSym) {
              
              if (existingSym->isEnum) {
                    // Update the existing symbol with enum values
                   
            existingSym->enumValues = values;
      
                    // ✅ ADD THIS: Create scope for enum constants
                    //symbolTable.enterScope();
                    for (const auto& pair : values) {
                        if (!symbolTable.addSymbol(pair.first, "int", node)) {
                            addError("Redefinition of enumerator '" + pair.first + "'");
                        } else {
                            cout << "DEBUG: Added enum constant " << pair.first << " = " << pair.second << "\n";
                        }
                    }
                    //symbolTable.exitScope();
                     
                    cout << "DEBUG: Defined previously declared enum " << enumName << " with " << (existingSym->enumValues).size() << " value(s)\n";
                   
                    
                } else {
                    addError("Redefinition of '" + enumName + "' as different kind of symbol");
                }
            }else {
                // New enum definition
                if (symbolTable.addSymbol(enumName, "enum", node, false, {}, false, {}, 0, false, true)) {
                    Symbol* sym = symbolTable.lookupCurrentScope(enumName);
                    if (sym) {
                        sym->enumValues = values;
                    }
                    
                    // Add enum constants to scope
                    //symbolTable.enterScope();
                    for (const auto& pair : values) {
                        if (!symbolTable.addSymbol(pair.first, "int", node)) {
                            addError("Redefinition of enumerator '" + pair.first + "'");
                        } else {
                            cout << "DEBUG: Added enum constant " << pair.first << " = " << pair.second << "\n";
                        }
                    }
                    //symbolTable.exitScope();
                    
                    cout << "DEBUG: Defined enum " << enumName << " with " << values.size() << " value(s)\n";
                } else {
                    addError("Redefinition of enum '" + enumName + "'");
                }
            }
        }
    } else {
        // This is a forward declaration (no enumerators)
        if (existingSym) {
            if (!existingSym->isEnum) {
                addError("Redefinition of '" + enumName + "' as different kind of symbol");
            }
            // Else: duplicate declaration is OK in C
        } else {
            // Add forward declaration
            if (symbolTable.addSymbol(enumName, "enum", node, false, {}, false, {}, 0, false, true)) {
                cout << "DEBUG: Forward declared enum " << enumName << "\n";
            }
        }
    }
}

bool SemanticAnalyzer::isStructMemberDeclaration(Node* node) {
    if (!node || !node->parent) return false;
    
    Node* parent = node->parent;
    while (parent) {
        if (parent->name == "STRUCT_OR_UNION_SPECIFIER") {
            return true; // This declaration is inside a struct definition
        }
        parent = parent->parent;
    }
    return false;
}



// ============== TYPE CHECKING METHODS ==============


bool SemanticAnalyzer::isIntegerType(const std::string& type) {
    return type == "int" || type == "short" || type == "long" || type == "long long" ||
           type == "unsigned" || type == "unsigned int" || type == "unsigned short" || 
           type == "unsigned long" || type == "signed" || type == "char" ||
           type == "unsigned char" || type == "signed char" || type == "bool";
}
bool SemanticAnalyzer::isNumericType(const std::string& type) {
    return isIntegerType(type) || type == "float" || type == "double";
}

bool SemanticAnalyzer::isPointerType(const string& type) {
    return type.find("*") != string::npos || type == "char*" || type == "int*";
}


string SemanticAnalyzer::getResultType(const string& type1, const string& type2) {
    // If either is double, result is double
    if (type1 == "double" || type2 == "double") return "double";
    
    // If either is float, result is float
    if (type1 == "float" || type2 == "float") return "float";
    
    // If either is long, result is long
    if (type1 == "long" || type2 == "long") return "long";
    
    // Otherwise, result is int
    return "int";
}

bool SemanticAnalyzer::areTypesCompatible(const string& type1, const string& type2, 
                                           const string& operation) {
    // Empty types (couldn't determine) - skip checking
    if (type1.empty() || type2.empty()) return true;
    
    // Exact match - always compatible
    if (type1 == type2) return true;
    
    // ========== ARITHMETIC OPERATIONS ==========
    if (operation == "+" || operation == "-" || operation == "*" || 
        operation == "/" || operation == "%") {
        
        // Both numeric types - compatible
        if (isNumericType(type1) && isNumericType(type2)) return true;
        
        // Pointer arithmetic: pointer +/- integer
        if ((operation == "+" || operation == "-") &&
            ((isPointerType(type1) && isIntegerType(type2)) ||
             (isIntegerType(type1) && isPointerType(type2)))) {
            return true;
        }
        
        // Pointer subtraction: pointer - pointer
        if (operation == "-" && isPointerType(type1) && isPointerType(type2)) {
            return true;
        }
        
        // Modulo requires integer types
        if (operation == "%" && isIntegerType(type1) && isIntegerType(type2)) {
            return true;
        }
        
        return false;
    }
    
    // ========== COMPARISON OPERATIONS ==========
    if (operation == "==" || operation == "!=" || operation == "<" || 
        operation == ">" || operation == "<=" || operation == ">=") {
        
        // CASE 1: Both numeric types - always compatible
        if (isNumericType(type1) && isNumericType(type2)) return true;
        
        // CASE 2: Both boolean types - compatible
        if (type1 == "bool" && type2 == "bool") return true;
        
        // CASE 3: Boolean with integer (0=false, non-zero=true)
        if ((type1 == "bool" && isIntegerType(type2)) || 
            (type2 == "bool" && isIntegerType(type1))) {
            return true;
        }
        
        // CASE 4: Integer with char (char is essentially integer)
        if ((isIntegerType(type1) && type2 == "char") ||
            (type1 == "char" && isIntegerType(type2))) {
            return true;
        }
        
        // CASE 5: Both pointer types - can compare addresses
        if (isPointerType(type1) && isPointerType(type2)) return true;
        
        // CASE 6: Pointer with integer (for null checks)
        if ((isPointerType(type1) && isIntegerType(type2)) ||
            (isPointerType(type2) && isIntegerType(type1))) {
            return true;
        }
        
        // CASE 7: Pointer with boolean (treat as null check)
        if ((isPointerType(type1) && type2 == "bool") ||
            (isPointerType(type2) && type1 == "bool")) {
            return true;
        }
        
        // CASE 8: Character with boolean
        if ((type1 == "char" && type2 == "bool") ||
            (type2 == "char" && type1 == "bool")) {
            return true;
        }
        
        return false;
    }
    
    // ========== LOGICAL OPERATIONS ==========
    if (operation == "&&" || operation == "||") {
        // Any scalar type can be used in logical context
        // (will be converted to boolean: 0=false, non-zero=true)
        if ((isNumericType(type1) || isPointerType(type1) || type1 == "bool") &&
            (isNumericType(type2) || isPointerType(type2) || type2 == "bool")) {
            return true;
        }
        return false;
    }
    
    // ========== BITWISE OPERATIONS ==========
    if (operation == "&" || operation == "|" || operation == "^" || 
        operation == "<<" || operation == ">>") {
        // Both must be integer types (including char)
        if ((isIntegerType(type1) || type1 == "char") && 
            (isIntegerType(type2) || type2 == "char")) {
            return true;
        }
        return false;
    }
    
    // ========== ASSIGNMENT OPERATIONS ==========
    if (operation == "=" || operation == "+=" || operation == "-=" || 
        operation == "*=" || operation == "/=" || operation == "%=" ||
        operation == "&=" || operation == "|=" || operation == "^=" ||
        operation == "<<=" || operation == ">>=") {
        
        // Exact match
        if (type1 == type2) return true;
        
        // Both numeric types
        if (isNumericType(type1) && isNumericType(type2)) return true;
        
        // Pointer assignments
        if (isPointerType(type1) && isPointerType(type2)) return true;
        
        // Assigning integer to pointer (for null assignment)
        if (isPointerType(type1) && isIntegerType(type2)) {
            return true;
        }
        
        // Integer with char
        if ((isIntegerType(type1) && type2 == "char") ||
            (type1 == "char" && isIntegerType(type2))) {
            return true;
        }
        
        // Boolean with integer
        if ((type1 == "bool" && isIntegerType(type2)) || 
            (type2 == "bool" && isIntegerType(type1))) {
            return true;
        }
        
        // Character with boolean
        if ((type1 == "char" && type2 == "bool") ||
            (type2 == "char" && type1 == "bool")) {
            return true;
        }
        
        return false;
    }
    
    // ========== UNKNOWN OPERATION ==========
    // For unknown operations, use conservative approach
    return false;
}
void SemanticAnalyzer::checkArithmeticOperation(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    Node* left = node->children[0];
    Node* right = node->children[1];
    
    string leftType = getExpressionType(left);
    string rightType = getExpressionType(right);
    
    // Skip if we couldn't determine types
    if (leftType.empty() || rightType.empty()) return;
    
    string operation = "";
    if (node->name == "ADD_EXPR") operation = "+";
    else if (node->name == "SUB_EXPR") operation = "-";
    else if (node->name == "MUL_EXPR") operation = "*";
    else if (node->name == "DIV_EXPR") operation = "/";
    else if (node->name == "MOD_EXPR") operation = "%";
    
    // Modulo requires integer types (no pointers, no floats)
    if (node->name == "MOD_EXPR") {
        if (!isIntegerType(leftType)) {
            addError("Invalid left operand type '" + leftType + "' for modulo operation (requires integer type)");
        }
        if (!isIntegerType(rightType)) {
            addError("Invalid right operand type '" + rightType + "' for modulo operation (requires integer type)");
        }
        return;
    }
    
    // Multiplication and Division: pointers are NOT allowed
    if (node->name == "MUL_EXPR" || node->name == "DIV_EXPR") {
        if (isPointerType(leftType)) {
            addError("Invalid left operand type '" + leftType + "' for operation '" + operation + "' (pointer not allowed)");
        }
        if (isPointerType(rightType)) {
            addError("Invalid right operand type '" + rightType + "' for operation '" + operation + "' (pointer not allowed)");
        }
        
        // Check both are numeric
        if (!isNumericType(leftType) && !isPointerType(leftType)) {
            addError("Invalid left operand type '" + leftType + "' for arithmetic operation '" + operation + "'");
        }
        if (!isNumericType(rightType) && !isPointerType(rightType)) {
            addError("Invalid right operand type '" + rightType + "' for arithmetic operation '" + operation + "'");
        }
        return;
    }
    
    // Addition and Subtraction: pointer arithmetic allowed with restrictions
    if (node->name == "ADD_EXPR") {
        // pointer + pointer is INVALID
        if (isPointerType(leftType) && isPointerType(rightType)) {
            addError("Invalid operation: cannot add two pointers");
            return;
        }
        
        // pointer + float is INVALID
        if (isPointerType(leftType) && (rightType == "float" || rightType == "double")) {
            addError("Invalid operation: cannot add pointer and floating-point type");
            return;
        }
        if (isPointerType(rightType) && (leftType == "float" || leftType == "double")) {
            addError("Invalid operation: cannot add floating-point type and pointer");
            return;
        }
    }
    
    if (node->name == "SUB_EXPR") {
        // int - pointer is INVALID (but pointer - int is valid)
        if (isIntegerType(leftType) && isPointerType(rightType)) {
            addError("Invalid operation: cannot subtract pointer from integer");
            return;
        }
        
        // pointer - float is INVALID
        if (isPointerType(leftType) && (rightType == "float" || rightType == "double")) {
            addError("Invalid operation: cannot subtract floating-point type from pointer");
            return;
        }
    }
    
    // Check general compatibility
    if (!areTypesCompatible(leftType, rightType, operation)) {
        addError("Type mismatch in arithmetic operation: '" + leftType + "' " + operation + " '" + rightType + "'");
    }
}

void SemanticAnalyzer::checkComparisonOperation(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    Node* left = node->children[0];
    Node* right = node->children[1];
    
    string leftType = getExpressionType(left);
    string rightType = getExpressionType(right);
    
    // Skip if we couldn't determine types
    if (leftType.empty() || rightType.empty()) return;
    
    string operation = "";
    if (node->name == "LT_EXPR") operation = "<";
    else if (node->name == "GT_EXPR") operation = ">";
    else if (node->name == "LE_EXPR") operation = "<=";
    else if (node->name == "GE_EXPR") operation = ">=";
    else if (node->name == "EQ_EXPR") operation = "==";
    else if (node->name == "NEQ_EXPR") operation = "!=";
    
    // Check if types are comparable
    if (!isNumericType(leftType) && !isPointerType(leftType)) {
        addError("Invalid left operand type '" + leftType + "' for comparison '" + operation + "'");
    }
    if (!isNumericType(rightType) && !isPointerType(rightType)) {
        addError("Invalid right operand type '" + rightType + "' for comparison '" + operation + "'");
    }
    
    // Check compatibility
    if (!areTypesCompatible(leftType, rightType, operation)) {
        addError("Type mismatch in comparison: '" + leftType + "' " + operation + " '" + rightType + "'");
    }
}


void SemanticAnalyzer::checkBitwiseOperation(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    string operation = "";
    if (node->name == "BIT_AND") operation = "&";
    else if (node->name == "BIT_OR") operation = "|";
    else if (node->name == "BIT_XOR") operation = "^";
    else if (node->name == "LSHIFT_EXPR") operation = "<<";
    else if (node->name == "RSHIFT_EXPR") operation = ">>";
    
    for (auto child : node->children) {
        string childType = getExpressionType(child);
        
        // Skip if we couldn't determine type
        if (childType.empty()) continue;
        
        // Bitwise operations require integer types
        if (!isIntegerType(childType)) {
            addError("Invalid operand type '" + childType + "' for bitwise operation '" + operation + "' (requires integer type)");
        }
    }
}

void SemanticAnalyzer::checkAssignment(Node* node) {
    if (!node || node->children.size() < 3) return;
    
    Node* lhs = node->children[0];
    Node* rhs = node->children[2];  // children[1] is the operator
    
    string lhsType = getExpressionType(lhs);
    string rhsType = getExpressionType(rhs);
    
    // Skip if we couldn't determine types
    if (lhsType.empty() || rhsType.empty()) return;
    
    // Exact match is always OK
    if (lhsType == rhsType) return;
    
    // Numeric types can be assigned to each other (with implicit conversion)
    if (isNumericType(lhsType) && isNumericType(rhsType)) return;
    
    // Pointer to pointer of same base type
    if (isPointerType(lhsType) && isPointerType(rhsType)) {
        checkPointerAssignment(/*node, */lhsType, rhsType);
        return;
    }
    
    // NULL/0 can be assigned to pointers
    if (isPointerType(lhsType) && isIntegerType(rhsType)) {
        // Allow for now (could be NULL)
        return;
    }
    
    // Cannot assign pointer to non-pointer numeric
    if (!isPointerType(lhsType) && isPointerType(rhsType) && isNumericType(lhsType)) {
        addError("Cannot assign pointer type '" + rhsType + "' to non-pointer type '" + lhsType + "'");
        return;
    }
    
    // Cannot assign non-pointer numeric to pointer
    if (isPointerType(lhsType) && !isPointerType(rhsType) && isNumericType(rhsType)) {
        // Allow integer constants (could be 0/NULL), but warn for variables
        if (rhs->name != "INTEGER_CONSTANT") {
            addError("Cannot assign non-pointer type '" + rhsType + "' to pointer type '" + lhsType + "'");
        }
        return;
    }
    
    // General type mismatch
    if (lhsType != rhsType) {
        addError("Type mismatch in assignment: cannot assign '" + rhsType + "' to '" + lhsType + "'");
    }
}

void SemanticAnalyzer::checkLogicalOperation(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    string operation = (node->name == "LOGICAL_AND") ? "&&" : "||";
    
    Node* left = node->children[0];
    Node* right = node->children[1];
    
    string leftType = getExpressionType(left);
    string rightType = getExpressionType(right);
    
    // Skip if we couldn't determine types
    if (leftType.empty() || rightType.empty()) return;
    
    // Helper function to check if type is valid for logical operations
    auto isValidForLogicalOp = [this](const string& type) -> bool {
        // Valid types for logical operations:
        // - boolean types directly
        // - integer types (can be converted to boolean: 0=false, non-zero=true)
        // - pointer types (can be converted to boolean: null=false, non-null=true)
        
        if (type == "bool") return true;
        if (isIntegerType(type)) return true;
        if (isPointerType(type)) return true;
        
        // Floating-point types should NOT be used directly in logical operations
        // Characters should NOT be used directly in logical operations
        return false;
    };
    
    // Check left operand
    if (!isValidForLogicalOp(leftType)) {
        addError("Invalid operand type '" + leftType + "' for logical operation '" + operation + 
                "' (requires boolean, integer, or pointer type)");
    }
    
    // Check right operand
    if (!isValidForLogicalOp(rightType)) {
        addError("Invalid operand type '" + rightType + "' for logical operation '" + operation + 
                "' (requires boolean, integer, or pointer type)");
    }
    
    // Additional specific checks for common mistakes
    if (leftType == "float" || leftType == "double") {
        addError("Invalid use of floating-point type '" + leftType + "' in logical operation '" + operation + 
                "' (use explicit comparison: " + leftType + " != 0.0)");
    }
    
    if (rightType == "float" || rightType == "double") {
        addError("Invalid use of floating-point type '" + rightType + "' in logical operation '" + operation + 
                "' (use explicit comparison: " + rightType + " != 0.0)");
    }
    
    if (leftType == "char") {
        addError("Invalid use of character type in logical operation '" + operation + 
                "' (use explicit comparison: " + leftType + " != '\\0')");
    }
    
    if (rightType == "char") {
        addError("Invalid use of character type in logical operation '" + operation + 
                "' (use explicit comparison: " + rightType + " != '\\0')");
    }
}

void SemanticAnalyzer::checkPointerAssignment(/*Node* node, */const string& lhsType, const string& rhsType) {
    if (!isPointerType(lhsType) || !isPointerType(rhsType)) return;
    
    int lhsLevel = countPointerLevel(lhsType);
    int rhsLevel = countPointerLevel(rhsType);
    
    if (lhsLevel != rhsLevel) {
        addError("Pointer level mismatch: cannot assign '" + rhsType + 
                "' (level " + to_string(rhsLevel) + ") to '" + lhsType + 
                "' (level " + to_string(lhsLevel) + ")");
    }
}

int SemanticAnalyzer::countPointerLevel(const string& type) {
    int level = 0;
    for (char c : type) {
        if (c == '*') level++;
    }
    return level;
}

 void SemanticAnalyzer::checkPostfixOperation(Node* node) {
    if (!node || node->children.empty()) return;
    
    Node* operandNode = node->children[0];
    string operandType = getExpressionType(operandNode);
    
    string operation = (node->name == "POST_INC") ? "++" : "--";
    
    // Check if operand is an lvalue
    if (!isLvalue(operandNode)) {
        addError("Lvalue required as " + operation + " operand");
        return;
    }
    
    // Check if operand type is valid (numeric or pointer)
    if (!operandType.empty() && !isNumericType(operandType) && !isPointerType(operandType)) {
        addError("Invalid operand to " + operation + " (have '" + operandType + "')");
    }
}

void SemanticAnalyzer::checkPrefixOperation(Node* node) {
    if (!node || node->children.empty()) return;
    
    Node* operandNode = node->children[0];
    string operandType = getExpressionType(operandNode);
    
    string operation = (node->name == "PRE_INC") ? "++" : "--";
    
    // Check if operand is an lvalue
    if (!isLvalue(operandNode)) {
        addError("Lvalue required as " + operation + " operand");
        return;
    }
    
    // Check if operand type is valid (numeric or pointer)
    if (!operandType.empty() && !isNumericType(operandType) && !isPointerType(operandType)) {
        addError("Invalid operand to " + operation + " (have '" + operandType + "')");
    }
}
bool SemanticAnalyzer::isLvalue(Node* node) {
    if (!node) return false;
    
    // Valid lvalues:
    if (node->name == "IDENTIFIER") return true;
    if (node->name == "ARRAY_ACCESS") return true;
    if (node->name == "MEMBER_ACCESS") return true;
    if (node->name == "PTR_MEMBER_ACCESS") return true;   // Arrow access
    
    // Dereferenced pointer is an lvalue: *ptr
    if (node->name == "UNARY_OP" && node->children.size() >= 2) {
        if (node->children[0]->name == "*") {
            return true;
        }
    }
    
    // Invalid lvalues (rvalues) - constants and literals:
    if (node->name == "INTEGER_CONSTANT") return false;
    if (node->name == "FLOAT_CONSTANT") return false;
    if (node->name == "CHAR_LITERAL") return false;
    if (node->name == "STRING_LITERAL") return false;
    if (node->name == "BOOL_LITERAL") return false;
    if (node->name == "HEX_INT_LITERAL") return false;
    if (node->name == "OCTAL_INT_LITERAL") return false;
    if (node->name == "BINARY_INT_LITERAL") return false;
    if (node->name == "HEX_FLOAT_LITERAL") return false;
    
    // Function calls return rvalues
    if (node->name == "FUNC_CALL") return false;
    
    // Arithmetic operations return rvalues
    if (node->name == "ADD_EXPR" || node->name == "SUB_EXPR" ||
        node->name == "MUL_EXPR" || node->name == "DIV_EXPR" ||
        node->name == "MOD_EXPR") return false;
    
    // Comparison operations return rvalues
    if (node->name == "LT_EXPR" || node->name == "GT_EXPR" ||
        node->name == "LE_EXPR" || node->name == "GE_EXPR" ||
        node->name == "EQ_EXPR" || node->name == "NEQ_EXPR") return false;
    
    // Logical operations return rvalues
    if (node->name == "LOGICAL_AND" || node->name == "LOGICAL_OR") return false;
    
    // Bitwise operations return rvalues
    if (node->name == "BIT_AND" || node->name == "BIT_OR" ||
        node->name == "BIT_XOR" || node->name == "LSHIFT_EXPR" ||
        node->name == "RSHIFT_EXPR") return false;
    
    // Postfix/Prefix operations return rvalues (result can't be modified)
    if (node->name == "POST_INC" || node->name == "POST_DEC") return false;
    if (node->name == "PRE_INC" || node->name == "PRE_DEC") return false;
    
    // Default: assume not an lvalue
    return false;
}



void SemanticAnalyzer::checkMemberAccess(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    Node* baseNode = node->children[0];      // The struct/union variable
    Node* memberNode = node->children[1];    // The member being accessed
    
    if (memberNode->name != "IDENTIFIER") return;
    
    string memberName = memberNode->lexeme;
    string baseType = getExpressionType(baseNode);
    
    // Skip if we couldn't determine base type
    if (baseType.empty()) return;
    
    // Determine if this is arrow (->) or dot (.) access based on grammar
    bool isArrowAccess = (node->name == "PTR_MEMBER_ACCESS");
    bool isDotAccess = (node->name == "MEMBER_ACCESS");
    
    // RULE 1: Arrow access (PTR_MEMBER_ACCESS) requires pointer type
    if (isArrowAccess && !isPointerType(baseType)) {
        addError("Member access with '->' requires pointer type, but got '" + baseType + "'");
        return;
    }
    
    // RULE 2: Dot access (MEMBER_ACCESS) requires non-pointer struct/union type
    if (isDotAccess && isPointerType(baseType)) {
        addError("Member access with '.' requires non-pointer type, but got pointer '" + baseType + "' (use '->' instead)");
        return;
    }
    
    // Remove pointer level if arrow access
    string structType = baseType;
    if (isArrowAccess && isPointerType(baseType)) {
        // Remove the trailing '*'
        structType = baseType.substr(0, baseType.length() - 1);
    }
    
    // Remove "struct " prefix if present (e.g., "struct Point" -> "Point")
    if (structType.find("struct ") == 0) {
        structType = structType.substr(7);
    }
    
    // Look up the struct/union type in symbol table
    Symbol* structSym = symbolTable.lookup(structType);
    
    if (!structSym) {
        addError("Unknown struct/union type '" + structType + "'");
        return;
    }
    
    // RULE 3: Base must be a struct or union type
    if (!structSym->isStruct) {
        addError("Member access requires struct or union type, but got '" + structType + "'");
        return;
    }
    
    // RULE 4: Check if the member exists in the struct/union
    if (structSym->structMembers.find(memberName) == structSym->structMembers.end()) {
        addError("Struct '" + structType + "' has no member named '" + memberName + "'");
        return;
    }
    
    // Success - member access is valid
    cout << "DEBUG: Valid member access: " << structType 
         << (isArrowAccess ? "->" : ".") << memberName 
         << " [type: " << structSym->structMembers[memberName] << "]\n";
}


bool SemanticAnalyzer::allPathsReturn(Node* node) {
    if (!node) return false;
    
    // Base case: if this node is a return statement, this path returns
    if (node->name == "RETURN_STMT") {
        return true;
    }
    
    
    
    
   // If-else statements: both branches must return
if (node->name == "IF_ELSE_STMT") {
    // IF_ELSE_STMT structure:
    // [0] = condition (EXPR_LIST)
    // [1] = if body (COMPOUND_STMT)
    // [2] = else body (COMPOUND_STMT or IF_STMT for else-if)
    
    if (node->children.size() >= 3) {
        bool ifReturns = allPathsReturn(node->children[1]);
        bool elseReturns = allPathsReturn(node->children[2]);
        
        return ifReturns && elseReturns;
    }
    
    return false;  // Malformed if-else
}

// If statement WITHOUT else: cannot guarantee return
if (node->name == "IF_STMT") {
    // IF_STMT structure (no else branch):
    // [0] = condition (EXPR_LIST)
    // [1] = if body (COMPOUND_STMT)
    
    // Since there's no else, we can't guarantee all paths return
    return false;
} 
    
    
  
    // Compound statements: check if any statement returns
if (node->name == "COMPOUND_STMT" || node->name == "BLOCK_ITEM_LIST") {
    for (auto child : node->children) {
        if (allPathsReturn(child)) {
            return true;  // Found a return in this block
        }
    }
    return false;
}
 
    
    // Switch statements: all cases must return (including default)
    if (node->name == "SWITCH_STMT") {
        bool hasDefault = false;
        bool allCasesReturn = true;
        
        for (auto child : node->children) {
            if (child->name == "CASE_STMT") {
                if (!allPathsReturn(child)) {
                    allCasesReturn = false;
                }
            }
            else if (child->name == "DEFAULT_STMT") {
                hasDefault = true;
                if (!allPathsReturn(child)) {
                    allCasesReturn = false;
                }
            }
        }
        
        return hasDefault && allCasesReturn;
    }
    
    // For other nodes, check children recursively
    for (auto child : node->children) {
        if (allPathsReturn(child)) {
            return true;
        }
    }
    
    return false;
}




void SemanticAnalyzer::processTypedef(Node* node) {
    if (!node) return;
    
    string baseType;
    string aliasName;
    int pointerDepth = 0;
    bool isArray = false;
    vector<int> arrayDims;
    
    // Extract base type from DECL_SPECIFIERS
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
            baseType = extractTypeFromDeclSpecifiers(child);
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto declChild : child->children) {
                if (!declChild->children.empty()) {
                    Node* firstChild = declChild->children[0];
                    
                    if (firstChild->name == "IDENTIFIER") {
                        aliasName = firstChild->lexeme;
                    }
                    else if (firstChild->name == "DECLARATOR") {
                        analyzeDeclarator(firstChild, aliasName, pointerDepth, isArray, arrayDims);
                    }
                    else if (firstChild->name == "ARRAY") {
                        isArray = true;
                        arrayDims = extractArrayDimensions(firstChild, aliasName);
                    }
                }
            }
        }
    }
    
    if (aliasName.empty() || baseType.empty()) {
        addError("Invalid typedef declaration");
        return;
    }
    
    // Build the full aliased type
    string fullAliasedType = baseType;
    for (int i = 0; i < pointerDepth; i++) {
        fullAliasedType += "*";
    }
    
    cout << "DEBUG: Processing typedef: " << aliasName << " = " << fullAliasedType;
    if (isArray) {
        for (int dim : arrayDims) {
            cout << "[" << (dim == -1 ? "" : to_string(dim)) << "]";
        }
    }
    cout << "\n";
    
    // Add typedef to symbol table
    if (!symbolTable.addSymbol(aliasName, "typedef", node, false, {}, 
                               isArray, arrayDims, pointerDepth, false, false, 
                               true, fullAliasedType)) {
        addError("Redefinition of typedef '" + aliasName + "'");
    }
}

string SemanticAnalyzer::resolveTypedef(const string& type) {
    // Check if this type is a typedef alias
    Symbol* sym = symbolTable.lookup(type);
    
    if (sym && sym->isTypedef) {
        // Recursively resolve in case of chained typedefs
        return resolveTypedef(sym->aliasedType);
    }
    
    return type;  // Not a typedef, return as-is
}


void SemanticAnalyzer::checkStaticKeyword(Node* declSpecNode, bool& hasStatic, bool& hasTypeSpec) {
    if (!declSpecNode) return;
    
    hasStatic = false;
    hasTypeSpec = false;
    int staticCount = 0;
    vector<string> storageClasses;
    
    // Recursively collect storage classes and type specifiers
    function<void(Node*)> collectInfo = [&](Node* node) {
        if (!node) return;
        
        for (auto child : node->children) {
            // Check for both STORAGE_CLASS_SPECIFIER wrapper and direct STATIC nodes
            if (child->name == "STORAGE_CLASS_SPECIFIER") {
                string storageClass = child->lexeme;
                storageClasses.push_back(storageClass);
                
                if (storageClass == "static") {
                    staticCount++;
                    hasStatic = true;
                }
            }
            else if (child->name == "STATIC") {
                // Direct STATIC node (as seen in your AST)
                storageClasses.push_back("static");
                staticCount++;
                hasStatic = true;
            }
            else if (child->name == "AUTO") {
                storageClasses.push_back("auto");
            }
            else if (child->name == "TYPEDEF") {
                storageClasses.push_back("typedef");
            }
            else if (child->name == "TYPE_SPECIFIER") {
                if (!child->lexeme.empty()) {
                    hasTypeSpec = true;
                }
            }
            else if (child->name == "STRUCT_OR_UNION_SPECIFIER") {
                hasTypeSpec = true;
            }
            else if (child->name == "ENUM_SPECIFIER") {
                hasTypeSpec = true;
            }
            else if (child->name == "DECL_SPECIFIERS") {
                collectInfo(child);  // Recurse into nested DECL_SPECIFIERS
            }
        }
    };
    
    collectInfo(declSpecNode);
    cout << "DEBUG: staticCount = " << staticCount << ", hasStatic = " << hasStatic << "\n";

    // ✅ CHECK #1: Duplicate static (HIGHEST PRIORITY)
    if (staticCount > 1) {
        addError("Duplicate 'static' storage class specifier");
        return;  // Return immediately
    }
    
    // ✅ CHECK #2: Conflicting storage classes
    if (storageClasses.size() > 1) {
        set<string> uniqueClasses(storageClasses.begin(), storageClasses.end());
        
        // Only check if we have different storage classes
        if (uniqueClasses.size() > 1) {
            // Check all combinations of conflicts
            if (uniqueClasses.count("static") && uniqueClasses.count("auto")) {
                addError("Cannot combine 'static' and 'auto' storage classes");
                
            }
            else if (uniqueClasses.count("static") && uniqueClasses.count("typedef")) {
                addError("Cannot combine 'static' and 'typedef' storage classes");
                
            }
            else if (uniqueClasses.count("auto") && uniqueClasses.count("typedef")) {
                addError("Cannot combine 'auto' and 'typedef' storage classes");
            }
        }
    }
}

void SemanticAnalyzer::checkTypeSpecifier(Node* typeSpecNode) {
    if (!typeSpecNode || typeSpecNode->name != "TYPE_SPECIFIER") return;
    
    string typeName = typeSpecNode->lexeme;
    
    // Skip built-in types
    if (typeName == "int" || typeName == "float" || typeName == "double" || 
        typeName == "char" || typeName == "void" || typeName == "short" || 
        typeName == "long" || typeName == "signed" || typeName == "unsigned" ||
        typeName == "bool") {
        return;  // Valid built-in type
    }
    
    // Check if it's a valid typedef, struct, or enum
    Symbol* typeSym = symbolTable.lookup(typeName);
    
    if (!typeSym) {
        addError("Unknown type '" + typeName + "'");
        return;
    }
    
    // Verify it's actually a type (typedef, struct, or enum)
    if (!typeSym->isTypedef && !typeSym->isStruct && !typeSym->isEnum) {
        addError("'" + typeName + "' does not name a type");
    }
}
