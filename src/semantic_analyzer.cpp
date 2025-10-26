#include "semantic_analyzer.h"
#include <iostream>
#include<functional>
#include<set>
#include<unordered_set>
#include<stdexcept>

using namespace std;

SemanticAnalyzer::SemanticAnalyzer(SymbolTable& symTab) 
    : symbolTable(symTab), currentFunctionReturnType(""), currentFunctionName(""), loopDepth(0), switchDepth(0) {
    currentFunctionLabels.clear();
    pendingGotoStatements.clear();
    enumConstants.clear();
}
// Main entry
bool SemanticAnalyzer::buildSymbolTable(Node* root) {
    if (!root) return false;

      cout << "DEBUG: Before enterScope, active scopes = " << symbolTable.getCurrentScopeLevel() << "\n";
        // Ensure a global scope is available for all top-level declarations/definitions
        symbolTable.enterScope();  // Global scope
    enumConstants.clear();
    

    cout << "\n=== Building Symbol Table ===\n";
    traverseAST(root);
   
    // Close the global scope created above
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
    cout << "=== INSIDE DECLARATION BLOCK ===\n";
   if (isFunctionPrototype(node)) {
            processFunctionPrototype(node);
            return;  // Don't process as variable declaration
        }
    // NEW: Check if this is a typedef declaration
    bool isTypedefDecl = false;
    for (auto child : node->children) {
        cout << "  Child: " << child->name << "\n";
        
        // Check for TYPEDEF node directly in DECLARATION children
        if (child->name == "TYPEDEF") {
            isTypedefDecl = true;
            cout << "  → Found TYPEDEF as direct child!\n";
            break;
        }
        
        // Check inside DECL_SPECIFIERS for TYPEDEF or STORAGE_CLASS_SPECIFIER
        if (child->name == "DECL_SPECIFIERS") {
            for (auto specChild : child->children) {
                cout << "    DECL_SPECIFIERS child: " << specChild->name << "\n";
                
                // Check for TYPEDEF node inside DECL_SPECIFIERS
                if (specChild->name == "TYPEDEF") {
                    isTypedefDecl = true;
                    cout << "  → Found TYPEDEF inside DECL_SPECIFIERS!\n";
                    break;
                }
                
                // Also check for storage class specifier with "typedef"
                if (specChild->name == "STORAGE_CLASS_SPECIFIER" && specChild->lexeme == "typedef") {
                    isTypedefDecl = true;
                    cout << "  → Found STORAGE_CLASS_SPECIFIER typedef!\n";
                    break;
                }
            }
        }
    }
    
    if (isTypedefDecl) {
        cout << "DEBUG: Calling processTypedef!\n";
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
    // ✅ FIX: Only process if it's actually a definition
    if (isEnumDefinition(declChild)) {
        processEnum(declChild);
        isStructOrEnumDef = true;
    }
    // If it's just a reference, let it fall through to processVariable
    break;
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
    else if (node->name == "PRINTF") {
    checkPrintfStatement(node);
}

// Check scanf statements
else if (node->name == "SCANF") {
    checkScanfStatement(node);
}
    // Check if statement conditions
// Check if statement conditions
else if (node->name == "IF_STMT" || node->name == "IF_ELSE_STMT") {
    if (!node->children.empty()) {
        checkConditionExpression(node->children[0]);
    }
}
// Check return statements
else if (node->name == "RETURN_STMT") {
    checkReturnStatement(node);
}


else if (node->name == "WHILE_STMT" || node->name == "DO_WHILE_STMT" || 
         node->name == "FOR_STMT_2" || node->name == "FOR_STMT_3" || 
         node->name == "FOR_RANGE_STMT" || node->name == "UNTIL_STMT") {
    loopDepth++;

    // Handle loops, ensuring correct scoping for for-loop declarations
    if (node->name == "WHILE_STMT") {
        if (!node->children.empty()) {
            checkConditionExpression(node->children[0]);
        }
        for (auto child : node->children) {
            traverseAST(child);
        }
        loopDepth--;
        return;
    }
    else if (node->name == "DO_WHILE_STMT") {
        for (auto child : node->children) {
            traverseAST(child);
        }
        if (node->children.size() > 1) {
            checkConditionExpression(node->children[1]);
        }
        loopDepth--;
        return;
    }
    else if (node->name == "FOR_STMT_3") {
        // C: a declaration in for-init introduces a new scope for the entire for-statement
        symbolTable.enterScope();
        // Expect children: [0]=DECLARATION, [1]=condition, [2]=update, [3]=body
        if (!node->children.empty()) {
            // Process init declaration first so its symbols are visible to cond/update/body
            traverseAST(node->children[0]);
        }
        if (node->children.size() > 1) {
            checkConditionExpression(node->children[1]);
        }
        // Traverse remaining parts (condition itself, update, body)
        for (size_t idx = 1; idx < node->children.size(); ++idx) {
            traverseAST(node->children[idx]);
        }
        symbolTable.exitScope();
        loopDepth--;
        return;
    }
    else if (node->name == "FOR_STMT_2") {
        // No new scope needed if init is an expression
        if (node->children.size() > 1) {
            checkConditionExpression(node->children[1]);
        }
        for (auto child : node->children) {
            traverseAST(child);
        }
        loopDepth--;
        return;
    }
    else {
        // FOR_RANGE_STMT, UNTIL_STMT: validate condition if present, then default traversal
        if (node->name == "FOR_RANGE_STMT") {
            if (!node->children.empty()) {
                // Typically, child[0] would be the range expression; if it represents a condition, validate
                checkConditionExpression(node->children[0]);
            }
        }
        if (node->name == "UNTIL_STMT") {
            if (!node->children.empty()) {
                // Assume first child is the condition for UNTIL
                checkConditionExpression(node->children[0]);
            }
        }
        for (auto child : node->children) {
            traverseAST(child);
        }
        loopDepth--;
        return;
    }
}


else if (node->name == "SWITCH_STMT") {
    // Check switch expression type BEFORE traversing children
    checkSwitchStatement(node);
    
    switchDepth++;
    for (auto child : node->children) {
        traverseAST(child);
    }
    switchDepth--;
    return;
}

else if (node->name == "BREAK_STMT") {
    if (loopDepth == 0 && switchDepth == 0) {
        addError("'break' statement not within loop or switch");
    }
}

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

// Handle labeled statements
else if (node->name == "LABELED_STMT") {
    processLabel(node);
}
// Handle goto statements
else if (node->name == "GOTO_STMT") {
    processGoto(node);
}

    // Continue traversal for other children
    for (auto child : node->children) {
        traverseAST(child);
    }
}
void SemanticAnalyzer::processFunction(Node* node) {
    if (!node) return;
Node *nodee=NULL;
    string funcName;
    string returnType;
    vector<string> paramTypes;
    Node* funcDeclNode = nullptr;
    Node* funcDeclContainer = nullptr; // The DECLARATOR or FUNCTION_DECL node holding the function declarator
    int returnPtrDepth = 0;            // Number of '*' on the function return type declarator
    bool hasStatic = false;
    bool hasTypeSpec = false;

    // Extract function info from FUNCTION_DEFINITION children
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
        // FIRST: Check for static keyword and type specifier presence
            checkStaticKeyword(child, hasStatic, hasTypeSpec);
            returnType = extractTypeFromDeclSpecifiers(child);
            for (auto specChild : child->children) {
                if (specChild->name == "TYPE_SPECIFIER") {
                    checkTypeSpecifier(specChild);
                }
            }
        }
        else if (child->name == "FUNCTION_DECL" || child->name == "DECLARATOR") {
            // Helper: find the deepest FUNCTION_DECL node inside this subtree
            std::function<Node*(Node*)> findFuncDecl = [&](Node* n) -> Node* {
                if (!n) return nullptr;
                if (n->name == "FUNCTION_DECL") return n;
                for (auto gc : n->children) {
                    Node* res = findFuncDecl(gc);
                    if (res) return res;
                }
                return nullptr;
            };

            Node* found = (child->name == "FUNCTION_DECL") ? child : findFuncDecl(child);
            if (found) {
                funcDeclNode = found;
                funcDeclContainer = child; // Save the root where we found the function declarator
                // Find the IDENTIFIER node within the FUNCTION_DECL subtree
                std::function<Node*(Node*)> findIdent = [&](Node* n) -> Node* {
                    if (!n) return nullptr;
                    if (n->name == "IDENTIFIER") return n;
                    for (auto gc : n->children) {
                        Node* res = findIdent(gc);
                        if (res) return res;
                    }
                    return nullptr;
                };
                Node* id = findIdent(funcDeclNode);
                if (id) {
                    nodee = id;
                    funcName = id->lexeme;
                }

                // Count pointer depth contributed by declarator prefix (e.g., char* func())
                if (funcDeclContainer) {
                    std::function<int(Node*)> countPointerStars = [&](Node* node) -> int {
                        if (!node) return 0;
                        int total = (node->name == "POINTER") ? 1 : 0;
                        for (auto childPtr : node->children) {
                            total += countPointerStars(childPtr);
                        }
                        return total;
                    };

                    std::function<int(Node*, int)> findPointerDepth =
                        [&](Node* current, int depthAccum) -> int {
                            if (!current) return -1;
                            if (current == funcDeclNode) {
                                return depthAccum;
                            }

                            int workingDepth = depthAccum;
                            for (auto child : current->children) {
                                if (child == funcDeclNode) {
                                    return workingDepth;
                                }

                                if (child->name == "POINTER") {
                                    int foundInPointer = findPointerDepth(child, workingDepth + 1);
                                    if (foundInPointer != -1) {
                                        return foundInPointer;
                                    }
                                    workingDepth += countPointerStars(child);
                                } else {
                                    int found = findPointerDepth(child, workingDepth);
                                    if (found != -1) {
                                        return found;
                                    }
                                }
                            }
                            return -1;
                        };

                    int depth = findPointerDepth(funcDeclContainer, 0);
                    if (depth >= 0) {
                        returnPtrDepth = depth;
                    }
                }
            }
        }
    }
// Build the full return type by applying any '*' depth from the declarator
std::string fullReturnType = returnType;
for (int i = 0; i < returnPtrDepth; ++i) fullReturnType += "*";
cout << "DEBUG processFunction: func='" << (funcName.empty() ? string("<unnamed>") : funcName)
    << "' baseReturn='" << returnType << "' ptrDepth=" << returnPtrDepth
    << " fullReturn='" << fullReturnType << "'\n";

currentFunctionName = funcName;
currentFunctionReturnType = fullReturnType;

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

    if (funcName.empty()) {
        addError("Function name missing");
        return;
    }

    // Extract parameter types
    paramTypes = extractFunctionParameters(funcDeclNode);

 
// Add function to symbol table with parameters
    bool added = symbolTable.addSymbol(funcName, fullReturnType, nodee, true, paramTypes);
    
  
    if (!added) {
        // Check if it's an incompatible redeclaration
        Symbol* existing = symbolTable.lookupCurrentScope(funcName);
        if (existing && existing->isFunction) {
            if (existing->paramTypes != paramTypes || existing->type != returnType) {
                addError("Conflicting declaration of function '" + funcName + "'");
            }
            // If parameters match, it's a valid redeclaration (no error)
        } else if (existing) {
            addError("Redeclaration of '" + funcName + "' as different kind of symbol");
        }
    } else {
    // ✅ ADDED SUCCESSFULLY - NOW LOOKUP TO GET THE SYMBOL
    Symbol* sym = symbolTable.lookupCurrentScope(funcName);
    if (sym && nodee) {
        nodee->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }
}

for (auto child : node->children) {
    if (child->name == "COMPOUND_STMT") {
        processBlock(child, true, funcDeclNode);
        
        // CHECK: Non-void functions must return on all paths
        if (returnType != "void") {
            if (!allPathsReturn(child)) {
                addError("Function '" + funcName + "' with non-void return type '" + 
                         fullReturnType + "' does not return a value on all control paths");
            }
        }
        
        // Verify all goto statements have valid labels
        verifyGotoLabels();
        
        // Clear labels for next function
        currentFunctionLabels.clear();
        pendingGotoStatements.clear();
        
        currentFunctionName = "";
        currentFunctionReturnType = "";
        return;
    }
}

// Clear labels if function body wasn't found
currentFunctionLabels.clear();
pendingGotoStatements.clear();
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
    
     if (child->name == "TYPE_NAME") {
            string type = child->lexeme;
            cout << "  DEBUG extractType: Found TYPE_NAME = '" << type << "'\n";
            
            if (!type.empty()) {
                // Resolve the typedef to get the actual type
                string resolvedType = resolveTypedef(type);
                cout << "  DEBUG extractType: Resolved '" << type << "' to '" << resolvedType << "'\n";
                return resolvedType;
            }
        }
        // Handle struct/union specifiers
      
        else if (child->name == "STRUCT_OR_UNION_SPECIFIER") {
            // Determine if struct or union
            bool isUnion = false;
            string typeName;
            
            for (auto structChild : child->children) {
                if (structChild->name == "UNION") {
                    isUnion = true;
                }
                else if (structChild->name == "IDENTIFIER") {
                    typeName = structChild->lexeme;
                }
            }
            
            string typePrefix = isUnion ? "union" : "struct";
            if (!typeName.empty()) {
                return typePrefix + " " + typeName;  // "struct Point" or "union Data"
            }
            return typePrefix;  // Anonymous
        }
        else if (child->name == "ENUM_SPECIFIER") {
    // Extract enum name as type
    for (auto enumChild : child->children) {
        if (enumChild->name == "IDENTIFIER") {
            string enumType = "enum " + enumChild->lexeme;
            cout << "  DEBUG extractType: Found ENUM_SPECIFIER = '" << enumType << "'\n";
            return enumType;
        }
    }
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
bool hasStatic = false;
    bool hasTypeSpec = false;
    string varType;
     int pointerDepth = 0;
     bool hasStorageError=false;
     Node* nodee=NULL;
    // Extract type
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
         checkStaticKeyword(child, hasStatic, hasTypeSpec);
            if (hasStorageError) {
                return;  // Don't process this declaration further
            }
            varType = extractTypeFromDeclSpecifiers(child);
            
            // Check type specifiers
            for (auto specChild : child->children) {
                if (specChild->name == "TYPE_SPECIFIER") {
                    checkTypeSpecifier(specChild);
                }
            }
            
            
            cout<<"VARRRTYPE"<<varType<<endl;
              // Count trailing '*'
    int starCount = 0;
    for (int i = static_cast<int>(varType.size()) - 1; i >= 0; --i) {
        if (varType[i] == '*') {
            starCount++;
        } else {
            break;
        }
    }

    // Remove trailing '*'s from the type string
    if (starCount > 0) {
        varType = varType.substr(0, varType.size() - starCount);
    }
    
cout<<"VARRRTYPE"<<varType<<endl;
    pointerDepth += starCount; 
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto declChild : child->children) {
                if (!declChild->children.empty()) {
                    Node* firstChild = declChild->children[0];
                    int pointerDepthh=0;
                    string varName;
                    bool isArray = false;
                    vector<int> arrayDimensions; 
                   nodee = nullptr; 
                    // Direct identifier: int x;
                    if (firstChild->name == "IDENTIFIER") {
                        nodee=firstChild;
                        varName = firstChild->lexeme;
                    }
                    // Array: int a[10]; or int matrix[3][4];
                    else if (firstChild->name == "ARRAY") {
                        isArray = true;
                        arrayDimensions = extractArrayDimensions(firstChild, varName);
                         nodee = findIdentifierInArray(firstChild);
                    }
                    else if (firstChild->name == "DECLARATOR") {
                        analyzeDeclarator(firstChild, varName, pointerDepthh, isArray, arrayDimensions);
                        nodee = findIdentifierInDeclarator(firstChild); 
           
                    }
                    pointerDepth=pointerDepth+pointerDepthh;
                     cout<<"VARRRTjhYPE"<<pointerDepth<<endl;
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
                    
                        cout << "DEBUG: Adding variableee " << varName << " [" << varType;
                        
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
                        
                        if (!symbolTable.addSymbol(varName, varType, nodee, false, {},  isArray, arrayDimensions, pointerDepth)) {
                            addError("Redeclaration of variable: " + varName);
                        }
                        else {
    // ✅ ADDED SUCCESSFULLY - NOW LOOKUP TO GET THE SYMBOL
    Symbol* sym = symbolTable.lookupCurrentScope(varName);
    if (sym && nodee) {
        nodee->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        
        cout << "DEBUG: Attached symbollll " <<  sym->name<< " to AST node" << endl;
    }
}
                    }
                }
            }
        }
    }
}
Node* SemanticAnalyzer::findIdentifierInDeclarator(Node* declaratorNode) {
    if (!declaratorNode) return nullptr;
    
    // Traverse DECLARATOR to find the IDENTIFIER
    if (declaratorNode->name == "IDENTIFIER") {
        return declaratorNode;
    }
    
    for (auto child : declaratorNode->children) {
        Node* result = findIdentifierInDeclarator(child);
        if (result) return result;
    }
    
    return nullptr;
}

Node* SemanticAnalyzer::findIdentifierInArray(Node* arrayNode) {
    if (!arrayNode) return nullptr;
    
    // Traverse nested ARRAY nodes to find the IDENTIFIER
    Node* current = arrayNode;
    while (current && current->name == "ARRAY") {
        if (!current->children.empty()) {
            Node* firstChild = current->children[0];
            if (firstChild->name == "IDENTIFIER") {
                return firstChild;  // Found it!
            }
            current = firstChild;  // Go deeper for multi-dimensional
        } else {
            break;
        }
    }
    
    return nullptr;
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
    
    for (auto child : funcDeclNode->children) {
        if (child->name == "PARAM_TYPE_LIST") {
            for (auto paramTypeListChild : child->children) {
                if (paramTypeListChild->name == "PARAM_LIST") {
                    for (auto paramDecl : paramTypeListChild->children) {
                        if (paramDecl->name == "PARAM_DECL") {
                            string baseType;
                            int pointerDepth = 0;
                            bool isArray = false;
                            vector<int> arrayDims;
                            string paramName;
                            
                            for (auto paramChild : paramDecl->children) {
                                if (paramChild->name == "DECL_SPECIFIERS" || 
                                    paramChild->name == "declaration_specifiers") {
                                    baseType = extractTypeFromDeclSpecifiers(paramChild);
                                }
                                else if (paramChild->name == "DECLARATOR") {
                                    analyzeDeclarator(paramChild, paramName, pointerDepth, isArray, arrayDims);
                                }
                                else if (paramChild->name == "ARRAY") {
                                    // ✅ Handle array parameters
                                    isArray = true;
                                    arrayDims = extractArrayDimensions(paramChild, paramName);
                                }
                            }
                            
                            // ✅ Build full type
                            string fullType = baseType;
                            for (int i = 0; i < pointerDepth; i++) {
                                fullType += "*";
                            }
                            if (isArray) {
                                // For function parameters, arrays decay to pointers
                                fullType += "*";  // char argv[] → char*
                            
                            if (arrayDims.size()>1) {
                                // For function parameters, first dimension decays to pointer
                                // But preserve remaining dimensions
                                
                                // Add remaining dimensions first
                                for (size_t i = 1; i < arrayDims.size(); i++) {
                                    if (arrayDims[i] == -1) {
                                        fullType += "[]";
                                    } else {
                                        fullType += "[" + to_string(arrayDims[i]) + "]";
                                    }
                                }
                                
                               
                            } }
                            if (!baseType.empty()) {
                                paramTypes.push_back(fullType);
                                cout << "DEBUG: Function param type: " << fullType << "\n";
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
    Node * nodee=NULL;
    for (auto child : funcDeclNode->children) {
        if (child->name == "PARAM_TYPE_LIST") {
            for (auto paramTypeListChild : child->children) {
                if (paramTypeListChild->name == "PARAM_LIST") {
                    for (auto paramDecl : paramTypeListChild->children) {
                        if (paramDecl->name == "PARAM_DECL") {
                            string baseType;
                            int pointerDepth = 0;
                            bool isArray = false;
                            vector<int> arrayDims;
                            string paramName;
                            nodee=NULL;
                            for (auto paramChild : paramDecl->children) {
                                if (paramChild->name == "DECL_SPECIFIERS" || 
                                    paramChild->name == "declaration_specifiers") {
                                    baseType = extractTypeFromDeclSpecifiers(paramChild);
                                }
                                else if (paramChild->name == "DECLARATOR") {
                                    analyzeDeclarator(paramChild, paramName, pointerDepth, isArray, arrayDims);
                                     nodee = findIdentifierInDeclarator(paramChild);  // ✅ FIX!
                          
                                }
                                else if (paramChild->name == "ARRAY") {
                                     isArray=true;
                                    arrayDims = extractArrayDimensions(paramChild, paramName);
                                     nodee = findIdentifierInArray(paramChild);  // ✅ FIX!
                           
                                }
                                else if (paramChild->name == "IDENTIFIER") {
                                nodee=paramChild;
                                    paramName = paramChild->lexeme;
                                }
                                else if (paramChild->name == "&") {
                                    // Handle reference parameters
                                }
                            }
                            
                            if (isArray) {
                                pointerDepth += 1;  // array → pointer conversion
                                 
    if (arrayDims.size() > 1) {
        // Multi-dimensional: keep subsequent dimensions
        // Remove first dimension, keep the rest
        vector<int> newDims(arrayDims.begin() + 1, arrayDims.end());
        arrayDims = newDims;
        isArray = true;  // Still an array (but with fewer dimensions)
    } else {
        // Single dimension: becomes pure pointer
        isArray = false;
        arrayDims.clear();
    }
                            }
                            
                            if (!paramName.empty() && !baseType.empty()) {
                                // ✅ Store with proper array/pointer info
                                if (!symbolTable.addSymbol(paramName, baseType, nodee, false, {}, 
                                                          isArray, arrayDims, pointerDepth)) {
                                    addError("Duplicate parameter name: " + paramName);
                                } else {
                             
    Symbol* sym = symbolTable.lookupCurrentScope(paramName);
    if (sym && nodee) {
        nodee->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }

                                    cout << "DEBUG: Added parameter " << paramName << " [" << baseType;
                                    for (int i = 0; i < pointerDepth; i++) cout << "*";
                                    if (isArray) {
                                        for (int dim : arrayDims) {
                                            cout << "[" << (dim == -1 ? "" : to_string(dim)) << "]";
                                        }
                                    }
                                    cout << "]\n";
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

// Check argument types (strict for function calls: disallow narrowing)
for (size_t i = 0; i < actualArgs; i++) {
    if (funcSym->paramTypes.size() <= i) {
        break;  // Already reported count mismatch above
    }
    string argType = getExpressionType(argNodes[i]);
    string paramType = funcSym->paramTypes[i];

    if (!areFunctionArgCompatible(argType, paramType)) {
        addError("Type mismatch for argument " + to_string(i + 1) +
                 " of function '" + funcName + "': expected '" + paramType +
                 "', but got '" + argType + "'");
    }
}
}

// Strict function-argument compatibility policy:
// - Exact match: OK
// - Numeric vs numeric:
//   - Disallow float/double -> integer params (narrowing)
//   - Disallow double -> float (narrowing)
//   - Allow others (e.g., int->float/double, int->long, float->double)
// - Otherwise, fall back to the general assignment compatibility
bool SemanticAnalyzer::areFunctionArgCompatible(const string& argType, const string& paramType) {
    if (argType.empty() || paramType.empty()) return true; // unknown types: skip
    if (argType == paramType) return true;

    auto isFloatType = [](const string& t) {
        return t == "float" || t == "double";
    };

    // Both numeric types?
    if (isNumericType(argType) && isNumericType(paramType)) {
        bool argIsFloat = isFloatType(argType);
        bool paramIsFloat = isFloatType(paramType);
        bool argIsInt = isIntegerType(argType);
        bool paramIsInt = isIntegerType(paramType);

        // Disallow any float/double -> integer parameter
        if (argIsFloat && paramIsInt) return false;

        // Disallow double -> float
        if (argType == "double" && paramType == "float") return false;

        // Otherwise permit (includes int->float/double, int->long, float->double, etc.)
        return true;
    }

    // Non-numeric cases: reuse the general compatibility (assignment-like)
    return areTypesCompatible(paramType, argType, "=");
}

void SemanticAnalyzer::checkIdentifier(Node* node) {
    if (!node || node->lexeme.empty()) return;
    
    Symbol* sym = symbolTable.lookup(node->lexeme);
    if (!sym) {
        addError("Undeclared identifier: '" + node->lexeme + "'");
    }
    else {
        // ✅ ATTACH SYMBOL TO AST NODE (this was missing!)
        node->symbol = sym;
        cout << "DEBUG: Attached symbol " << sym->name << " to identifier usage node" << endl;
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
        // NEW: If parent is LABELED_STMT, this is a label definition (not a variable reference)
    if (parent->name == "LABELED_STMT") {
        return true;  // Label definition, not identifier usage
    }
    
    // NEW: If parent is GOTO_STMT, this is a label reference (handled separately by processGoto)
    if (parent->name == "GOTO_STMT") {
        return true;  // Label reference, don't check as undeclared identifier
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
    
    // More precise check: Only treat as declaration context if this IDENTIFIER
    // is within the LHS (declarator) of an initializer inside a declaration.
    // Identifiers used in the RHS of an initializer should be treated as usages.
    Node* anc = node;
    Node* nearestAssign = nullptr;
    Node* nearestDecl = nullptr;
    while (anc) {
        if (!nearestDecl && anc->name == "DECLARATION") nearestDecl = anc;
        if (!nearestAssign && anc->name == "ASSIGN_EXPR") nearestAssign = anc;
        anc = anc->parent;
    }
    if (nearestDecl) {
        // If inside an ASSIGN_EXPR, check whether we're on the LHS subtree
        if (nearestAssign && nearestAssign->children.size() >= 1) {
            Node* lhsSubtree = nearestAssign->children[0];
            // Walk up from node to see if we hit the LHS subtree root first
            Node* cur = node;
            while (cur && cur != nearestAssign) {
                if (cur == lhsSubtree) {
                    return true;  // Part of declarator (LHS)
                }
                cur = cur->parent;
            }
            return false; // Inside declaration but not on LHS → usage context
        }
        // No ASSIGN_EXPR ancestor: treat as declaration context only if it's
        // within a DECLARATOR or ARRAY declarator subtree
        anc = node;
        while (anc && anc != nearestDecl) {
            if (anc->name == "DECLARATOR" || anc->name == "ARRAY") {
                return true;
            }
            anc = anc->parent;
        }
        // Otherwise, it's likely a usage within the declaration statement
        return false;
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
        // Type checking for non-void returns
        if (hasReturnValue) {
            // Unwrap EXPR_LIST to get the actual returned expression
            Node* expr = node->children[0];
            if (expr && expr->name == "EXPR_LIST" && !expr->children.empty()) {
                expr = expr->children[0];
            }

            std::string retExprType = getExpressionType(expr);
            std::string funcRetType = currentFunctionReturnType;

            // Special rule: returning integer to pointer is only allowed for literal 0 (NULL)
            if (isPointerType(funcRetType) && isIntegerType(retExprType)) {
                bool isNullConstant = (expr && expr->name == "INTEGER_CONSTANT" && expr->lexeme == "0");
                if (!isNullConstant) {
                    addError("Return type mismatch in function '" + currentFunctionName +
                             "': expected '" + funcRetType + "', but got '" + retExprType + "'");
                    return;
                }
                // else OK: returning 0 to pointer type
            } else if (!areTypesCompatible(funcRetType, retExprType, "=")) {
                addError("Return type mismatch in function '" + currentFunctionName +
                         "': expected '" + funcRetType + "', but got '" + retExprType + "'");
            }
        }
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

namespace {

static inline bool isHex(char c) {
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static inline int hexVal(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
    return 0;
}

long long decodeCharLiteral(const std::string& literal) {
    if (literal.size() < 3) return 0;

    // Find first single quote (handles optional prefixes like L, u, U)
    size_t firstQuote = literal.find('\'');
    if (firstQuote == std::string::npos) {
        // Fallback: try double quote (shouldn't happen for char, but be defensive)
        firstQuote = literal.find('"');
    }
    if (firstQuote == std::string::npos || firstQuote + 1 >= literal.size()) return 0;

    // Extract content between quotes (lexer guarantees exactly one CHAR_CONTENT)
    size_t pos = firstQuote + 1;
    if (literal[pos] != '\\') {
        return static_cast<unsigned char>(literal[pos]);
    }

    // Escape sequence decoding
    if (pos + 1 >= literal.size()) return 0;
    char e = literal[pos + 1];
    // Hex: \x[1+ hex]
    if (e == 'x') {
        size_t i = pos + 2;
        unsigned int val = 0;
        bool any = false;
        while (i < literal.size() && isHex(literal[i])) {
            any = true;
            val = (val << 4) | (unsigned)hexVal(literal[i]);
            ++i;
        }
        return any ? static_cast<long long>(val & 0xFF) : 0;
    }
    // Unicode: \uXXXX or \UXXXXXXXX (truncate to 8-bit char semantics)
    if (e == 'u') {
        unsigned int val = 0;
        size_t i = pos + 2;
        for (int k = 0; k < 4 && i < literal.size(); ++k, ++i) {
            if (!isHex(literal[i])) return 0;
            val = (val << 4) | (unsigned)hexVal(literal[i]);
        }
        return static_cast<long long>(val & 0xFF);
    }
    if (e == 'U') {
        unsigned int val = 0;
        size_t i = pos + 2;
        for (int k = 0; k < 8 && i < literal.size(); ++k, ++i) {
            if (!isHex(literal[i])) return 0;
            val = (val << 4) | (unsigned)hexVal(literal[i]);
        }
        return static_cast<long long>(val & 0xFF);
    }
    // Octal: \[0-7]{1,3}
    if (e >= '0' && e <= '7') {
        unsigned int val = 0;
        size_t i = pos + 1;
        int count = 0;
        while (i < literal.size() && count < 3 && literal[i] >= '0' && literal[i] <= '7') {
            val = (val << 3) | (unsigned)(literal[i] - '0');
            ++i; ++count;
        }
        return static_cast<long long>(val & 0xFF);
    }

    // Standard single-char escapes
    switch (e) {
        case '\\': return '\\';
        case '\'': return '\'';
        case '"': return '"';
        case 'n': return '\n';
        case 't': return '\t';
        case 'r': return '\r';
        case '0': return '\0';
        case 'b': return '\b';
        case 'f': return '\f';
        case 'v': return '\v';
        case 'a': return '\a';
        case '?': return '?';
        default: return static_cast<unsigned char>(e);
    }
}

}

bool SemanticAnalyzer::tryEvaluateConstantExpression(Node* node, long long& value) {
    if (!node) return false;

    if (node->name == "CONSTANT_EXPR" && !node->children.empty()) {
        return tryEvaluateConstantExpression(node->children.back(), value);
    }

    if (node->name == "EXPR_LIST" && !node->children.empty()) {
        return tryEvaluateConstantExpression(node->children.back(), value);
    }

    if (node->name == "INTEGER_CONSTANT") {
        value = stoll(node->lexeme);
        return true;
    }

    if (node->name == "CHAR_LITERAL") {
        value = decodeCharLiteral(node->lexeme);
        return true;
    }

    if (node->name == "BOOL_LITERAL") {
        value = (node->lexeme == "true") ? 1 : 0;
        return true;
    }

    if (node->name == "IDENTIFIER") {
        auto it = enumConstants.find(node->lexeme);
        if (it != enumConstants.end()) {
            value = it->second;
            return true;
        }
        return false;
    }

    if (node->name == "UNARY_OP" && node->children.size() == 2) {
        Node* op = node->children[0];
        Node* operand = node->children[1];
        long long operandValue = 0;
        if (!tryEvaluateConstantExpression(operand, operandValue)) {
            return false;
        }
        if (op->name == "-") {
            value = -operandValue;
            return true;
        }
        if (op->name == "+") {
            value = operandValue;
            return true;
        }
        if (op->name == "!") {
            value = operandValue == 0;
            return true;
        }
        if (op->name == "~") {
            value = ~operandValue;
            return true;
        }
        return false;
    }

    auto evalBinary = [&](Node* lhs, Node* rhs, const std::function<long long(long long, long long)>& op) {
        long long leftVal = 0;
        long long rightVal = 0;
        if (!tryEvaluateConstantExpression(lhs, leftVal) ||
            !tryEvaluateConstantExpression(rhs, rightVal)) {
            return false;
        }
        value = op(leftVal, rightVal);
        return true;
    };

    if (node->name == "ADD_EXPR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) { return a + b; });
    }
    if (node->name == "SUB_EXPR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) { return a - b; });
    }
    if (node->name == "MUL_EXPR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) { return a * b; });
    }
    if (node->name == "DIV_EXPR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) {
                              if (b == 0) {
                                  throw std::runtime_error("Division by zero in constant expression");
                              }
                              return a / b;
                          });
    }
    if (node->name == "MOD_EXPR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) {
                              if (b == 0) {
                                  throw std::runtime_error("Modulo by zero in constant expression");
                              }
                              return a % b;
                          });
    }
    if (node->name == "BIT_AND" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) { return a & b; });
    }
    if (node->name == "BIT_OR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) { return a | b; });
    }
    if (node->name == "BIT_XOR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) { return a ^ b; });
    }
    if (node->name == "LSHIFT_EXPR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) { return a << b; });
    }
    if (node->name == "RSHIFT_EXPR" && node->children.size() == 2) {
        return evalBinary(node->children[0], node->children[1],
                          [](long long a, long long b) { return a >> b; });
    }

    return false;
}

int SemanticAnalyzer::evaluateConstantExpression(Node* node) {
    try {
        long long value = 0;
        if (tryEvaluateConstantExpression(node, value)) {
            return static_cast<int>(value);
        }
    } catch (const std::exception&) {
        return -1;
    }
    return -1;
}
void SemanticAnalyzer::checkArrayAccess(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    //Node* arrayNode = node->children[0];
    Node* indexNode = node->children[1];
    
    // Unwrap EXPR_LIST if present
    if (indexNode->name == "EXPR_LIST" && !indexNode->children.empty()) {
        indexNode = indexNode->children[0];
    }
    
    // Get the base array symbol and count total dimensions being accessed
    string arrayName;
    Symbol* sym = nullptr;
    int accessedDimensions = 0;
    
    // Traverse nested ARRAY_ACCESS to find base and count dimensions
    Node* current = node;
    while (current && current->name == "ARRAY_ACCESS") {
        accessedDimensions++;
        if (current->children.size() > 0) {
            current = current->children[0];
        } else {
            break;
        }
    }
    
    // Now current should be the base (IDENTIFIER or other expr like STRING_LITERAL)
    if (current && current->name == "IDENTIFIER") {
        arrayName = current->lexeme;
        sym = symbolTable.lookup(arrayName);
        
        if (!sym) {
            addError("Undeclared identifier: '" + arrayName + "'");
            return;
        }
        
        // Check if it's actually an array or pointer
        if (!sym->isArray && sym->pointerDepth == 0) {
            addError("Subscripted value '" + arrayName + "' is not an array or pointer");
            return;
        }

        // Simulate stepwise indexing: consume array dimensions first, then pointer levels
        int remainingArrayDims = sym->isArray ? static_cast<int>(sym->arrayDimensions.size()) : 0;
        int remainingPtrDepth = sym->pointerDepth;
        for (int i = 0; i < accessedDimensions; ++i) {
            if (remainingArrayDims > 0) {
                remainingArrayDims--;  // consume an array dimension
            } else if (remainingPtrDepth > 0) {
                remainingPtrDepth--;   // consume a pointer level (arr of pointers case)
            } else {
                addError("Too many array subscripts for '" + arrayName + 
                         "' (no remaining array dimensions or pointer levels to index)");
                return;
            }
        }
    } else if (current) {
        // Base is not an identifier (e.g., string literal). Validate it's a pointer-like expr.
        string baseType = getExpressionType(current);
        // Allow if it's a pointer OR an encoded array type like "struct B[2]"
        bool isEncodedArrayType = (baseType.find('[') != string::npos && baseType.find(']') != string::npos);
        if (!isPointerType(baseType) && !isEncodedArrayType) {
            addError("Subscripted value is not an array or pointer");
            return;
        }
        // No symbol-based bounds checks when base is not a declared array.
    }
    
    // Check index type
    string indexType = getExpressionType(indexNode);
    if (!indexType.empty() && indexType != "int" && indexType != "long" && 
        indexType != "short" && indexType != "char") {
        addError("Array subscript is not an integer (got '" + indexType + "')");
    }
    
    // Perform bounds checking for the current dimension
    if (sym && sym->isArray && !sym->arrayDimensions.empty()) {
        // Determine which dimension this is
        int currentDimIndex = accessedDimensions - 1;
        if (currentDimIndex >= 0 && currentDimIndex < static_cast<int>(sym->arrayDimensions.size())){
            int dimSize = sym->arrayDimensions[currentDimIndex];
            
            if (dimSize > 0) {  // Only check if size is known
                int indexValue;
                if (isConstantIndex(indexNode, indexValue)) {
                    // Negative index check
                    if (indexValue < 0) {
                        addError("Array index " + to_string(indexValue) + 
                                 " is negative for array '" + arrayName + 
                                 "' at dimension " + to_string(currentDimIndex + 1));
                    }
                    // Out of bounds check
                    else if (indexValue >= dimSize) {
                        addError("Array index " + to_string(indexValue) + 
                                 " is out of bounds for array '" + arrayName + 
                                 "' at dimension " + to_string(currentDimIndex + 1) +
                                 " (size is " + to_string(dimSize) + 
                                 ", valid indices: 0-" + to_string(dimSize - 1) + ")");
                    }
                }
            }
        }
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
        // Address-of requires an lvalue operand (e.g., identifier, array element, *ptr, member)
        if (!isLvalue(operandNode)) {
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
        // Disallow unary + / - on boolean explicitly
        else if (operandType == "bool") {
            addError("Invalid unary operation '" + opNode->name + "' on boolean type");
        }
    }
  
}

string SemanticAnalyzer::getExpressionType(Node* node) {
    if (!node) return "";
    // Unwrap trivial EXPR_LIST wrappers to reach the underlying expression
    while (node && node->name == "EXPR_LIST" && !node->children.empty()) {
        node = node->children[0];
    }
    
  
     if (node->name == "IDENTIFIER") {
        Symbol* sym = symbolTable.lookup(node->lexeme);
        if (!sym){ return "";}
        
        string type = resolveTypedef(sym->type);
        // Array identifiers decay to pointer-to-element in expressions.
        // If the element itself is a pointer (e.g., char* arr[]), include its pointer depth, then add one more for decay.
        if (sym->isArray) {
            for (int i = 0; i < sym->pointerDepth + 1; i++) {
                type += "*";
            }
            return type;
        }
        
        // Non-array: apply pointer depth normally
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
      if (node->name == "SIZEOF" || node->name == "SIZEOF_TYPE") {
        return "int";  // sizeof returns an integer type
    }
    // Handle array access - returns element type
    else if (node->name == "ARRAY_ACCESS") {
        if (!node->children.empty()) {
            string baseType = getExpressionType(node->children[0]);
            // Remove one level of pointer
            if (!baseType.empty() && baseType.back() == '*') {
                return baseType.substr(0, baseType.length() - 1);
            }
            // If baseType encodes an array (e.g., "struct B[2]"), strip one dimension
            size_t lb = baseType.rfind('[');
            size_t rb = baseType.rfind(']');
            if (lb != string::npos && rb != string::npos && rb > lb) {
                return baseType.substr(0, lb);
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
            Node* operandNode = node->children[1];
            // Unwrap EXPR_LIST for operand if present
            if (operandNode && operandNode->name == "EXPR_LIST" && !operandNode->children.empty()) {
                operandNode = operandNode->children[0];
            }
            string operandType = getExpressionType(operandNode);
            if (operandType.length() > 0 && operandType.back() == '*') {
                return operandType.substr(0, operandType.length() - 1);
            }
        }
        
        // Handle address-of - adds pointer level
        else if (node->children[0]->name == "&") {
          Node* operandNode = node->children[1];
          if (operandNode && operandNode->name == "EXPR_LIST" && !operandNode->children.empty()) {
              operandNode = operandNode->children[0];
          }
          string operandType = getExpressionType(operandNode);
        
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
        // Logical NOT (!) yields a boolean type regardless of operand's scalar type
        else if (node->children[0]->name == "!") {
            return "bool";
        }
        // Bitwise NOT (~) yields an integer type; prefer preserving operand's integer type
        else if (node->children[0]->name == "~") {
            // Determine the operand type and, if it's an integer type, preserve it
            Node* operandNode = node->children[1];
            if (operandNode && operandNode->name == "EXPR_LIST" && !operandNode->children.empty()) {
                operandNode = operandNode->children[0];
            }
            string opType = getExpressionType(operandNode);
            if (!opType.empty() && isIntegerType(opType)) {
                return opType;
            }
            // Fallback: treat as int if we can't determine a specific integer type
            return "int";
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
        
     
        // Remove "struct " or "union " prefix
        if (baseType.find("struct ") == 0) {
            baseType = baseType.substr(7);
        } else if (baseType.find("union ") == 0) {
            baseType = baseType.substr(6);
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
    Node * nodee=NULL;
    string structName;
    map<string, string> members;
    map<string, Node*> memberNodes; 
    vector<string> memberOrder; // preserve declaration order
    bool isUnion = false;
    
    // Check if this is a union or struct
    if (node->name == "STRUCT_OR_UNION_SPECIFIER" && !node->children.empty()) {
        Node* firstChild = node->children[0];
        if (firstChild->name == "UNION") {
            isUnion = true;
        }
    }
    for (auto child : node->children) {
        if (child->name == "IDENTIFIER") {
        nodee=child;
            structName = child->lexeme;
        }
        else if (child->name == "DECLARATION") {
            extractStructMembers(child, members, memberNodes, memberOrder);
        }
    }
    
    if (structName.empty()) {
        return; // Anonymous struct
    }
    
    if (!members.empty()) {
        symbolTable.enterScope();
        
        // Add struct members to the struct scope
        for (const auto& member : members) {
          Node* memberNode = memberNodes[member.first];  // ✅ GET CORRECT NODE!
         
            if (!symbolTable.addSymbol(member.first, member.second, memberNode)) {
                addError("Duplicate struct member: " + member.first);
            } else {
          
    // ✅ ADDED SUCCESSFULLY - NOW LOOKUP TO GET THE SYMBOL
    Symbol* sym = symbolTable.lookupCurrentScope(member.first);
    if (sym && memberNode) {
        memberNode->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }

                cout << "DEBUG: Added struct member " << member.first << " [" << member.second << "] to struct scope\n";
            }
        }
        
        // EXIT STRUCT MEMBER SCOPE (members are now in scope history)
        symbolTable.exitScope();
        
        string typeKeyword = isUnion ? "union" : "struct";
        if (symbolTable.addSymbol(structName, typeKeyword, nodee, false, {}, false, {}, 0, true, false)) {
            Symbol* sym = symbolTable.lookupCurrentScope(structName);
            
         
    if (sym && nodee) {
        nodee->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }
            if (sym) {
                sym->structMembers = members;  // Store member info
                sym->structMemberOrder = memberOrder; // preserve order
                sym->isUnion = isUnion;  // Mark if it's a union
            }
            cout << "DEBUG: Defined " << typeKeyword << " " << structName << " with " << members.size() << " member(s)\n";
        } else {
            addError("Redefinition of " + typeKeyword + " '" + structName + "'");
        }
    }
}

void SemanticAnalyzer::extractStructMembers(Node* declNode, map<string, string>& members ,map<string, Node*>& memberNodes, vector<string>& memberOrder) {
    if (!declNode || declNode->name != "DECLARATION") return;
    
    string memberType;
    vector<string> memberNames;
    vector<pair<string, Node*>> memberNamesAndNodes;  // ✅ Store both!
   
    
    for (auto child : declNode->children) {
        if (child->name == "DECL_SPECIFIERS") {
            memberType = extractTypeFromDeclSpecifiers(child);
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto initDecl : child->children) {
                if (!initDecl->children.empty()) {
                    Node* firstChild = initDecl->children[0];
                    Node* identifierNode = nullptr;
                    if (firstChild->name == "IDENTIFIER") {
                        memberNames.push_back(firstChild->lexeme);
                        identifierNode = firstChild;
                        memberNamesAndNodes.push_back({firstChild->lexeme, identifierNode});
                        memberOrder.push_back(firstChild->lexeme);
                 
                    }
                    // Handle array declarators for struct members, e.g., struct B b[2];
                    else if (firstChild->name == "ARRAY") {
                        string memberName;
                        vector<int> arrayDims = extractArrayDimensions(firstChild, memberName);
                        identifierNode = findIdentifierInArray(firstChild);
                        if (!memberName.empty()) {
                            // Encode array dimensions into type string, e.g., "struct B[2][3]"
                            string fullType = memberType;
                            for (int dim : arrayDims) {
                                fullType += "[";
                                if (dim > 0) fullType += to_string(dim);
                                fullType += "]";
                            }
                            members[memberName] = fullType;
                            memberNodes[memberName] = identifierNode;
                            memberOrder.push_back(memberName);
                            cout << "DEBUG: Found struct array member: " << memberName << " [" << fullType << "]\n";
                            continue;
                        }
                    }
                    // Handle declarators with pointers/arrays
                    else if (firstChild->name == "DECLARATOR") {
                        string memberName;
                        int pointerDepth = 0;
                        bool isArray = false;
                        vector<int> arrayDims;
                        analyzeDeclarator(firstChild, memberName, pointerDepth, isArray, arrayDims);
                         identifierNode = findIdentifierInDeclarator(firstChild);  // ✅ GET NODE!
                       
                        if (!memberName.empty()) {
                            // Append pointer notation to type if needed
                            string fullType = memberType;
                            for (int i = 0; i < pointerDepth; i++) {
                                fullType += "*";
                            }
                            if (isArray) {
                                for (int dim : arrayDims) {
                                    fullType += "[";
                                    if (dim > 0) fullType += to_string(dim);
                                    fullType += "]";
                                }
                            }
                            members[memberName] = fullType;
                            memberNodes[memberName] = identifierNode;
                            memberOrder.push_back(memberName);
                            cout << "DEBUG: Found struct member: " << memberName << " [" << fullType << "]\n";
                            continue;  // Skip the default add below
                        }
                    }
                }
            }
        }
        else if (child->name == "DECLARATION") {
            // Nested declaration - recursively extract
            extractStructMembers(child, members, memberNodes, memberOrder);
        }
    }
    for (const auto& pair : memberNamesAndNodes) {
        if (members.find(pair.first) == members.end()) {
            members[pair.first] = memberType;
            memberNodes[pair.first] = pair.second;  // ✅ STORE NODE!
            memberOrder.push_back(pair.first);
             cout << "DEBUG: Found struct member: " << pair.first << " [" << memberType << "]\n";
        }
    }
  
}
void SemanticAnalyzer::processEnum(Node* node) {
    if (!node) return;
    Node* nodee=NULL;
    string enumName;
    vector<Node*> enumerators;
    bool isDefinition = false;
    
    // Check if this is a definition (has enumerators) or declaration (no enumerators)
    for (auto child : node->children) {
        if (child->name == "IDENTIFIER") {
        nodee=child;
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
    Symbol* existingSym = symbolTable.lookup(enumName);
    
    if (isDefinition) {
        // This is a definition (with enumerators)
        if (!enumerators.empty()) {
            map<string, int> values;
            int currentValue = 0;
            map<string, Node*> enumeratorNodes;
            for (auto enumerator : enumerators) {
                int value = currentValue;
                if (enumerator->parent && enumerator->parent->name == "ENUMERATOR") {
                    for (auto enumChild : enumerator->parent->children) {
                        if (enumChild->name == "CONSTANT_EXPR") {
                            // Evaluate the constant expression
                            value = evaluateConstantExpression(enumChild);
                            break;
                        }
                    }
                }
                
                values[enumerator->lexeme] = value;
                enumeratorNodes[enumerator->lexeme] = enumerator;
                currentValue = value + 1;  // Next enumerator gets next value
            }
            for (const auto& pair : values) {
                enumConstants[pair.first] = pair.second;
            }
            
            
            if (existingSym) {
              
              if (existingSym->isEnum) {
                    // Update the existing symbol with enum values
                   
            existingSym->enumValues = values;
      
                    // ✅ ADD THIS: Create scope for enum constants
                    //symbolTable.enterScope();
                    for (const auto& pair : values) {
                        enumConstants[pair.first] = pair.second;
                       Node* enumNode = enumeratorNodes[pair.first];  // ✅ GET CORRECT NODE!
                       string enumConstType = "enum " + enumName;
                        if (!symbolTable.addSymbol(pair.first, enumConstType, enumNode)) {
                            addError("Redefinition of enumerator '" + pair.first + "'");
                        } else {
    Symbol* sym = symbolTable.lookupCurrentScope(pair.first);
    if (sym && enumNode) {
        enumNode->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }

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
                if (symbolTable.addSymbol(enumName, "enum", nodee, false, {}, false, {}, 0, false, true)) {
                    Symbol* sym = symbolTable.lookupCurrentScope(enumName);
                    
    if (sym && nodee) {
        nodee->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }
      
                    if (sym) {
                        sym->enumValues = values;
                    }
                    
                    // Add enum constants to scope
                    //symbolTable.enterScope();
                    for (const auto& pair : values) {
                        enumConstants[pair.first] = pair.second;
                        string enumConstType = "enum " + enumName;
                        if (!symbolTable.addSymbol(pair.first, enumConstType, node)) {
                            addError("Redefinition of enumerator '" + pair.first + "'");
                        } else {
                        
    Symbol* sym = symbolTable.lookupCurrentScope(pair.first);
    if (sym && node) {
        node->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }

                        
                        
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
            if (symbolTable.addSymbol(enumName, "enum", nodee, false, {}, false, {}, 0, false, true)) {
            
    // ✅ ADDED SUCCESSFULLY - NOW LOOKUP TO GET THE SYMBOL
    Symbol* sym = symbolTable.lookupCurrentScope(enumName);
    if (sym && nodee) {
        nodee->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }

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
    // Treat boolean as a distinct logical type (not an integer) for semantics.
    // This ensures bitwise ops, modulo, and arithmetic integer-only checks exclude 'bool'.
    return type == "int" || type == "short" || type == "long" || type == "long long" ||
           type == "unsigned" || type == "unsigned int" || type == "unsigned short" || 
           type == "unsigned long" || type == "signed" || type == "char" ||
           type == "unsigned char" || type == "signed char"; // intentionally excludes bool
}
bool SemanticAnalyzer::isNumericType(const std::string& type) {
    return isIntegerType(type) || type == "float" || type == "double" || type.rfind("enum ", 0) == 0;;
}

bool SemanticAnalyzer::isPointerType(const string& type) {
    return type.find("*") != string::npos ;
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
        
          if ((type1.find("enum ") == 0 && isIntegerType(type2)) || (type2.find("enum ") == 0 && isIntegerType(type1))) {
        return true;
    }
    
    // Enum to enum of same type
    if (type1.find("enum ") == 0 && type2.find("enum ") == 0) {
        return type1 == type2;  // Same enum type
    }
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

        bool type1IsEnum = type1.rfind("enum ", 0) == 0;
        bool type2IsEnum = type2.rfind("enum ", 0) == 0;
        if (type1IsEnum || type2IsEnum) {
            if (type1IsEnum && type2IsEnum) {
                return type1 == type2;
            }
            if (type1IsEnum && isIntegerType(type2)) return true;
            if (type2IsEnum && isIntegerType(type1)) return true;
            return false;
        }

        // Both numeric types (non-enum)
        if (isNumericType(type1) && isNumericType(type2)) return true;

        // Pointer assignments
        if (isPointerType(type1) && isPointerType(type2)) {
    // ✅ Check if pointer depths match
    int depth1 = countPointerLevel(type1);
    int depth2 = countPointerLevel(type2);
    
    if (depth1 != depth2) {
        return false;  // int* vs int** are incompatible
    }
    
    // ✅ Optional: Check base types are compatible
    string base1 = getBaseTypeFromPointer(type1);
    string base2 = getBaseTypeFromPointer(type2);
        if (base1 == base2) {return true;}
    
    // Allow some base type conversions
    if ((base1 == "char" && base2 == "int") ||
        (base1 == "int" && base2 == "char") ||
        (base1 == "void" && isPointerType(base2)) ||  
        (base2 == "void" && isPointerType(base1))) {
        return true;
    }
    
    return false;

}
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

    // Disallow boolean arithmetic explicitly for all arithmetic ops
    if (leftType == "bool" || rightType == "bool") {
        addError("Invalid arithmetic on boolean type with '" + operation + "' (operands must be non-bool numeric types)");
        return;
    }
    
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
    string op;
    if (node->children[1]) {
        op = node->children[1]->lexeme; // e.g., '=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>='
    }

    // (debug traces removed)

    // Special-case: skip generic assignment checks for initializer lists used in declarations
    // Pattern: inside INIT_DECL_LIST with RHS=INIT_LIST/INIT_LIST_EMPTY
    // Detailed per-element checks (arrays, structs, etc.) are handled separately by the
    // initializer handling logic (and IR generation). Skipping here avoids false
    // type-mismatch reports during the declaration processing phase.
    if (node->parent && node->parent->name == "INIT_DECL_LIST") {
        if (rhs && (rhs->name == "INIT_LIST" || rhs->name == "INIT_LIST_EMPTY")) {
            return;
        }
    }
    
    string lhsType = getExpressionType(lhs);
    string rhsType = getExpressionType(rhs);
    
    // Skip if we couldn't determine types
    if (lhsType.empty() || rhsType.empty()) return;

    // Handle compound assignment operators with proper arithmetic semantics
    if (op == "+=" || op == "-=" || op == "*=" || op == "/=" || op == "%=" ||
        op == "&=" || op == "|=" || op == "^=" || op == "<<=" || op == ">>=") {
        // Disallow boolean participation in compound arithmetic/bitwise
        if (lhsType == "bool" || rhsType == "bool") {
            addError("Invalid compound operation '" + op + "' with boolean operand(s)");
            return;
        }
        // Pointer rules
        if (isPointerType(lhsType)) {
            if (op == "+=" || op == "-=") {
                // pointer += integer is allowed; float/double and pointer RHS are invalid
                if (!isIntegerType(rhsType)) {
                    addError("Invalid operation: cannot apply '" + op + "' with pointer '" + lhsType + "' and non-integer '" + rhsType + "'");
                }
                return;
            }
            // Any other compound op on pointer is invalid
            addError("Invalid operation: cannot apply '" + op + "' to pointer type '" + lhsType + "'");
            return;
        }
        // Numeric requirements for specific ops
        if (op == "%=") {
            if (!isIntegerType(lhsType) || !isIntegerType(rhsType)) {
                addError("Invalid operands for '%=' (requires integer types)");
                return;
            }
            return; // ok
        }
        if (op == "&=" || op == "|=" || op == "^=" || op == "<<=" || op == ">>=") {
            if (!isIntegerType(lhsType) || !isIntegerType(rhsType)) {
                addError("Invalid operands for bitwise compound op '" + op + "' (requires integer types)");
                return;
            }
            return; // ok
        }
        // For +=, -=, *=, /= on numerics, ensure both are numeric (non-pointer) types
        if (!isNumericType(lhsType) || !isNumericType(rhsType)) {
            addError("Type mismatch in compound arithmetic: '" + lhsType + "' " + op + " '" + rhsType + "'");
            return;
        }
        return; // ok
    }
    
    // Exact match is always OK
    if (lhsType == rhsType) return;
    
    // Numeric types can be assigned to each other (with implicit conversion)
    if (isNumericType(lhsType) && isNumericType(rhsType)) return;

    // Allow assignments between boolean and integer types (implicit conversion)
    if ((lhsType == "bool" && isIntegerType(rhsType)) ||
        (rhsType == "bool" && isIntegerType(lhsType))) {
        return;
    }
    
      if ((lhsType.find("enum ") == 0 && isIntegerType(rhsType)) ||
        (rhsType.find("enum ") == 0 && isIntegerType(lhsType))) {
        return ;
    }
    
    // Enum to enum of same type
    if (lhsType.find("enum ") == 0 && rhsType.find("enum ") == 0) {
        return ;  // Same enum type
    }
    
    // Pointer to pointer of same base type
    if (isPointerType(lhsType) && isPointerType(rhsType)) {
        checkPointerAssignment(/*node, */lhsType, rhsType);
        return;
    }
    
    // NULL (integer constant 0) can be assigned to pointers; other integers cannot
    if (isPointerType(lhsType) && isIntegerType(rhsType)) {
        // Allow only the literal 0 as a null-pointer constant
        if (rhs && rhs->name == "INTEGER_CONSTANT") {
            // Accept only exact 0 as null (avoid accepting arbitrary integer constants)
            if (rhs->lexeme == "0") {
                return; // ok: ptr = 0;
            }
        }
        // For any non-zero integer value or non-constant integer expression, this is invalid
        addError("Cannot assign non-pointer type '" + rhsType + "' to pointer type '" + lhsType + "'");
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


string SemanticAnalyzer::getBaseTypeFromPointer(const string& type) {
    // Find the first '*' in the type string and return everything before it as the base type
    size_t pos = type.find('*');
    if (pos == string::npos) {
        // No pointer marker - return the type as-is
        return type;
    }
    // Trim trailing spaces before the '*' if any
    size_t end = pos;
    while (end > 0 && isspace(static_cast<unsigned char>(type[end - 1]))) --end;
    return type.substr(0, end);
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
          if (type.find("struct ") == 0 || type.find("union ") == 0) {
          return false;}
        
        // Floating-point types should NOT be used directly in logical operations
        // Characters should NOT be used directly in logical operations
        return false;
    };
    // Check left operand
if (!isValidForLogicalOp(leftType)) {
    if (leftType.find("struct ") == 0 || leftType.find("union ") == 0) {
        addError("Invalid use of " + leftType + " in logical operation '" + operation + 
                "' (struct/union types cannot be used in boolean context)");
    } else {
        addError("Invalid operand type '" + leftType + "' for logical operation '" + operation + 
                "' (requires boolean, integer, or pointer type)");
    }
}

// Check right operand
if (!isValidForLogicalOp(rightType)) {
    if (rightType.find("struct ") == 0 || rightType.find("union ") == 0) {
        addError("Invalid use of " + rightType + " in logical operation '" + operation + 
                "' (struct/union types cannot be used in boolean context)");
    } else {
        addError("Invalid operand type '" + rightType + "' for logical operation '" + operation + 
                "' (requires boolean, integer, or pointer type)");
    }
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
      string lhsBase = getBaseTypeFromPointer(lhsType); // "float" from "float*"
    string rhsBase = getBaseTypeFromPointer(rhsType); // "int" from "int*"
   if (lhsBase == "void" || rhsBase == "void") {
        // Allowing assignment if one side is a void pointer
        return;
    }

    // 3. Handle Generic Pointer Type Mismatch
    // Check if the base types are different.
    if (lhsBase != rhsBase) {
        // Error: assigning int* (rhsType) to float* (lhsType)
        addError("Type mismatch in pointer assignment: cannot assign '" + rhsType + "' to '" + lhsType + "'");
       
    }
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
    // Disallow increment/decrement on boolean explicitly
    else if (operandType == "bool") {
        addError("Invalid operand to " + operation + " (boolean type)");
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
    // Disallow increment/decrement on boolean explicitly
    else if (operandType == "bool") {
        addError("Invalid operand to " + operation + " (boolean type)");
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
    
     if (baseNode->name == "IDENTIFIER") {
        if (!baseNode->symbol) {
            Symbol* baseSym = symbolTable.lookup(baseNode->lexeme);
            if (baseSym) {
                baseNode->symbol = baseSym;
                cout << "DEBUG: Attached symbol " << baseSym->name 
                     << " to member access base identifier" << endl;
            } else {
                addError("Undeclared identifier: '" + baseNode->lexeme + "'");
                return;
            }
        }
    }
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
     else if (structType.find("union ") == 0) {
       structType = structType.substr(6);
    }
    // Look up the struct/union type in symbol table
    Symbol* structSym = symbolTable.lookup(structType);
  
    if (!structSym) {
        addError("Unknown struct type '" + structType + "'");
        return;
    }

    if (!structSym->isStruct && !structSym->isUnion) {
        addError("Member access requires struct or union type, but got '" + structType + "'");
        return;
    }
    
    // RULE 4: Check if the member exists in the struct/union
    if (structSym->structMembers.find(memberName) == structSym->structMembers.end()) {
        addError("Struct '" + structType + "' has no member named '" + memberName + "'");
        return;
    }
    // Success - member access is valid
    string typeKind = structSym->isUnion ? "union" : "struct";
    cout << "DEBUG: Valid member access: " << typeKind << " " << structType 
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
            if (child->name == "CASE_ELEMENT") {
                if (!allPathsReturn(child)) {
                    allCasesReturn = false;
                }
            }
            else if (child->name == "DEFAULT_ELEMENT") {
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
    Node* nodee=NULL;
    string baseType;
    string aliasName;
    int pointerDepth = 0;
    bool isArray = false;
    vector<int> arrayDims;
    
    // Extract base type from DECL_SPECIFIERS
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
            baseType = extractTypeFromDeclSpecifiers(child);
             cout << "DEBUG processTypedef: Extracted base type = '" << baseType << "'\n";
            // If this typedef includes a struct/union/enum definition, process it so the type exists
            for (auto specChild : child->children) {
                if (specChild->name == "STRUCT_OR_UNION_SPECIFIER") {
                    // Has body? then define struct/union
                    for (auto bodyChild : specChild->children) {
                        if (bodyChild->name == "DECLARATION") {
                            processStruct(specChild);
                            break;
                        }
                    }
                } else if (specChild->name == "ENUM_SPECIFIER") {
                    // Has enumerators? then define enum and its constants
                    if (isEnumDefinition(specChild)) {
                        processEnum(specChild);
                    }
                }
            }
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto declChild : child->children) {
                if (!declChild->children.empty()) {
                    Node* firstChild = declChild->children[0];
                    
                    if (firstChild->name == "IDENTIFIER") {
                    nodee=firstChild;
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
   /* for (int i = 0; i < pointerDepth; i++) {
        fullAliasedType += "*";
    }*/
    
    cout << "DEBUG: Processing typedef: " << aliasName << " = " << fullAliasedType;
    if (isArray) {
        for (int dim : arrayDims) {
            cout << "[" << (dim == -1 ? "" : to_string(dim)) << "]";
        }
    }
    cout << "\n";
   
  
     if (!symbolTable.addSymbol(aliasName, fullAliasedType, nodee, false, {}, 
                               isArray, arrayDims, pointerDepth, false, false, false,
                               true, fullAliasedType)) {  // ← This is the aliased type!
        // If a symbol with the same name already exists, it may be a struct/union/enum tag.
        // In C, tags (struct/union/enum) live in a separate namespace and it's legal to
        // have "typedef struct S S;" and "typedef enum E E;". In that case, don't treat it
        // as a redefinition error — augment the existing tag symbol with typedef metadata.
        Symbol* existing = symbolTable.lookupCurrentScope(aliasName);
        if (existing && (existing->isStruct || existing->isUnion || existing->isEnum) && !existing->isTypedef) {
            existing->isTypedef = true;
            existing->aliasedType = fullAliasedType;  // e.g., "struct S" or "enum E"
            // Note: do NOT mutate pointerDepth for the tag symbol; typedef name equals tag implies no stars.
            if (nodee) {
                nodee->symbol = existing;
                cout << "DEBUG: Attached symbol " << existing->name << " to AST node (typedef overlay)" << endl;
            }
            cout << "SUCCESS: Typedef '" << aliasName << "' registered as alias for '"
                 << fullAliasedType << "' (overlay on tag)\n";
        } else {
            addError("Redefinition of typedef '" + aliasName + "'");
        }
    } else {
    
    
    Symbol* sym = symbolTable.lookupCurrentScope(aliasName);
    if (sym && nodee) {
        nodee->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    
}
        cout << "SUCCESS: Typedef '" << aliasName << "' registered as alias for '" 
             << fullAliasedType << "'\n";
    }
}



string SemanticAnalyzer::resolveTypedef(const string& type) {
    cout << "DEBUG resolveTypedef: Looking up '" << type << "'\n";
    Symbol* sym = symbolTable.lookup(type);

    if (sym) {
        // If this symbol is a typedef name, resolve to its aliased type.
        if (sym->isTypedef) {
            cout << "  Resolving typedef '" << type << "' -> '" << sym->aliasedType << "'\n";
            string resolved = resolveTypedef(sym->aliasedType);

            // Append this typedef's pointer depth
            for (int i = 0; i < sym->pointerDepth; i++) {
                resolved += "*";
            }

            return resolved;
        }
        // Not a typedef: could be a tag name or ordinary symbol. Do not substitute
        // with sym->type (which may be just "struct"/"union"/"enum"). Preserve the input
        // type token. This ensures we don't lose tag names like 'struct S'.
    } else {
        cout << "  Symbol not found in table\n";
    }

    cout << "  Returning original type: '" << type << "'\n";
    return type;  // Not a typedef, return as-is (e.g., 'int' or tag reference)
}

bool SemanticAnalyzer::isConstantIndex(Node* indexNode, int& value) {
    if (!indexNode) return false;
    
    // Case 1: Direct integer constant
    if (indexNode->name == "INTEGER_CONSTANT") {
        try {
            value = stoi(indexNode->lexeme);
            return true;
        } catch (...) {
            return false;
        }
    }
    
    // Case 2: Unary minus with constant: -5
    if (indexNode->name == "UNARY_OP" && indexNode->children.size() == 2) {
        Node* op = indexNode->children[0];
        Node* operand = indexNode->children[1];
        
        if (op->name == "-" && operand->name == "INTEGER_CONSTANT") {
            try {
                value = -stoi(operand->lexeme);
                return true;
            } catch (...) {
                return false;
            }
        }
    }
    
    // Case 3: Use existing constant expression evaluator
    int result = evaluateConstantExpression(indexNode);
    if (result != -1) {
        value = result;
        return true;
    }
    
    return false;
}

void SemanticAnalyzer::checkConditionExpression(Node* node) {
    if (!node) return;
    
    string condType = getExpressionType(node);
    
    if (condType.empty()) return;
    
    // Check if the type can be used in a boolean context
    bool isValid = (condType == "bool" || isIntegerType(condType) || isPointerType(condType));
    
    // CRITICAL: Structs and unions cannot be used in boolean context
    if (condType.find("struct ") == 0 || condType.find("union ") == 0) {
        addError("Invalid use of " + condType + " in condition (struct/union types cannot be used in boolean context)");
        return;
    }
    
    // Arrays cannot be used directly (though they decay to pointers in most contexts)
    Symbol* sym = nullptr;
    if (node->name == "IDENTIFIER") {
        sym = symbolTable.lookup(node->lexeme);
        if (sym && sym->isArray) {
            // This is actually allowed in C (array decays to pointer, then checked for NULL)
            // But we can add a warning if desired
            return;
        }
    }
    
    // Additional specific checks aligned with logical operation rules
    if (condType == "float" || condType == "double") {
        addError("Invalid use of floating-point type '" + condType + "' in condition (use explicit comparison, e.g., " + condType + " != 0.0)");
        return;
    }
    if (!isValid) {
        addError("Invalid type '" + condType + "' in condition (requires boolean, integer, or pointer type)");
    }
}



void SemanticAnalyzer::processLabel(Node* node) {
    if (!node || node->children.empty()) return;
    
    Node* labelNode = node->children[0];
    if (labelNode->name != "IDENTIFIER") return;
    
    string labelName = labelNode->lexeme;
    
    // Check if this function is active
    if (currentFunctionName.empty()) {
        addError("Label '" + labelName + "' outside of function");
        return;
    }
    
    // Check if label already exists in this function
    if (currentFunctionLabels.find(labelName) != currentFunctionLabels.end()) {
        addError("Duplicate label '" + labelName + "' in function '" + currentFunctionName + "'");
        return;
    }
    
    // Register the label
    currentFunctionLabels[labelName] = node;
    cout << "DEBUG: Registered label '" << labelName << "' in function '" << currentFunctionName << "'\n";
}

void SemanticAnalyzer::processGoto(Node* node) {
    if (!node || node->children.empty()) return;
    
    Node* labelNode = node->children[0];
    if (labelNode->name != "IDENTIFIER") return;
    
    string labelName = labelNode->lexeme;
    
    // Check if we're inside a function
    if (currentFunctionName.empty()) {
        addError("goto statement outside of function");
        return;
    }
    
    // Store for later verification (labels might be defined after goto)
    pendingGotoStatements.push_back({labelName, node});
    cout << "DEBUG: Found goto to label '" << labelName << "' in function '" << currentFunctionName << "'\n";
}

void SemanticAnalyzer::verifyGotoLabels() {

   cout << "DEBUG verifyGotoLabels: Checking " << pendingGotoStatements.size() 
         << " goto statements\n";
    cout << "DEBUG verifyGotoLabels: Available labels in '" << currentFunctionName << "': ";
    for (const auto& pair : currentFunctionLabels) {
        cout << pair.first << " ";
    }
    cout << "\n";
    
    
    // Verify all pending goto statements
    for (const auto& gotoInfo : pendingGotoStatements) {
        string labelName = gotoInfo.first;
        
        // Check if label exists in current function
     
        if (currentFunctionLabels.find(labelName) == currentFunctionLabels.end()) {
    addError("Label '" + labelName + "' referenced in goto statement is not defined");
}
else {
            cout << "DEBUG: Verified goto to label '" << labelName << "' in function '" << currentFunctionName << "'\n";
        }
    }
}


void SemanticAnalyzer::checkSwitchStatement(Node* node) {
    if (!node || node->children.empty()) return;
    
    // The first child of SWITCH_STMT should be the switch expression
    Node* switchExpr = node->children[0];
    string exprType = getExpressionType(switchExpr);
    
    if (exprType.empty()) return; // Skip if type couldn't be determined
    
    // Check if the expression type is an integer type or enum
    if (!isIntegerType(exprType) && exprType.rfind("enum ", 0) != 0) {
        addError("Switch quantity not an integer (got '" + exprType + "')");
    }
    
    // Also check that case labels are integer constants
    checkCaseLabels(node);
}

void SemanticAnalyzer::checkCaseLabels(Node* switchNode) {
    if (!switchNode) return;

    std::unordered_set<long long> seenValues;
    std::unordered_set<std::string> seenIdentifiers;
    bool defaultSeen = false;

    std::function<void(Node*)> visit = [&](Node* node) {
        if (!node) return;

        if (node->name == "CASE_ELEMENT" && !node->children.empty()) {
            Node* caseExpr = node->children[0];
            long long value = 0;

            if (!tryEvaluateConstantExpression(caseExpr, value)) {
                if (caseExpr->name == "IDENTIFIER") {
                    const std::string& ident = caseExpr->lexeme;
                    if (!enumConstants.count(ident)) {
                        addError("Case label must be a constant integer expression");
                    } else if (!seenIdentifiers.insert(ident).second) {
                        addError("Duplicate case label '" + ident + "'");
        
                    }
                } else {
                    addError("Case label must be a constant integer expression");
                }
                return;
            }

            if (!seenValues.insert(value).second) {
                addError("Duplicate case label with value " + std::to_string(value));
            }
            return;
        }

        if (node->name == "DEFAULT_ELEMENT") {
            if (defaultSeen) {
                addError("Duplicate default label in switch statement");
            }
            defaultSeen = true;
            return;
        }

        for (auto child : node->children) {
            visit(child);
        }
    };

    visit(switchNode);
}

bool SemanticAnalyzer::isConstantIntegerExpression(Node* node) {
    long long value = 0;
    return tryEvaluateConstantExpression(node, value);
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


bool SemanticAnalyzer::isFunctionPrototype(Node* declNode) {
    if (!declNode || declNode->name != "DECLARATION") return false;
    
    // Check if INIT_DECL_LIST contains a function declarator
    for (auto child : declNode->children) {
        if (child->name == "INIT_DECL_LIST") {
            for (auto initDecl : child->children) {
                // Check for FUNCTION_DECL or function-style declarator
                if (hasFunctionDeclarator(initDecl)) {
                    return true;
                }
            }
        }
    }
    
    return false;
}

bool SemanticAnalyzer::hasFunctionDeclarator(Node* node) {
    if (!node) return false;
    
    // Direct function declarator
    if (node->name == "FUNCTION_DECL") return true;
    
    // Check children recursively
    for (auto child : node->children) {
        if (child->name == "FUNCTION_DECL") return true;
        if (hasFunctionDeclarator(child)) return true;
    }
    
    return false;
}

void SemanticAnalyzer::processFunctionPrototype(Node* declNode) {
    if (!declNode) return;
    
    cout << "DEBUG: Processing function prototype\n";
    Node* nodee=NULL;
    string funcName;
    string returnType;
    vector<string> paramTypes;
    Node* funcDeclNode = nullptr;
    
    // Extract return type
    for (auto child : declNode->children) {
        if (child->name == "DECL_SPECIFIERS") {
            returnType = extractTypeFromDeclSpecifiers(child);
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto initDecl : child->children) {
                funcDeclNode = findFunctionDeclarator(initDecl);
                if (funcDeclNode) {
                    // Get function name
                    if (!funcDeclNode->children.empty() && 
                        funcDeclNode->children[0]->name == "IDENTIFIER") {
                        nodee=funcDeclNode->children[0];
                        funcName = funcDeclNode->children[0]->lexeme;
                    }
                    paramTypes = extractFunctionParameters(funcDeclNode);
                    break;
                }
            }
        }
    }
    
    if (returnType.empty()) {
        returnType = "int";  // Default
    }
    
    if (!funcName.empty()) {
        // Add function declaration to symbol table
        bool added = symbolTable.addSymbol(funcName, returnType,nodee, true, paramTypes);
        if (!added) {
            Symbol* existing = symbolTable.lookupCurrentScope(funcName);
            if (existing && existing->isFunction) {
                // Check if signatures match
                if (existing->paramTypes != paramTypes || existing->type != returnType) {
                    addError("Conflicting declaration of function '" + funcName + "'");
                }
                // Otherwise it's a valid redeclaration (no error)
            } else {
                addError("Redeclaration of '" + funcName + "' as different kind of symbol");
            }
        } else {
        
    Symbol* sym = symbolTable.lookupCurrentScope(funcName);
    if (sym && nodee) {
        nodee->symbol = sym;  // ✅ ATTACH SYMBOL TO AST NODE
        cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
    }

            cout << "DEBUG: Added function prototype: " << funcName 
                 << " [" << returnType << "] with " << paramTypes.size() << " parameter(s)\n";
        }
    }
}

Node* SemanticAnalyzer::findFunctionDeclarator(Node* node) {
    if (!node) return nullptr;
    
    if (node->name == "FUNCTION_DECL") return node;
    
    for (auto child : node->children) {
        Node* result = findFunctionDeclarator(child);
        if (result) return result;
    }
    
    return nullptr;
}


void SemanticAnalyzer::checkPrintfStatement(Node* node) {
    if (!node || node->children.empty()) {
        addError("printf requires at least one argument (format string)");
        return;
    }
    
    // First child should be STRING_LITERAL (format string)
    Node* formatNode = node->children[0];
    
    // CHECK 1: First argument must be a string literal
    if (!isStringLiteral(formatNode)) {
        addError("printf format argument must be a string literal");
        return;
    }
    
    // Extract format string
    string formatStr = formatNode->lexeme;
    // Remove surrounding quotes
    if (formatStr.length() >= 2 && formatStr.front() == '"' && formatStr.back() == '"') {
        formatStr = formatStr.substr(1, formatStr.length() - 2);
    }
    
    // Count format specifiers
    int specifierCount = countFormatSpecifiers(formatStr);
    
    // Extract actual arguments (skip first child which is format string)
    vector<Node*> argNodes;
    for (size_t i = 1; i < node->children.size(); i++) {
        Node* child = node->children[i];
        
        // Handle ARG_LIST wrapper
        if (child->name == "ARG_LIST") {
            for (auto argChild : child->children) {
                argNodes.push_back(argChild);
            }
        } else {
            // Direct argument (like in first example: printf("Hello, World!");)
            argNodes.push_back(child);
        }
    }
    
    int actualArgCount = argNodes.size();
    
    // CHECK 2: Argument count matches format specifiers
    if (specifierCount != actualArgCount) {
        addError("printf expects " + to_string(specifierCount) + 
                 " argument(s) for format string, but " + 
                 to_string(actualArgCount) + " provided");
    }
    
    // CHECK 3: Validate argument types match format specifiers
    vector<string> argTypes;
    for (Node* arg : argNodes) {
        string argType = getExpressionType(arg);
        argTypes.push_back(argType);
        
        // Also check that identifiers are declared
        if (arg->name == "IDENTIFIER") {
            checkIdentifier(arg);
        }
    }
    
    if (!argTypes.empty()) {
        validateFormatString(formatStr, argTypes, false);
    }
    
    cout << "DEBUG: printf statement validated with " << actualArgCount << " arguments\n";
    
    // Continue traversing children for nested expressions
    for (auto child : node->children) {
        traverseAST(child);
    }
}

void SemanticAnalyzer::checkScanfStatement(Node* node) {
    if (!node || node->children.empty()) {
        addError("scanf requires at least one argument (format string)");
        return;
    }
    
    // First child should be STRING_LITERAL (format string)
    Node* formatNode = node->children[0];
    
    // CHECK 1: First argument must be a string literal
    if (!isStringLiteral(formatNode)) {
        addError("scanf format argument must be a string literal");
        return;
    }
    
    // Extract format string
    string formatStr = formatNode->lexeme;
    if (formatStr.length() >= 2 && formatStr.front() == '"' && formatStr.back() == '"') {
        formatStr = formatStr.substr(1, formatStr.length() - 2);
    }
    
    // Count format specifiers
    int specifierCount = countFormatSpecifiers(formatStr);
    
    // Extract actual arguments (skip first child which is format string)
    vector<Node*> argNodes;
    for (size_t i = 1; i < node->children.size(); i++) {
        Node* child = node->children[i];
        
        // Handle ARG_LIST wrapper
        if (child->name == "ARG_LIST") {
            for (auto argChild : child->children) {
                argNodes.push_back(argChild);
            }
        } else {
            argNodes.push_back(child);
        }
    }
    
    int actualArgCount = argNodes.size();
    
    // CHECK 2: Argument count matches format specifiers
    if (specifierCount != actualArgCount) {
        addError("scanf expects " + to_string(specifierCount) + 
                 " argument(s) for format string, but " + 
                 to_string(actualArgCount) + " provided");
    }
    
    // CHECK 3: All scanf arguments (except format string) must be pointers or address-of expressions
    for (size_t i = 0; i < argNodes.size(); i++) {
        Node* arg = argNodes[i];
        
        // Check if identifier is declared first
        if (arg->name == "IDENTIFIER") {
            Symbol* sym = symbolTable.lookup(arg->lexeme);
            if (!sym) {
                addError("Undeclared identifier in scanf: '" + arg->lexeme + "'");
                continue;
            }

            checkIdentifier(arg);

            bool treatsAsPointer = sym->pointerDepth > 0 || sym->isArray;
            if (!treatsAsPointer) {
                addError("scanf argument " + to_string(i + 1) +
                         " ('" + arg->lexeme + "') must be a pointer (use & operator)");
            }
            continue;
        }
        // Check if it's an address-of expression (which is correct)
        else if (arg->name == "UNARY_OP") {
            if (arg->children.size() >= 2 && arg->children[0]->name == "&") {
                // This is correct: &x
                Node* operand = arg->children[1];
                if (operand->name == "IDENTIFIER") {
                    checkIdentifier(operand);  // Verify the variable is declared
                }
            }
        }
        else {
            // Get type and check if it's a pointer
            string argType = getExpressionType(arg);
            if (!argType.empty() && !isPointerType(argType)) {
                addError("scanf argument " + to_string(i + 1) + 
                         " must be a pointer (use & operator for variables)");
            }
        }
    }
    
    // CHECK 4: Validate argument types match format specifiers
    vector<string> argTypes;
    for (Node* arg : argNodes) {
        argTypes.push_back(getExpressionType(arg));
    }
    
    if (!argTypes.empty()) {
        validateFormatString(formatStr, argTypes, true);
    }
    
    cout << "DEBUG: scanf statement validated with " << actualArgCount << " arguments\n";
    
    // Continue traversing children for nested expressions
    for (auto child : node->children) {
        traverseAST(child);
    }
}

bool SemanticAnalyzer::isStringLiteral(Node* node) {
    if (!node) return false;
    return node->name == "STRING_LITERAL";
}

int SemanticAnalyzer::countFormatSpecifiers(const string& format) {
    int count = 0;
    for (size_t i = 0; i < format.length(); i++) {
        if (format[i] == '%') {
            if (i + 1 < format.length()) {
                if (format[i + 1] == '%') {
                    i++;  // Skip %% (escaped percent)
                } else {
                    count++;
                    // Skip format specifier characters
                    i++;
                    while (i < format.length() && !isalpha(format[i])) {
                        i++;  // Skip modifiers like .2, l, etc.
                    }
                }
            }
        }
    }
    return count;
}

bool SemanticAnalyzer::validateFormatString(const string& format, 
                                            const vector<string>& argTypes, 
                                            bool isScanf) {
    size_t argIndex = 0;
    
    for (size_t i = 0; i < format.length(); i++) {
        if (format[i] == '%') {
            if (i + 1 >= format.length()) continue;
            
            if (format[i + 1] == '%') {
                i++;  // Skip %%
                continue;
            }
            
            // Skip width/precision modifiers
            i++;
            while (i < format.length() && (isdigit(format[i]) || format[i] == '.' || format[i] == '-')) {
                i++;
            }
            
            // Skip length modifiers (l, ll, h, etc.)
            if (i < format.length() && (format[i] == 'l' || format[i] == 'h' || format[i] == 'z')) {
                if (i + 1 < format.length() && format[i] == format[i + 1]) {
                    i++;  // ll or hh
                }
                i++;
            }
            
            if (i >= format.length()) continue;
            
            char specifier = format[i];
            
            if (argIndex >= argTypes.size()) {
                // Already reported count mismatch
                return false;
            }
            
            string argType = argTypes[argIndex];
            
            if (argType.empty()) {
                argIndex++;
                continue;  // Skip if type couldn't be determined
            }
            
            // Remove pointer level for scanf (we expect pointers)
            if (isScanf && isPointerType(argType)) {
                argType = argType.substr(0, argType.length() - 1);
            }
            
            // Type checking based on format specifier
            bool typeMatch = true;
            string expectedType;
            
            switch (specifier) {
                case 'd': case 'i':  // signed int
                    expectedType = "int";
                    typeMatch = (argType == "int" || argType == "short" || argType == "long");
                    break;
                case 'u': case 'o': case 'x': case 'X':  // unsigned int
                    expectedType = "unsigned int";
                    typeMatch = (argType.find("unsigned") != string::npos || 
                                argType == "int" || argType == "long");
                    break;
                case 'f': case 'e': case 'g': case 'F': case 'E': case 'G':  // float/double
                    expectedType = "float/double";
                    typeMatch = (argType == "float" || argType == "double");
                    break;
                case 'c':  // char
                    expectedType = "char";
                    typeMatch = (argType == "char" || argType == "int");
                    break;
                case 's':  // string
                    expectedType = "char*";
                    typeMatch = (argType == "char*" || 
                                (isPointerType(argTypes[argIndex]) && argType == "char"));
                    break;
                case 'p':  // pointer
                    expectedType = "pointer";
                    typeMatch = isPointerType(argTypes[argIndex]);
                    break;
                default:
                    // Unknown specifier - skip validation
                    argIndex++;
                    continue;
            }
            
            if (!typeMatch) {
                string funcName = isScanf ? "scanf" : "printf";
                addError(funcName + " format specifier '%" + string(1, specifier) + 
                         "' expects " + expectedType + " but argument " + 
                         to_string(argIndex + 1) + " has type '" + argTypes[argIndex] + "'");
            }
            
            argIndex++;
        }
    }
    
    return true;
}

bool SemanticAnalyzer::isEnumDefinition(Node* enumSpecNode) {
    if (!enumSpecNode || enumSpecNode->name != "ENUM_SPECIFIER") return false;
    
    for (auto child : enumSpecNode->children) {
        if (child->name == "ENUMERATOR_LIST") {
            return true;  // Has enumerators = definition
        }
    }
    return false;  // Just a name = reference
}
