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
        
        // Register standard library functions in global scope
        symbolTable.addSymbol("atoi", "int", nullptr, true, {"char*"}, false, {}, 0, false, false, false, false, "", false, false, false);
        symbolTable.addSymbol("atof", "float", nullptr, true, {"char*"}, false, {}, 0, false, false, false, false, "", false, false, false);
        symbolTable.addSymbol("abs", "int", nullptr, true, {"int"}, false, {}, 0, false, false, false, false, "", false, false, false);
        
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
        // Before processing typedef, validate no conflicting storage classes like 'static'
        bool hasStaticHere = false;
        bool hasAutoHere = false;
        for (auto child : node->children) {
            if (child->name == "DECL_SPECIFIERS") {
                for (auto specChild : child->children) {
                    if (specChild->name == "STATIC" ||
                        (specChild->name == "STORAGE_CLASS_SPECIFIER" && specChild->lexeme == "static")) {
                        hasStaticHere = true;
                    }
                    if (specChild->name == "AUTO" ||
                        (specChild->name == "STORAGE_CLASS_SPECIFIER" && specChild->lexeme == "auto")) {
                        hasAutoHere = true;
                    }
                }
            }
        }
        if (hasStaticHere) addError("Cannot combine 'static' and 'typedef' storage classes");
        if (hasAutoHere) addError("Cannot combine 'auto' and 'typedef' storage classes");

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
        // Defer full validation to dedicated checker which supports
        // direct calls and function pointer calls.
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
// Validate va_arg requested type (basic checks)
else if (node->name == "VA_ARG") {
    // node children: [0]=va_list expr, [1]=type spec
    if (node->children.size() >= 2) {
        std::string reqType = extractTypeFromTypeName(node->children[1]);
        if (reqType.empty() || reqType == "void") {
            addError("'va_arg' requested invalid type '" + (reqType.empty()?std::string("<unknown>"):reqType) + "'");
        }
    }
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

// Check sizeof operations
else if (node->name == "SIZEOF" || node->name == "SIZEOF_TYPE") {
    checkSizeof(node);
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
    {
        std::vector<bool> _tmpRefFlags;
        paramTypes = extractFunctionParameters(funcDeclNode, &_tmpRefFlags);
    }
    // Determine if this is a variadic declaration
    bool declIsVariadic = false;
    if (funcDeclNode) {
        std::function<bool(Node*)> hasEllipsis = [&](Node* n) -> bool {
            if (!n) return false;
            if (n->name == "ELLIPSIS") return true;
            for (auto c : n->children) if (hasEllipsis(c)) return true;
            return false;
        };
        declIsVariadic = hasEllipsis(funcDeclNode);
    }

 
 checkStaticRedeclaration(funcName, hasStatic); 
// Add function to symbol table with parameters
    bool added = symbolTable.addSymbol(funcName, fullReturnType, nodee, true, paramTypes, false, {}, 0, false, false, false, false, "",  hasStatic, false, declIsVariadic);
    
  
    if (!added) {
        // Check if it's an incompatible redeclaration
        Symbol* existing = symbolTable.lookupCurrentScope(funcName);
        if (existing && existing->isFunction) {
            if (existing->paramTypes != paramTypes || existing->type != fullReturnType || existing->isVariadic != declIsVariadic) {
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
    if (sym) {
        // isVariadic already set during addSymbol call above
        // Attach parameter reference flags
        std::vector<bool> paramIsRef2;
        extractFunctionParameters(funcDeclNode, &paramIsRef2);
        sym->paramIsReference = paramIsRef2;
          sym->isStatic = hasStatic;  // ✅ Ensure it's set
        cout << "DEBUG: Function '" << funcName << "' marked as " 
             << (hasStatic ? "static" : "non-static") << ", variadic=" << sym->isVariadic << "\n";
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

    // First pass: handle typedef/type names and struct/union/enum early
    for (auto child : declSpecifiersNode->children) {
        if (child->name == "STORAGE_CLASS_SPECIFIER") {
            continue; // ignore storage in type composition
        }
        if (child->name == "TYPE_NAME") {
            string type = child->lexeme;
            cout << "  DEBUG extractType: Found TYPE_NAME = '" << type << "'\n";
            if (!type.empty()) {
                string resolvedType = resolveTypedef(type);
                cout << "  DEBUG extractType: Resolved '" << type << "' to '" << resolvedType << "'\n";
                return resolvedType;
            }
        }
        else if (child->name == "STRUCT_OR_UNION_SPECIFIER") {
            bool isUnion = false;
            string typeName;
            for (auto structChild : child->children) {
                if (structChild->name == "UNION") isUnion = true;
                else if (structChild->name == "IDENTIFIER") typeName = structChild->lexeme;
            }
            string typePrefix = isUnion ? "union" : "struct";
            if (!typeName.empty()) return typePrefix + " " + typeName;
            return typePrefix; // anonymous
        }
        else if (child->name == "ENUM_SPECIFIER") {
            for (auto enumChild : child->children) {
                if (enumChild->name == "IDENTIFIER") {
                    string enumType = "enum " + enumChild->lexeme;
                    cout << "  DEBUG extractType: Found ENUM_SPECIFIER = '" << enumType << "'\n";
                    return enumType;
                }
            }
            // Anonymous enum - return generic "enum" type
            cout << "  DEBUG extractType: Found anonymous ENUM_SPECIFIER\n";
            return "enum";
        }
    }

    // Second pass: aggregate builtin type specifiers (signed/unsigned/short/long/char/int/float/double/void)
    int longCount = 0;
    bool isShort = false, isUnsigned = false, isSigned = false;
    string base;

    function<void(Node*)> collectSpecs = [&](Node* n){
        if (!n) return;
        for (auto c : n->children) {
            if (c->name == "TYPE_SPECIFIER") {
                string t = resolveTypedef(c->lexeme);
                if (t == "long") longCount++;
                else if (t == "short") isShort = true;
                else if (t == "unsigned") isUnsigned = true;
                else if (t == "signed") isSigned = true;
                else if (t == "int" || t == "char" || t == "float" || t == "double" || t == "void") base = t;
                else base = t; // typedef or others resolved here
            } else if (c->name == "DECL_SPECIFIERS") {
                collectSpecs(c);
            }
        }
    };
    collectSpecs(declSpecifiersNode);

    if (base.empty() && (isShort || isUnsigned || isSigned || longCount > 0)) base = "int";
    if (base.empty()) return "";

    // Build canonical type string
    string result;
    if (base == "char") {
        if (isUnsigned) result = "unsigned char";
        else if (isSigned) result = "signed char";
        else result = "char";
    } else if (base == "int") {
        if (isUnsigned) {
            if (isShort) result = "unsigned short";
            else if (longCount >= 2) result = "unsigned long long";
            else if (longCount == 1) result = "unsigned long";
            else result = "unsigned int";
        } else {
            if (isShort) result = "short";
            else if (longCount >= 2) result = "long long";
            else if (longCount == 1) result = "long";
            else result = "int";
        }
    } else if (base == "double") {
        // Treat long double as double for now (no separate support elsewhere)
        result = "double";
    } else if (base == "float") {
        result = "float";
    } else if (base == "void") {
        result = "void";
    } else {
        // Fallback for typedef-resolved names
        result = base;
    }

    return result;
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
    string typedefNameUsed;
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
            
            
            // Capture typedef name if present to carry array dims from alias
            for (auto specChild : child->children) {
                if (specChild->name == "TYPE_NAME") {
                    typedefNameUsed = specChild->lexeme;
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
                    // Detect and handle function pointer declarator: int (*fp)(...)
                    {
                        Node* funcDecl = findFunctionDeclaratorInNode(firstChild);
                        if (funcDecl && isFunctionPointerDeclarator(firstChild)) {
                            // Base type and decl-spec pointer depth captured earlier in pointerDepth
                            int declSpecPtrDepth = pointerDepth; // stars from DECL_SPECIFIERS

                            // Compute return pointer depth contributed between container and FUNCTION_DECL
                            Node* container = firstChild;
                            std::function<int(Node*)> countPointerStars = [&](Node* n)->int{
                                if (!n) return 0;
                                int total = (n->name == "POINTER") ? 1 : 0;
                                for (auto c : n->children) total += countPointerStars(c);
                                return total;
                            };
                            std::function<int(Node*, int)> findPointerDepth = [&](Node* current, int accum)->int{
                                if (!current) return -1;
                                if (current == funcDecl) return accum;
                                int working = accum;
                                for (auto c : current->children) {
                                    if (c == funcDecl) return working;
                                    if (c->name == "POINTER") {
                                        int found = findPointerDepth(c, working + 1);
                                        if (found != -1) return found;
                                        working += countPointerStars(c);
                                    } else {
                                        int found = findPointerDepth(c, working);
                                        if (found != -1) return found;
                                    }
                                }
                                return -1;
                            };
                            int retPtrExtra = std::max(0, findPointerDepth(container, 0));
                            // Variable pointer depth is the stars inside the left child subtree
                            int varPtrDepth = 0;
                            if (!funcDecl->children.empty()) {
                                varPtrDepth = countPointerStars(funcDecl->children[0]);
                            }

                            // Determine variable name node inside the left child subtree
                            Node* idNodeFp = nullptr;
                            if (!funcDecl->children.empty()) {
                                idNodeFp = findIdentifierInDeclarator(funcDecl->children[0]);
                            }
                            string fpName = idNodeFp ? idNodeFp->lexeme : string("");

                            // Extract function parameter types and ref flags
                            std::vector<bool> fpParamIsRef;
                            std::vector<std::string> fpParamTypes = extractFunctionParameters(funcDecl, &fpParamIsRef);
                            // Detect variadic
                            std::function<bool(Node*)> hasEllipsis = [&](Node* n)->bool{
                                if (!n) return false;
                                if (n->name == "ELLIPSIS") return true;
                                for (auto c : n->children) if (hasEllipsis(c)) return true;
                                return false;
                            };
                            bool fpIsVariadic = hasEllipsis(funcDecl);

                            if (!fpName.empty()) {
                                string fullBase = varType; // base
                                // Compute function return type: base + declSpec stars + extra stars on path
                                string funcRetType = fullBase;
                                for (int i = 0; i < declSpecPtrDepth + retPtrExtra; ++i) funcRetType += "*";

                                // Add symbol for variable itself with pointer depth = varPtrDepth
                                if (!symbolTable.addSymbol(fpName, varType, idNodeFp, false, {}, false, {}, varPtrDepth)) {
                                    addError("Redeclaration of variable: " + fpName);
                                } else {
                                    Symbol* sym = symbolTable.lookupCurrentScope(fpName);
                                    if (sym && idNodeFp) {
                                        idNodeFp->symbol = sym;
                                    }
                                    if (sym) {
                                        sym->isFunctionPointer = true;
                                        sym->funcPtrReturnType = funcRetType;
                                        sym->funcPtrParamTypes = fpParamTypes;
                                        sym->funcPtrParamIsReference = fpParamIsRef;
                                        sym->funcPtrIsVariadic = fpIsVariadic;
                                        
                                        // Check initializer if present (declChild is ASSIGN_EXPR with 3 children: lhs, op, rhs)
                                        if (declChild && declChild->name == "ASSIGN_EXPR" && declChild->children.size() >= 3) {
                                            Node* initExpr = declChild->children[2];
                                            // Skip validation if initializer is EMPTY
                                            if (initExpr && initExpr->name == "EMPTY") {
                                                continue;
                                            }
                                            
                                            // Validate function pointer assignment
                                            Symbol* rhsFuncSym = nullptr;
                                            Node* r = initExpr;
                                            // Unwrap EXPR_LIST
                                            if (r && r->name == "EXPR_LIST" && !r->children.empty()) r = r->children[0];
                                            
                                            // Allow null assignment (0 or nullptr)
                                            if (r && ((r->name == "INTEGER_CONSTANT" && r->lexeme == "0") || 
                                                      r->name == "NULLPTR_CONSTANT")) {
                                                continue;
                                            }
                                            
                                            if (r && r->name == "UNARY_OP" && r->children.size() == 2 && r->children[0]->name == "&") {
                                                Node* opnd = r->children[1];
                                                if (opnd && opnd->name == "IDENTIFIER") {
                                                    rhsFuncSym = symbolTable.lookup(opnd->lexeme);
                                                }
                                            } else if (r && r->name == "IDENTIFIER") {
                                                rhsFuncSym = symbolTable.lookup(r->lexeme);
                                            }

                                            if (!rhsFuncSym) {
                                                addError("Cannot initialize function pointer '" + fpName + "' with undeclared symbol");
                                                continue;
                                            }
                                            
                                            // Allow initialization from either a function or a compatible function pointer
                                            bool rhsIsFunctionOrFP = false;
                                            if (rhsFuncSym->isFunction) {
                                                rhsIsFunctionOrFP = true;
                                            } else if (rhsFuncSym->isFunctionPointer) {
                                                // Check if function pointer signatures are compatible
                                                bool compatible = (funcRetType == rhsFuncSym->funcPtrReturnType) &&
                                                                  (fpIsVariadic == rhsFuncSym->funcPtrIsVariadic) &&
                                                                  (fpParamTypes.size() == rhsFuncSym->funcPtrParamTypes.size());
                                                if (compatible) {
                                                    for (size_t i = 0; i < fpParamTypes.size(); ++i) {
                                                        if (fpParamTypes[i] != rhsFuncSym->funcPtrParamTypes[i]) {
                                                            compatible = false;
                                                            break;
                                                        }
                                                    }
                                                }
                                                rhsIsFunctionOrFP = compatible;
                                            }
                                            
                                            if (!rhsIsFunctionOrFP) {
                                                string reason = "non-function";
                                                if (rhsFuncSym) {
                                                    if (rhsFuncSym->isFunctionPointer) {
                                                        reason = "incompatible function pointer signature";
                                                    } else {
                                                        reason = "non-function (type: " + rhsFuncSym->type + ")";
                                                    }
                                                }
                                                addError("Cannot initialize function pointer '" + fpName + "' with " + reason);
                                                continue;
                                            }
                                            // Compare signatures - handle both functions and function pointers  
                                            string rhsRetType = rhsFuncSym->isFunction ? rhsFuncSym->type : rhsFuncSym->funcPtrReturnType;
                                            vector<string> rhsParamTypes = rhsFuncSym->isFunction ? rhsFuncSym->paramTypes : rhsFuncSym->funcPtrParamTypes;
                                            bool rhsVariadic = rhsFuncSym->isFunction ? rhsFuncSym->isVariadic : rhsFuncSym->funcPtrIsVariadic;
                                            
                                            if (funcRetType != rhsRetType) {
                                                addError("Function pointer '" + fpName + "' return type mismatch: expected '" + funcRetType + "' but got '" + rhsRetType + "'");
                                                continue;
                                            }
                                            if (fpIsVariadic != rhsVariadic) {
                                                addError("Function pointer '" + fpName + "' variadic property mismatch");
                                                continue;
                                            }
                                            if (fpParamTypes.size() != rhsParamTypes.size()) {
                                                addError("Function pointer '" + fpName + "' parameter count mismatch: expected " + to_string(fpParamTypes.size()) +
                                                         ", got " + to_string(rhsParamTypes.size()));
                                                continue;
                                            }
                                            for (size_t i = 0; i < fpParamTypes.size(); ++i) {
                                                if (fpParamTypes[i] != rhsParamTypes[i]) {
                                                    addError("Function pointer '" + fpName + "' parameter " + to_string(i+1) + " type mismatch: expected '" +
                                                             fpParamTypes[i] + "', got '" + rhsParamTypes[i] + "'");
                                                    continue;
                                                }
                                                bool expRef = (i < fpParamIsRef.size() && fpParamIsRef[i]);
                                                // For rhs, check if it's a function or function pointer and get reference info accordingly
                                                bool gotRef = false;
                                                if (rhsFuncSym->isFunction) {
                                                    gotRef = (i < rhsFuncSym->paramIsReference.size() && rhsFuncSym->paramIsReference[i]);
                                                } else if (rhsFuncSym->isFunctionPointer) {
                                                    gotRef = (i < rhsFuncSym->funcPtrParamIsReference.size() && rhsFuncSym->funcPtrParamIsReference[i]);
                                                }
                                                if (expRef != gotRef) {
                                                    addError("Function pointer '" + fpName + "' parameter " + to_string(i+1) + " reference qualifier mismatch");
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                }
                                // Done handling this declarator (skip default variable flow)
                                continue;
                            }
                        }
                    }
                    // Direct identifier: int x;
                    if (firstChild->name == "IDENTIFIER") {
                        nodee=firstChild;
                        varName = firstChild->lexeme;
                    }
                    // Array: int a[10]; or int matrix[3][4];
                    else if (firstChild->name == "ARRAY") {
                        // Handle both true array variables and pointer-to-array declarators
                        isArray = true;
                        arrayDimensions = extractArrayDimensions(firstChild, varName);
                        // Try to find the identifier and any pointer depth inside the base declarator
                        Node* baseNode = firstChild;
                        while (baseNode && baseNode->name == "ARRAY" && !baseNode->children.empty()) {
                            baseNode = baseNode->children[0];
                        }
                        int innerPtrDepth = 0;
                        if (baseNode && baseNode->name == "DECLARATOR") {
                            // Pointer-to-array form: int (*p)[N][M]
                            string innerName;
                            bool dummyIsArray = false; vector<int> dummyDims;
                            analyzeDeclarator(baseNode, innerName, innerPtrDepth, dummyIsArray, dummyDims);
                            Node* idInDecl = findIdentifierInDeclarator(baseNode);
                            if (idInDecl) {
                                nodee = idInDecl;
                                varName = innerName.empty() ? idInDecl->lexeme : innerName;
                            }
                            // This is a pointer variable whose pointee is an array. Do not mark the variable itself as array.
                            if (innerPtrDepth > 0) {
                                pointerDepth += innerPtrDepth;
                                isArray = false; // variable is a pointer, not an array object
                            }
                        } else {
                            // True array variable case: base should be IDENTIFIER
                            nodee = findIdentifierInArray(firstChild);
                        }
                            // Check for too many initializers (including multidimensional arrays)
                            // Look for INIT_LIST in declChild (ASSIGN_EXPR)
                            if (declChild->name == "ASSIGN_EXPR" && declChild->children.size() >= 3) {
                                Node* rhs = declChild->children[2];
                                if (rhs && rhs->name == "INIT_LIST" && !arrayDimensions.empty()) {
                                    checkArrayInitializerSize(rhs, arrayDimensions, varName, 0);
                                }
                            }
                    }
                    else if (firstChild->name == "DECLARATOR") {
                        analyzeDeclarator(firstChild, varName, pointerDepthh, isArray, arrayDimensions);
                        nodee = findIdentifierInDeclarator(firstChild); 
           
                    }
                    pointerDepth=pointerDepth+pointerDepthh;
                     cout<<"VARRRTjhYPE"<<pointerDepth<<endl;
                    if (!varName.empty()) {
                        // Handle typedef-based declarations
                        if (!typedefNameUsed.empty()) {
                            Symbol* td = symbolTable.lookup(typedefNameUsed);
                            
                            // If no explicit array declarator but typedef provides array, adopt it
                            if (!isArray && td && td->isTypedef && td->isArray) {
                                isArray = true;
                                arrayDimensions = td->arrayDimensions;
                            }
                            
                            // Check if typedef is a function pointer (non-array case)
                            if (td && td->isTypedef && td->isFunctionPointer && !isArray) {
                                cout << "DEBUG: Variable '" << varName << "' declared as function pointer typedef '" << typedefNameUsed << "'\n";
                                
                                // Add symbol as function pointer variable
                                if (!symbolTable.addSymbol(varName, td->funcPtrReturnType, nodee, false, {}, false, {}, 0)) {
                                    addError("Redeclaration of variable: " + varName);
                                } else {
                                    Symbol* sym = symbolTable.lookupCurrentScope(varName);
                                    if (sym) {
                                        // Copy function pointer metadata from typedef
                                        sym->isFunctionPointer = true;
                                        sym->funcPtrReturnType = td->funcPtrReturnType;
                                        sym->funcPtrParamTypes = td->funcPtrParamTypes;
                                        sym->funcPtrParamIsReference = td->funcPtrParamIsReference;
                                        sym->funcPtrIsVariadic = td->funcPtrIsVariadic;
                                        // Set isLocal = true if we're inside a function
                                        sym->isLocal = !currentFunctionName.empty();
                                        
                                        if (nodee) {
                                            nodee->symbol = sym;
                                            cout << "DEBUG: Attached symbollll " << sym->name << " to AST node" << endl;
                                        }
                                    }
                                }
                                continue;  // Skip normal variable processing
                            }
                            
                            // Check if typedef is a function pointer array: BinaryOp ops[2]
                            if (td && td->isTypedef && td->isFunctionPointer && isArray) {
                                cout << "DEBUG: Variable '" << varName << "' declared as ARRAY of function pointer typedef '" << typedefNameUsed << "'\n";
                                
                                // Add symbol as array of function pointers
                                if (!symbolTable.addSymbol(varName, td->funcPtrReturnType, nodee, false, {}, true, arrayDimensions, 0)) {
                                    addError("Redeclaration of variable: " + varName);
                                } else {
                                    Symbol* sym = symbolTable.lookupCurrentScope(varName);
                                    if (sym) {
                                        // Copy function pointer metadata from typedef AND mark as array
                                        sym->isFunctionPointer = true;
                                        sym->isArray = true;
                                        sym->arrayDimensions = arrayDimensions;
                                        sym->funcPtrReturnType = td->funcPtrReturnType;
                                        sym->funcPtrParamTypes = td->funcPtrParamTypes;
                                        sym->funcPtrParamIsReference = td->funcPtrParamIsReference;
                                        sym->funcPtrIsVariadic = td->funcPtrIsVariadic;
                                        // Set isLocal = true if we're inside a function
                                        sym->isLocal = !currentFunctionName.empty();
                                        
                                        if (nodee) {
                                            nodee->symbol = sym;
                                            cout << "DEBUG: Attached symbollll " << sym->name << " to AST node" << endl;
                                        }
                                    }
                                }
                                continue;  // Skip normal variable processing
                            }
                        }
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
                        
                     

    checkStaticRedeclaration(varName, hasStatic);
                        // In processVariable - pass hasStatic to addSymbol
if (!symbolTable.addSymbol(varName, varType, nodee, false, {}, isArray, 
                          arrayDimensions, pointerDepth, false, false, false,
                          false, "", hasStatic, false)) {  // ✅ Pass static flag
    addError("Redeclaration of variable: " + varName);
} else {
    Symbol* sym = symbolTable.lookupCurrentScope(varName);
    if (sym && nodee) {
        nodee->symbol = sym;
        sym->isStatic = hasStatic;  // ✅ Ensure it's set
        // Set isLocal = true if we're inside a function
        sym->isLocal = !currentFunctionName.empty();
        cout << "DEBUG: Variable '" << varName << "' marked as " 
             << (hasStatic ? "static" : "non-static") << ", local=" << sym->isLocal << "\n";
    }
      // If this was a pointer-to-array declarator, store pointee array dimensions for IR sizing
    if (sym && !isArray && !arrayDimensions.empty() && pointerDepth > 0) {
        sym->pointeeArrayDimensions = arrayDimensions;
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
            // If the base is a DECLARATOR (e.g., int (*p)[3][3]), drill into it
            if (firstChild->name == "DECLARATOR") {
                return findIdentifierInDeclarator(firstChild);
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

vector<string> SemanticAnalyzer::extractFunctionParameters(Node* funcDeclNode, std::vector<bool>* outParamIsRef) {
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
                            bool isRefParam = false;
                            string typedefNameUsed;
                            
                            for (auto paramChild : paramDecl->children) {
                                if (paramChild->name == "DECL_SPECIFIERS" || 
                                    paramChild->name == "declaration_specifiers") {
                                    baseType = extractTypeFromDeclSpecifiers(paramChild);
                                    // Capture typedef name if present
                                    for (auto sc : paramChild->children) {
                                        if (sc->name == "TYPE_NAME") {
                                            typedefNameUsed = sc->lexeme;
                                        }
                                    }
                                }
                                else if (paramChild->name == "DECLARATOR") {
                                    analyzeDeclarator(paramChild, paramName, pointerDepth, isArray, arrayDims, true);
                                }
                                else if (paramChild->name == "ARRAY") {
                                    // ✅ Handle array parameters (allow incomplete dimensions like char argv[])
                                    isArray = true;
                                    arrayDims = extractArrayDimensions(paramChild, paramName, true);
                                }
                                else if (paramChild->name == "&") {
                                    isRefParam = true;
                                }
                            }
                            // If typedef alias provides array and no explicit ARRAY declarator, adopt it
                            if (!isArray && !typedefNameUsed.empty()) {
                                Symbol* td = symbolTable.lookup(typedefNameUsed);
                                if (td && td->isTypedef && td->isArray) {
                                    isArray = true;
                                    arrayDims = td->arrayDimensions;
                                }
                            }
                            
                            // ✅ Build full type
                            string fullType = baseType;
                            for (int i = 0; i < pointerDepth; i++) {
                                fullType += "*";
                            }
                            if (isArray) {
                                if (isRefParam) {
                                    // Reference to array: do NOT decay; encode all dimensions
                                    for (size_t i = 0; i < arrayDims.size(); i++) {
                                        if (arrayDims[i] == -1) fullType += "[]";
                                        else fullType += "[" + to_string(arrayDims[i]) + "]";
                                    }
                                } else {
                                    // Non-reference parameter: arrays decay to pointers
                                    fullType += "*";  // char argv[] → char*
                                    if (arrayDims.size() > 1) {
                                        // Preserve remaining dimensions after decay of the first
                                        for (size_t i = 1; i < arrayDims.size(); i++) {
                                            if (arrayDims[i] == -1) fullType += "[]";
                                            else fullType += "[" + to_string(arrayDims[i]) + "]";
                                        }
                                    }
                                }
                            }
                            if (!baseType.empty()) {
                                paramTypes.push_back(fullType);
                                cout << "DEBUG: Function param type: " << fullType << "\n";
                                if (outParamIsRef) outParamIsRef->push_back(isRefParam);
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
                            bool isRefParam = false;
                            string typedefNameUsed;
                            nodee=NULL;
                            for (auto paramChild : paramDecl->children) {
                                if (paramChild->name == "DECL_SPECIFIERS" || 
                                    paramChild->name == "declaration_specifiers") {
                                    baseType = extractTypeFromDeclSpecifiers(paramChild);
                                    for (auto sc : paramChild->children) {
                                        if (sc->name == "TYPE_NAME") typedefNameUsed = sc->lexeme;
                                    }
                                }
                                else if (paramChild->name == "DECLARATOR") {
                                    analyzeDeclarator(paramChild, paramName, pointerDepth, isArray, arrayDims, true);
                                     nodee = findIdentifierInDeclarator(paramChild);  // ✅ FIX!
                          
                                }
                                else if (paramChild->name == "ARRAY") {
                                     isArray=true;
                                    arrayDims = extractArrayDimensions(paramChild, paramName, true);
                                     nodee = findIdentifierInArray(paramChild);  // ✅ FIX!
                           
                                }
                                else if (paramChild->name == "IDENTIFIER") {
                                nodee=paramChild;
                                    paramName = paramChild->lexeme;
                                }
                                else if (paramChild->name == "&") {
                                    // Handle reference parameters
                                    isRefParam = true;
                                }
                            }
                            
                            // Adopt array dims from typedef alias if not explicitly declared as array
                            if (!isArray && !typedefNameUsed.empty()) {
                                Symbol* td = symbolTable.lookup(typedefNameUsed);
                                if (td && td->isTypedef && td->isArray) {
                                    isArray = true;
                                    arrayDims = td->arrayDimensions;
                                }
                            }

                            if (isArray && !isRefParam) {
                                // Normal parameter: decay array to pointer
                                pointerDepth += 1;  // array → pointer conversion
                                if (arrayDims.size() > 1) {
                                    // Keep subsequent dimensions
                                    vector<int> newDims(arrayDims.begin() + 1, arrayDims.end());
                                    arrayDims = newDims;
                                    isArray = true;
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
    // Mark parameter symbols declared with '&' as reference so IR treats them as addresses
    if (sym && isRefParam) {
        sym->isReference = true;
    }

                                    cout << "DEBUG: Added parameter " << paramName << " [" << baseType;
                                    for (int i = 0; i < pointerDepth; i++) cout << "*";
                                    if (isArray) {
                                        for (int dim : arrayDims) {
                                            cout << "[" << (dim == -1 ? "" : to_string(dim)) << "]";
                                        }
                                    }
                                    cout << "]";
                                    if (isRefParam) cout << " &";
                                    cout << "\n";
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
    // Handle built-in varargs helpers as no-op calls for semantics: va_start(list, last), va_end(list), va_copy(dst, src)
    if (funcNode && funcNode->name == "IDENTIFIER") {
        std::string name = funcNode->lexeme;
        if (name == "va_start" || name == "va_end" || name == "va_copy") {
            size_t argc = 0;
            if (node->children.size() > 1 && node->children[1]->name == "ARG_LIST") {
                argc = node->children[1]->children.size();
            }
            // Enforce exact arity
            if (name == "va_start" && argc != 2) {
                addError("'va_start' requires exactly two arguments: (va_list, lastNamedParam)");
            }
            if (name == "va_end" && argc != 1) {
                addError("'va_end' requires exactly one argument: (va_list)");
            }
            if (name == "va_copy" && argc != 2) {
                addError("'va_copy' requires exactly two arguments: (dest, src)");
            }
            // Do not perform normal function resolution/type checks
            return;
        }
    }
    
    // Determine callee signature and name for diagnostics
    std::vector<std::string> calleeParamTypes;
    std::vector<bool> calleeParamIsRef;
    bool calleeIsVariadic = false;
    std::string calleeName = "<expr>";
    std::string retType;
    
    if (funcNode->name == "IDENTIFIER") {
        calleeName = funcNode->lexeme;
        Symbol* sym = symbolTable.lookup(funcNode->lexeme);
        if (!sym) { addError("Undeclared function: '" + calleeName + "'"); return; }
        if (sym->isFunction) {
            calleeParamTypes = sym->paramTypes;
            calleeParamIsRef = sym->paramIsReference;
            calleeIsVariadic = sym->isVariadic;
            retType = sym->type;
        } else if (sym->isFunctionPointer) {
            calleeParamTypes = sym->funcPtrParamTypes;
            calleeParamIsRef = sym->funcPtrParamIsReference;
            calleeIsVariadic = sym->funcPtrIsVariadic;
            retType = sym->funcPtrReturnType;
        } else {
            addError("'" + calleeName + "' is not a function or function pointer");
            return;
        }
    } else if (funcNode->name == "UNARY_OP" && funcNode->children.size() == 2 && funcNode->children[0]->name == "*") {
        // Handle (*fp)(...)
        Node* inner = funcNode->children[1];
        // Unwrap IDENTIFIER
        if (inner && inner->name == "IDENTIFIER") {
            calleeName = inner->lexeme;
            Symbol* sym = symbolTable.lookup(inner->lexeme);
            if (!sym) { addError("Undeclared identifier: '" + calleeName + "'"); return; }
            if (sym->isFunctionPointer) {
                calleeParamTypes = sym->funcPtrParamTypes;
                calleeParamIsRef = sym->funcPtrParamIsReference;
                calleeIsVariadic = sym->funcPtrIsVariadic;
                retType = sym->funcPtrReturnType;
            } else {
                addError("Cannot call through non-function pointer '" + calleeName + "'");
                return;
            }
        } else {
            // Other complex expressions not supported yet
            addError("Unsupported function pointer call expression");
            return;
        }
    } else {
        // Not supported callee form
        return;
    }
 
    // Extract actual arguments
vector<Node*> argNodes;
if (node->children.size() > 1 && node->children[1]->name == "ARG_LIST") {
    argNodes = node->children[1]->children;
}

    size_t expectedParams = calleeParamTypes.size();
    size_t actualArgs = argNodes.size();

    // Arity rules: exact for non-variadic, >= fixed for variadic
    if (!calleeIsVariadic) {
        if (actualArgs != expectedParams) {
            addError("Function '" + calleeName + "' expects " + 
                     to_string(expectedParams) + " argument(s), but " + 
                     to_string(actualArgs) + " provided");
            return;
        }
    } else {
        if (actualArgs < expectedParams) {
            addError("Variadic function '" + calleeName + "' requires at least " + 
                     to_string(expectedParams) + " fixed argument(s), but " + 
                     to_string(actualArgs) + " provided");
            return;
        }
    }

    // Check fixed arguments strictly (no narrowing) and enforce reference requirements
    for (size_t i = 0; i < std::min(actualArgs, expectedParams); i++) {
        string argType = getExpressionType(argNodes[i]);
        string paramType = calleeParamTypes[i];
        bool paramIsRef = (i < calleeParamIsRef.size() && calleeParamIsRef[i]);

        if (paramIsRef) {
            // For reference params: require an lvalue
            if (!isLvalue(argNodes[i])) {
                addError("Argument " + to_string(i + 1) + " of function '" + calleeName + "' must be an lvalue for reference parameter");
                continue;
            }

            auto parseArrayType = [&](const string& s, string& base, vector<int>& dims) -> bool {
                size_t pos = s.find('[');
                if (pos == string::npos) { base = s; return false; }
                base = s.substr(0, pos);
                dims.clear();
                size_t i = pos;
                while (i < s.size()) {
                    size_t lb = s.find('[', i);
                    if (lb == string::npos) break;
                    size_t rb = s.find(']', lb);
                    if (rb == string::npos) break;
                    if (rb == lb + 1) {
                        dims.push_back(-1);
                    } else {
                        int val = stoi(s.substr(lb + 1, rb - lb - 1));
                        dims.push_back(val);
                    }
                    i = rb + 1;
                }
                return true;
            };

            string pBase; vector<int> pDims;
            bool pIsArray = parseArrayType(paramType, pBase, pDims) && !pDims.empty();
            if (pIsArray) {
                // Expect argument to be an array lvalue with matching base and dims
                Node* arg = argNodes[i];
                // Unwrap simple EXPR_LIST
                if (arg && arg->name == "EXPR_LIST" && !arg->children.empty()) arg = arg->children[0];
                Symbol* argSym = (arg && arg->name == "IDENTIFIER") ? symbolTable.lookup(arg->lexeme) : nullptr;
        if (!arg || arg->name != "IDENTIFIER" || !argSym || !argSym->isArray) {
            addError("Type mismatch for reference argument " + to_string(i + 1) +
                 " of function '" + calleeName + "': expected array lvalue '" + paramType + "'");
                    continue;
                }
                string aBase = resolveTypedef(argSym->type);
                if (aBase != pBase) {
                    addError("Type mismatch for reference argument " + to_string(i + 1) +
                             ": expected base '" + pBase + "' but got '" + aBase + "'");
                    continue;
                }
                const vector<int>& aDims = argSym->arrayDimensions;
                if (aDims.size() != pDims.size()) {
                    addError("Type mismatch for reference argument " + to_string(i + 1) +
                             ": array rank mismatch");
                    continue;
                }
                for (size_t di = 0; di < pDims.size(); ++di) {
                    if (pDims[di] > 0 && aDims[di] != pDims[di]) {
                        addError("Type mismatch for reference argument " + to_string(i + 1) +
                                 ": dimension " + to_string(di+1) + " expected " + to_string(pDims[di]) +
                                 ", got " + to_string(aDims[di]));
                        break;
                    }
                }
            } else {
                // Scalar/pointer reference: require exact type match
                if (argType != paramType) {
                    addError("Type mismatch for reference argument " + to_string(i + 1) +
                             " of function '" + calleeName + "': expected '" + paramType +
                             "', but got '" + argType + "'");
                }
            }
        } else {
            if (!areFunctionArgCompatible(argType, paramType)) {
                addError("Type mismatch for argument " + to_string(i + 1) +
                         " of function '" + calleeName + "': expected '" + paramType +
                         "', but got '" + argType + "'");
            }
        }
    }

    // For extra variadic arguments, apply default argument promotions and basic validity checks
    if (calleeIsVariadic && actualArgs > expectedParams) {
        for (size_t i = expectedParams; i < actualArgs; ++i) {
            Node* arg = argNodes[i];
            string argType = getExpressionType(arg);
            // Disallow void and function types (not modeled) and empty types
            if (argType.empty() || argType == "void") {
                addError("Invalid variadic argument " + to_string(i + 1) + ": type cannot be '" + (argType.empty()?string("<unknown>"):argType) + "'");
                continue;
            }
            // In C default promotions: float -> double; char/short/bool -> int
            auto isIntegralLike = [&](const string& t){ return t == "char" || t == "signed char" || t == "unsigned char" || t == "short" || t == "unsigned short" || t == "bool" || t == "int" || t == "unsigned int" || t == "long" || t == "unsigned long" || t == "long long" || t == "unsigned long long"; };
            if (argType == "float") {
                // Allowed (promotes to double at call boundary)
                continue;
            }
            if (isIntegralLike(argType)) {
                // Allowed (promotes at least to int)
                continue;
            }
            // Pointers are allowed
            if (isPointerType(argType)) {
                continue;
            }
            // Struct/union by value: allow but warn? For strictness, allow as many ABIs do; we don't error here.
            if (argType.rfind("struct ", 0) == 0 || argType.rfind("union ", 0) == 0) {
                // Accept - runtime callee must va_arg with correct type
                continue;
            }
            // Otherwise accept conservatively
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

    // Handle array-to-pointer decay for function arguments
    // In C/C++, when passing arrays to functions, they decay to pointers
    // Example: int arr[5] decays to int* when passed to function(int* param)
    //          int mat[3][4] decays to int(*)[4] when passed to function(int (*param)[4])
    if (isArrayType(argType) && isPointerType(paramType)) {
        string arrayElementType = getArrayElementType(argType);
        string pointerElementType = getBaseTypeFromPointer(paramType);
        if (arrayElementType == pointerElementType) {
            return true; // Valid array decay
        }
    }

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
    // Treat stdarg built-ins as special forms even without declarations
    if ((node->lexeme == "va_start" || node->lexeme == "va_end" || node->lexeme == "va_copy")) {
        // Only skip error if used as a call callee
        if (node->parent && node->parent->name == "FUNC_CALL" && !node->parent->children.empty() && node->parent->children[0] == node) {
            return;
        }
    }
    
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

            // Special rule: returning integer to pointer is only allowed for literal 0 (NULL) or nullptr
            if (isPointerType(funcRetType) && isIntegerType(retExprType)) {
                bool isNullConstant = (expr && ((expr->name == "INTEGER_CONSTANT" && expr->lexeme == "0") ||
                                                 expr->name == "NULLPTR_CONSTANT"));
                if (!isNullConstant) {
                    addError("Return type mismatch in function '" + currentFunctionName +
                             "': expected '" + funcRetType + "', but got '" + retExprType + "'");
                    return;
                }
                // else OK: returning 0/nullptr to pointer type
            } else if (!areTypesCompatible(funcRetType, retExprType, "=")) {
                addError("Return type mismatch in function '" + currentFunctionName +
                         "': expected '" + funcRetType + "', but got '" + retExprType + "'");
            }
            
            // Check for returning address of local variable (dangling pointer)
            if (isPointerType(funcRetType)) {
                checkReturnLocalAddress(expr);
            }
        }
    }
}

void SemanticAnalyzer::checkReturnLocalAddress(Node* expr) {
    if (!expr) return;
    
    // Check for &identifier pattern (address-of local variable)
    if (expr->name == "UNARY_OP" && expr->children.size() >= 2) {
        Node* opNode = expr->children[0];
        Node* opnd = expr->children[1];
        
        if (opNode && opNode->name == "&" && opnd && opnd->name == "IDENTIFIER") {
            Symbol* sym = symbolTable.lookup(opnd->lexeme);
            if (sym && sym->isLocal) {
                addError("Cannot return address of local variable '" + opnd->lexeme + "' (dangling pointer)");
                return;
            }
        }
    }
    
    // Check for &array[index] pattern (address-of local array element)
    if (expr->name == "UNARY_OP" && expr->children.size() >= 2) {
        Node* opNode = expr->children[0];
        Node* opnd = expr->children[1];
        
        if (opNode && opNode->name == "&" && opnd && opnd->name == "ARRAY_ACCESS") {
            // Get the base array identifier
            Node* arrayBase = opnd;
            while (arrayBase && arrayBase->name == "ARRAY_ACCESS" && !arrayBase->children.empty()) {
                arrayBase = arrayBase->children[0];
            }
            
            if (arrayBase && arrayBase->name == "IDENTIFIER") {
                Symbol* sym = symbolTable.lookup(arrayBase->lexeme);
                if (sym && sym->isLocal) {
                    addError("Cannot return address of local array element '" + arrayBase->lexeme + "[...]' (dangling pointer)");
                    return;
                }
            }
        }
    }
    
    // Check for direct identifier (returning local array)
    if (expr->name == "IDENTIFIER") {
        Symbol* sym = symbolTable.lookup(expr->lexeme);
        if (sym && sym->isLocal && sym->isArray) {
            addError("Cannot return local array '" + expr->lexeme + "' (arrays are not returnable)");
            return;
        }
    }
}

void SemanticAnalyzer::analyzeDeclarator(Node* node, string& name, int& pointerDepth,
                                         bool& isArray, vector<int>& arrayDims, bool allowIncomplete) {
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
        arrayDims = extractArrayDimensions(node, name, allowIncomplete);
        return;  // Don't recurse further, extractArrayDimensions handles it
    }
    
    for (auto child : node->children) {
        analyzeDeclarator(child, name, pointerDepth, isArray, arrayDims, allowIncomplete);
    }
}
vector<int> SemanticAnalyzer::extractArrayDimensions(Node* arrayNode, string& varName, bool allowIncomplete) {
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
            } else {
                // No size specified - incomplete array dimension
                // For function parameters, this is allowed (char argv[] or char* argv[])
                if (!allowIncomplete) {
                    addError("Incomplete array dimension: array size must be specified");
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
    
    // Handle DECLARATOR nodes (e.g., in initialization: char* z = ...)
    // Traverse to find the IDENTIFIER and return its type
    if (node->name == "DECLARATOR") {
        Node* idNode = findIdentifierInDeclarator(node);
        if (idNode && idNode->name == "IDENTIFIER") {
            Symbol* sym = symbolTable.lookup(idNode->lexeme);
            if (sym) {
                string type = resolveTypedef(sym->type);
                if (sym->isArray) {
                    // Build the full array type
                    string fullType = type;
                    for (int dim : sym->arrayDimensions) {
                        fullType += "[" + (dim > 0 ? to_string(dim) : "") + "]";
                    }
                    return fullType;
                }
                // Check if this is a pointer-to-array
                if (!sym->isArray && sym->pointerDepth > 0 && !sym->pointeeArrayDimensions.empty()) {
                    string fullType = type + "(";
                    for (int i = 0; i < sym->pointerDepth; i++) {
                        fullType += "*";
                    }
                    fullType += ")";
                    for (int dim : sym->pointeeArrayDimensions) {
                        fullType += "[" + (dim > 0 ? to_string(dim) : "") + "]";
                    }
                    return fullType;
                }
                for (int i = 0; i < sym->pointerDepth; i++) {
                    type += "*";
                }
                return type;
            }
        }
        return "";
    }
  
     if (node->name == "IDENTIFIER") {
        Symbol* sym = symbolTable.lookup(node->lexeme);
        if (!sym){ return "";}
        
        string type = resolveTypedef(sym->type);
        
        // For arrays, we need to return the full array type, not the decayed pointer type
        // This allows proper type checking in assignments
        if (sym->isArray) {
            // Build the full array type: base_type[dim1][dim2]...
            // IMPORTANT: For arrays of pointers (e.g., int* arr[5]), we need to apply
            // the pointerDepth to the base type first before adding array dimensions
            // sym->type = "int", pointerDepth = 1 → "int*[5]"
            string fullType = type;
            for (int i = 0; i < sym->pointerDepth; i++) {
                fullType += "*";
            }
            
            // Then add array dimensions
            for (int dim : sym->arrayDimensions) {
                fullType += "[" + (dim > 0 ? to_string(dim) : "") + "]";
            }
            return fullType;
        }
        
        // Check if this is a pointer-to-array: int (*p)[4]
        // In this case, we need to represent it as pointer notation with array dimensions
        if (!sym->isArray && sym->pointerDepth > 0 && !sym->pointeeArrayDimensions.empty()) {
            // Build pointer-to-array type: int(*)[4] or int(*)[3][4]
            string fullType = type + "(";
            for (int i = 0; i < sym->pointerDepth; i++) {
                fullType += "*";
            }
            fullType += ")";
            for (int dim : sym->pointeeArrayDimensions) {
                fullType += "[" + (dim > 0 ? to_string(dim) : "") + "]";
            }
            return fullType;
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
    else if (node->name == "NULLPTR_CONSTANT") return "void*";  // nullptr is a null pointer constant
    else if (node->name == "NULL_CONSTANT") return "void*";     // ← ADD THIS LINE
    else if (node->name == "HEX_INT_LITERAL" || 
             node->name == "OCTAL_INT_LITERAL" || 
             node->name == "BINARY_INT_LITERAL") return "int";
    else if (node->name == "HEX_FLOAT_LITERAL") return "float";
    else if (node->name == "FUNC_CALL") {
        if (!node->children.empty()) {
            Node* callee = node->children[0];
            if (callee->name == "IDENTIFIER") {
                Symbol* funcSym = symbolTable.lookup(callee->lexeme);
                if (!funcSym) return "";
                if (funcSym->isFunction) return funcSym->type;
                if (funcSym->isFunctionPointer) return funcSym->funcPtrReturnType;
            } else if (callee->name == "UNARY_OP" && callee->children.size() == 2 && callee->children[0]->name == "*") {
                Node* inner = callee->children[1];
                if (inner && inner->name == "IDENTIFIER") {
                    Symbol* s = symbolTable.lookup(inner->lexeme);
                    if (s && s->isFunctionPointer) return s->funcPtrReturnType;
                }
            }
        }
    }
    else if (node->name == "CAST_EXPR") {
        // Structure: CAST_EXPR -> SPEC_QUAL_LIST (or TYPE_NAME), <expr>
        for (auto c : node->children) {
            if (!c) continue;
            if (c->name == "SPEC_QUAL_LIST" || c->name == "TYPE_NAME") {
                return extractTypeFromTypeName(c);
            }
        }
        return "";
    }
    else if (node->name == "VA_ARG") {
        // node children: [0] va_list expr, [1] type_name
        if (node->children.size() >= 2) {
            return extractTypeFromTypeName(node->children[1]);
        }
        return "int";
    }
      if (node->name == "SIZEOF" || node->name == "SIZEOF_TYPE") {
        return "int";  // sizeof returns an integer type
    }
    // Handle array access - returns element type
    else if (node->name == "ARRAY_ACCESS") {
        if (!node->children.empty()) {
            string baseType = getExpressionType(node->children[0]);
            
            // Handle array types: implement proper array-to-pointer decay
            // For multidimensional arrays, we need to check how many dimensions are being accessed
            Node* baseNode = node->children[0];
            
            // Find the original array identifier by traversing up the ARRAY_ACCESS chain
            Node* originalArray = node;
            while (originalArray && originalArray->name == "ARRAY_ACCESS") {
                if (originalArray->children.empty()) break;
                originalArray = originalArray->children[0];
            }
            
            if (originalArray && originalArray->name == "IDENTIFIER") {
                Symbol* sym = symbolTable.lookup(originalArray->lexeme);
                if (sym && sym->isArray) {
                    // Count how many array access levels we have
                    int accessLevels = 1; // current ARRAY_ACCESS counts as 1
                    Node* current = node;
                    while (current && current->name == "ARRAY_ACCESS") {
                        if (current->children.empty()) break;
                        current = current->children[0];
                        if (current && current->name == "ARRAY_ACCESS") {
                            accessLevels++;
                        }
                    }
                    
                    // Calculate remaining dimensions after access
                    int remainingDims = static_cast<int>(sym->arrayDimensions.size()) - accessLevels;
                    
                    if (remainingDims > 0) {
                        // Still have array dimensions left - return array type
                        // For arr[1] where arr is int[2][3]: result should be int[3] (array of 3 ints)
                        // For arr[1][2] where arr is int[2][3][4]: result should be int[4] (array of 4 ints)
                        string resultType = sym->type;
                        for (int i = 0; i < remainingDims; i++) {
                            resultType += "[" + to_string(sym->arrayDimensions[accessLevels + i]) + "]";
                        }
                        return resultType;
                    } else {
                        // All dimensions consumed - return element type
                        // For arr1d[2] where arr1d is int[5]: result should be int
                        string resultType = sym->type;
                        for (int i = 0; i < sym->pointerDepth; i++) {
                            resultType += "*";
                        }
                        return resultType;
                    }
                }
            }
            
            // Handle pointer types: remove one level of pointer
            if (!baseType.empty() && baseType.back() == '*') {
                return baseType.substr(0, baseType.length() - 1);
            }
            
            // Fallback: If baseType encodes an array (e.g., "struct B[2]"), strip one dimension
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
            
            // ✅ CHECK: Void pointer arithmetic is forbidden in standard C
            if (isPointerType(leftType) && isIntegerType(rightType)) {
                // Check if it's void*
                string baseType = leftType.substr(0, leftType.length() - 1); // Remove trailing '*'
                if (baseType == "void" || baseType.find("void*") == 0) {
                    addError("Pointer arithmetic on 'void*' is not allowed (void has no size)");
                    return leftType; // Return type anyway to continue analysis
                }
                return leftType;
            }
            // int + pointer = pointer
            if (node->name == "ADD_EXPR" && isIntegerType(leftType) && isPointerType(rightType)) {
                // Check if it's void*
                string baseType = rightType.substr(0, rightType.length() - 1); // Remove trailing '*'
                if (baseType == "void" || baseType.find("void*") == 0) {
                    addError("Pointer arithmetic on 'void*' is not allowed (void has no size)");
                    return rightType; // Return type anyway to continue analysis
                }
                return rightType;
            }
            // pointer - pointer = ptrdiff_t (treated as int)
            if (node->name == "SUB_EXPR" && isPointerType(leftType) && isPointerType(rightType)) {
                // Check if either is void*
                string baseType1 = leftType.substr(0, leftType.length() - 1);
                string baseType2 = rightType.substr(0, rightType.length() - 1);
                if (baseType1 == "void" || baseType1.find("void*") == 0 || 
                    baseType2 == "void" || baseType2.find("void*") == 0) {
                    addError("Pointer arithmetic on 'void*' is not allowed (void has no size)");
                }
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

// (moved) extractTypeFromTypeName is defined at top-level after getExpressionType
    // For other expressions, recursively check first child
    else if (!node->children.empty()) {
        return getExpressionType(node->children[0]);
    }
    
    return "";
}

// Extract a canonical type string from a type_name or SPEC_QUAL_LIST node
std::string SemanticAnalyzer::extractTypeFromTypeName(Node* typeNode) {
    if (!typeNode) return "int";
    // Direct TYPE_NAME node with lexeme already resolved by lexer/typedef
    if (typeNode->name == "TYPE_NAME" && !typeNode->lexeme.empty()) {
        return resolveTypedef(typeNode->lexeme);
    }
    // TYPE_NAME wrapper with children (SPEC_QUAL_LIST and possibly POINTER)
    Node* sq = nullptr;
    Node* ptr = nullptr;
    if (typeNode->name == "TYPE_NAME") {
        for (auto c : typeNode->children) {
            if (c && c->name == "SPEC_QUAL_LIST") { sq = c; }
            if (c && c->name == "POINTER") { ptr = c; }
        }
    } else if (typeNode->name == "SPEC_QUAL_LIST") {
        sq = typeNode;
    }
    if (!sq) return "int";
    
    // Count pointer depth
    int pointerDepth = 0;
    if (ptr) {
        // POINTER node itself represents one level of indirection
        // Check if it has nested POINTER children for multiple levels
        std::function<int(Node*)> countPointers = [&](Node* n) -> int {
            if (!n) return 0;
            int count = 0;
            if (n->name == "POINTER") count = 1;
            for (auto child : n->children) {
                count += countPointers(child);
            }
            return count;
        };
        pointerDepth = countPointers(ptr);
    }
    
    // Aggregate builtin specifiers similar to extractTypeFromDeclSpecifiers
    int longCount = 0; bool isShort = false, isUnsigned = false, isSigned = false;
    std::string base;
    std::function<void(Node*)> walk = [&](Node* n){
        if (!n) return;
        if (n->name == "TYPE_SPECIFIER") {
            std::string t = resolveTypedef(n->lexeme);
            if (t == "long") longCount++;
            else if (t == "short") isShort = true;
            else if (t == "unsigned") isUnsigned = true;
            else if (t == "signed") isSigned = true;
            else if (t == "int" || t == "char" || t == "float" || t == "double" || t == "void" || t == "bool") base = t;
            else base = t; // typedef or tag
        }
        else if (n->name == "SPEC_QUAL_LIST") {
            for (auto c : n->children) walk(c);
        }
    };
    walk(sq);
    if (base.empty() && (isShort || isUnsigned || isSigned || longCount > 0)) base = "int";
    if (base.empty()) base = "int";
    // Build final type
    std::string result;
    if (base == "char") {
        if (isUnsigned) result = "unsigned char";
        else if (isSigned) result = "signed char";
        else result = "char";
    }
    else if (base == "int") {
        if (isUnsigned) {
            if (isShort) result = "unsigned short";
            else if (longCount >= 2) result = "unsigned long long";
            else if (longCount == 1) result = "unsigned long";
            else result = "unsigned int";
        } else {
            if (isShort) result = "short";
            else if (longCount >= 2) result = "long long";
            else if (longCount == 1) result = "long";
            else result = "int";
        }
    }
    else if (base == "double") result = "double";
    else if (base == "float") result = "float";
    else if (base == "void") result = "void";
    else result = base;
    
    // Append pointer stars
    for (int i = 0; i < pointerDepth; i++) {
        result += "*";
    }
    
    return result;
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
                        string memberName = firstChild->lexeme;
                        identifierNode = firstChild;
                        // ✅ FIX: Add to members map AND memberOrder (no fallback needed)
                        members[memberName] = memberType;
                        memberNodes[memberName] = identifierNode;
                        memberOrder.push_back(memberName);
                        cout << "DEBUG: Found struct member (IDENTIFIER): " << memberName << " [" << memberType << "]\n";
                        continue;  // ✅ Skip fallback loop to prevent duplicate
                    }
                    // Handle function pointer members in struct
                    else if (firstChild->name == "FUNCTION_DECL") {
                        string funcName;
                        vector<string> paramTypes;
                        vector<bool> paramIsRef;
                        
                        // Extract function pointer name and parameters
                        for (auto funcChild : firstChild->children) {
                            if (funcChild->name == "DECLARATOR") {
                                // Get identifier from declarator (handles pointer)
                                identifierNode = findIdentifierInDeclarator(funcChild);
                                if (identifierNode) {
                                    funcName = identifierNode->lexeme;
                                }
                            } else if (funcChild->name == "IDENTIFIER") {
                                funcName = funcChild->lexeme;
                                identifierNode = funcChild;
                            } else if (funcChild->name == "PARAM_TYPE_LIST") {
                                // Extract parameter types using existing function
                                paramTypes = extractFunctionParameters(firstChild, &paramIsRef);
                            }
                        }
                        
                        if (!funcName.empty()) {
                            // Build function pointer type: returnType (*)(paramTypes)
                            string fullType = memberType + " (*)";
                            fullType += "(";
                            for (size_t i = 0; i < paramTypes.size(); i++) {
                                fullType += paramTypes[i];
                                if (i < paramTypes.size() - 1) fullType += ", ";
                            }
                            fullType += ")";
                            
                            members[funcName] = fullType;
                            memberNodes[funcName] = identifierNode;
                            memberOrder.push_back(funcName);
                            cout << "DEBUG: Found struct function pointer member: " << funcName << " [" << fullType << "]\n";
                            continue;
                        }
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
            // ✅ FIX: Only add if not already in memberOrder (prevent duplicates)
            bool alreadyInOrder = false;
            for (const auto& existing : memberOrder) {
                if (existing == pair.first) {
                    alreadyInOrder = true;
                    break;
                }
            }
            if (!alreadyInOrder) {
                members[pair.first] = memberType;
                memberNodes[pair.first] = pair.second;  // ✅ STORE NODE!
                memberOrder.push_back(pair.first);
                cout << "DEBUG: Found struct member: " << pair.first << " [" << memberType << "]\n";
            }
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
    
    // For anonymous enums, we still need to process the enumerators
    // even though there's no enum tag name
    bool isAnonymous = enumName.empty();
    
    // Lookup if this enum was previously declared (only for named enums)
    Symbol* existingSym = nullptr;
    if (!isAnonymous) {
        existingSym = symbolTable.lookup(enumName);
    }
    
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
                if (isAnonymous) {
                    // Anonymous enum - just register the constants, no enum tag
                    for (const auto& pair : values) {
                        enumConstants[pair.first] = pair.second;
                        Node* enumNode = enumeratorNodes[pair.first];
                        if (!symbolTable.addSymbol(pair.first, "int", enumNode)) {
                            addError("Redefinition of enumerator '" + pair.first + "'");
                        } else {
                            Symbol* sym = symbolTable.lookupCurrentScope(pair.first);
                            if (sym && enumNode) {
                                enumNode->symbol = sym;
                                cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
                            }
                            cout << "DEBUG: Added enum constant " << pair.first << " = " << pair.second << "\n";
                        }
                    }
                    cout << "DEBUG: Defined anonymous enum with " << values.size() << " value(s)\n";
                } else if (symbolTable.addSymbol(enumName, "enum", nodee, false, {}, false, {}, 0, false, true)) {
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
    if (sym && nodee) {
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
        if (isAnonymous) {
            // Anonymous enum forward declaration - nothing to do
            return;
        }
        
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
    // In C, bool is an integer type (from stdbool.h) that gets promoted to int
    return type == "int" || type == "short" || type == "long" || type == "long long" ||
           type == "unsigned" || type == "unsigned int" || type == "unsigned short" || 
           type == "unsigned long" || type == "signed" || type == "char" || type == "bool" ||
           type == "unsigned char" || type == "signed char";
}
bool SemanticAnalyzer::isNumericType(const std::string& type) {
    return isIntegerType(type) || type == "float" || type == "double" || type.rfind("enum ", 0) == 0;;
}

bool SemanticAnalyzer::isPointerType(const string& type) {
    return type.find("*") != string::npos ;
}

bool SemanticAnalyzer::isArrayType(const string& type) {
    return !type.empty() && type.find('[') != string::npos && type.find(']') != string::npos;
}

string SemanticAnalyzer::getArrayElementType(const string& arrayType) {
    if (!isArrayType(arrayType)) return "";
    
    // Find the first '[' and the matching ']' to remove ONLY the first dimension
    // For int[3][4], this should return int[4], not int
    // For int[5], this should return int
    size_t firstBracket = arrayType.find('[');
    if (firstBracket != string::npos) {
        size_t closeBracket = arrayType.find(']', firstBracket);
        if (closeBracket != string::npos) {
            // Return base type + remaining dimensions (if any)
            string baseType = arrayType.substr(0, firstBracket);
            string remainingDims = arrayType.substr(closeBracket + 1);
            return baseType + remainingDims;
        }
    }
    return "";
}

void SemanticAnalyzer::validateArrayInitialization(Symbol* arraySym, Node* initList) {
    if (!arraySym || !arraySym->isArray || !initList) return;
    
    // Calculate expected total elements
    int expectedElements = 1;
    for (int dim : arraySym->arrayDimensions) {
        if (dim <= 0) return; // Skip validation if dimensions are unknown
        expectedElements *= dim;
    }
    
    // Count actual elements in the initialization list
    int actualElements = countInitializerElements(initList);
    
    // Note: Incomplete initialization is VALID in C (remaining elements are zero-initialized)
    // Only check for TOO MANY initializers, which is handled in checkArrayInitializerSize
    if (actualElements > expectedElements) {
        addError("Too many initializers for array: expected " + to_string(expectedElements) + 
                " elements, but " + to_string(actualElements) + " provided");
    }
}

int SemanticAnalyzer::countInitializerElements(Node* node) {
    if (!node) return 0;
    
    if (node->name == "INIT_LIST") {
        int count = 0;
        for (Node* child : node->children) {
            count += countInitializerElements(child);
        }
        return count;
    } else if (node->name == "INIT_LIST_EMPTY") {
        return 0;
    } else {
        // Single element (not a list)
        return 1;
    }
}

void SemanticAnalyzer::checkArrayInitializerSize(Node* initList, const vector<int>& dimensions, 
                                                   const string& arrayName, size_t dimIndex) {
    if (!initList || dimIndex >= dimensions.size()) return;
    
    int declaredSize = dimensions[dimIndex];
    if (declaredSize <= 0) return; // Skip if dimension is unknown
    
    int numInitializers = initList->children.size();
    
    // Check if we have too many initializers at this level
    if (numInitializers > declaredSize) {
        string dimStr = (dimensions.size() == 1) ? "" : " at dimension " + to_string(dimIndex + 1);
        addError("Too many initializers for array '" + arrayName + "'" + dimStr + 
                ". Declared size is " + to_string(declaredSize) + 
                " but initializer list has " + to_string(numInitializers) + " elements.");
        return; // Don't check nested levels if this level already failed
    }
    
    // For multidimensional arrays, recursively check nested initializers
    if (dimIndex + 1 < dimensions.size()) {
        for (Node* child : initList->children) {
            if (child && child->name == "INIT_LIST") {
                checkArrayInitializerSize(child, dimensions, arrayName, dimIndex + 1);
            }
        }
    }
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
        
          if ((type1.find("enum") == 0 && isIntegerType(type2)) || (type2.find("enum") == 0 && isIntegerType(type1))) {
        return true;
    }
    
    // Enum to enum of same type
    if (type1.find("enum") == 0 && type2.find("enum") == 0) {
        return true;  // Allow enum to enum comparisons
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
        
// CASE 6: Pointer with integer - NOT allowed here
// NULL checks are validated separately in checkComparisonOperation with node access
if ((isPointerType(type1) && isIntegerType(type2)) ||
    (isPointerType(type2) && isIntegerType(type1))) {
    return false;  // Reject - specific validation happens in checkComparisonOperation
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

        bool type1IsEnum = (type1.find("enum") == 0);
        bool type2IsEnum = (type2.find("enum") == 0);
        if (type1IsEnum || type2IsEnum) {
            if (type1IsEnum && type2IsEnum) {
                return true;  // Allow enum to enum conversions
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
    
    // Check for function pointer to integer comparison (warning-level issue)
    bool leftIsFuncPtr = false;
    bool rightIsFuncPtr = false;
    
    if (left->name == "IDENTIFIER") {
        Symbol* sym = symbolTable.lookup(left->lexeme);
        if (sym && sym->isFunctionPointer) leftIsFuncPtr = true;
    }
    if (right->name == "IDENTIFIER") {
        Symbol* sym = symbolTable.lookup(right->lexeme);
        if (sym && sym->isFunctionPointer) rightIsFuncPtr = true;
    }
    
  
    if (leftIsFuncPtr && isIntegerType(rightType) && !isPointerType(rightType)) {
    if (!((right->name == "INTEGER_CONSTANT" && right->lexeme == "0") || 
          right->name == "NULLPTR_CONSTANT" ||     // ← existing
          right->name == "NULL_CONSTANT")) {       // ← ADD THIS
        addError("Comparing function pointer to integer constant (not NULL) in operation '" + operation + "'");
    }
}
    if (rightIsFuncPtr && isIntegerType(leftType) && !isPointerType(leftType)) {
        if (!((left->name == "INTEGER_CONSTANT" && left->lexeme == "0") || 
              left->name == "NULLPTR_CONSTANT")) {  // Allow NULL/nullptr comparison
            addError("Comparing function pointer to integer constant (not NULL) in operation '" + operation + "'");
        }
    }
    
    // Check if types are comparable
    if (!isNumericType(leftType) && !isPointerType(leftType)) {
        addError("Invalid left operand type '" + leftType + "' for comparison '" + operation + "'");
    }
    if (!isNumericType(rightType) && !isPointerType(rightType)) {
        addError("Invalid right operand type '" + rightType + "' for comparison '" + operation + "'");
    }
    
   
    // ✅ NEW: Pointer-integer comparison validation (before areTypesCompatible)
if (isPointerType(leftType) && isIntegerType(rightType)) {
    // Only allow comparing pointer to literal 0 (NULL)
    if (!((right->name == "INTEGER_CONSTANT" && right->lexeme == "0") || 
          right->name == "NULLPTR_CONSTANT" ||
          right->name == "NULL_CONSTANT")) {
        addError("Invalid comparison between pointer type '" + leftType + 
                 "' and non-pointer type '" + rightType + "' in operation '" + operation + "'");
    }
    return;  // Exit - don't call areTypesCompatible
}

if (isPointerType(rightType) && isIntegerType(leftType)) {
    // Only allow comparing pointer to literal 0 (NULL)
    if (!((left->name == "INTEGER_CONSTANT" && left->lexeme == "0") || 
          left->name == "NULLPTR_CONSTANT" ||
          left->name == "NULL_CONSTANT")) {
        addError("Invalid comparison between non-pointer type '" + leftType + 
                 "' and pointer type '" + rightType + "' in operation '" + operation + "'");
    }
    return;  // Exit - don't call areTypesCompatible
}

// Check compatibility (for other cases)
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
    
    // Skip if RHS is EMPTY (uninitialized declaration)
    if (rhs && rhs->name == "EMPTY") {
        return;
    }
    
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
            cout << "DEBUG: Found INIT_LIST in declaration, checking array validation" << endl;
            cout << "DEBUG: LHS node name: " << (lhs ? lhs->name : "null") << endl;
            
            // Extract identifier from LHS (could be IDENTIFIER or ARRAY node)
            string varName;
            if (lhs->name == "IDENTIFIER") {
                varName = lhs->lexeme;
            } else if (lhs->name == "ARRAY") {
                // Extract identifier from ARRAY node
                Node* idNode = findIdentifierInArray(lhs);
                if (idNode) {
                    varName = idNode->lexeme;
                }
            }
            
            if (!varName.empty()) {
                Symbol* lhsSym = symbolTable.lookup(varName);
                cout << "DEBUG: Found variable: " << varName << endl;
                if (lhsSym && lhsSym->isArray) {
                    cout << "DEBUG: Variable is array" << endl;
                    // Note: Array initializer size checking is now done in processDeclaration
                    // via checkArrayInitializerSize(), which handles multidimensional arrays
                } else {
                    cout << "DEBUG: Variable is not array or symbol not found" << endl;
                }
            } else {
                cout << "DEBUG: Could not extract variable name from LHS" << endl;
            }
            return;
        }
    }
    
    string lhsType = getExpressionType(lhs);
    string rhsType = getExpressionType(rhs);
    // Check for array-to-array assignment (should be error)
    if (lhs->name == "IDENTIFIER" && rhs->name == "IDENTIFIER") {
        Symbol* lhsSym = symbolTable.lookup(lhs->lexeme);
        Symbol* rhsSym = symbolTable.lookup(rhs->lexeme);
        if (lhsSym && rhsSym && lhsSym->isArray && rhsSym->isArray) {
            addError("Cannot assign array '" + rhs->lexeme + "' to array '" + lhs->lexeme + "' (arrays are not assignable)");
            return;
        }
    }

    // Check for array-to-non-pointer assignment (should be error)
    // This handles cases like: int x = arr[1][2]; where arr[1][2] returns int[4]
    if (isArrayType(rhsType) && !isPointerType(lhsType)) {
        addError("Cannot assign array type '" + rhsType + "' to non-pointer type '" + lhsType + "'");
        return;
    }

    // Handle array-to-pointer assignment (array decay)
    // In C, arrays decay to pointers when assigned to pointer variables
    if (isArrayType(rhsType) && isPointerType(lhsType)) {
        // Check if the array can decay to the pointer type
        string arrayElementType = getArrayElementType(rhsType);
        string pointerElementType = getBaseTypeFromPointer(lhsType);
        
        // Count pointer depths
        int lhsPointerDepth = countPointerLevel(lhsType);
        int elementPointerDepth = countPointerLevel(arrayElementType);
        
        // Get the ultimate base types (strip all pointers)
        string arrayBaseType = getBaseTypeFromPointer(arrayElementType);
        string lhsBaseType = getBaseTypeFromPointer(lhsType);
        
        // Array decay adds ONE pointer level to the element type
        // Case 1: int[5] → int* (element is int, lhs is int*, depths: 0+1 = 1) ✅
        // Case 2: int*[5] → int** (element is int*, lhs is int**, depths: 1+1 = 2) ✅  
        // Case 3: int**[3] → int*** (element is int**, lhs is int***, depths: 2+1 = 3) ✅
        // Case 4: int[5] → int** (element is int, lhs is int**, depths: 0+1 ≠ 2) ❌
        // Case 5: int[3][4] → int(*)[4] (element is int[4], special handling needed)
        // Case 6: char*[5] → int** (depths match but base types differ) ❌
        
        // For multidimensional arrays or pointer-to-array types
        if (isArrayType(arrayElementType) || isPointerType(pointerElementType)) {
            // Multidimensional case: int[3][4] → int(*)[4]
            // arrayElementType = int[4], pointerElementType = int[4] (from int(*)[4])
            if (arrayElementType == pointerElementType) {
                return; // Valid
            }
        }
        // For simple arrays (including arrays of pointers)
        else {
            // Element type + 1 pointer level should equal LHS pointer depth
            // AND the ultimate base types must match
            int expectedDepth = elementPointerDepth + 1;
            if (expectedDepth == lhsPointerDepth && arrayBaseType == lhsBaseType) {
                return; // Valid: depths match and base types match
            }
        }
        
        addError("Type mismatch in assignment: cannot assign '" + rhsType + "' to '" + lhsType + "'");
        return;
    }

    // Special-case: allow assigning &array to a pointer (pointer-to-array scenario)
    // Pattern: LHS is identifier with pointerDepth >= 1, RHS is '&' IDENTIFIER where the identifier is an array symbol.
    if (lhs->name == "IDENTIFIER" && rhs && rhs->name == "UNARY_OP" && rhs->children.size() >= 2) {
        Node* opNode = rhs->children[0];
        Node* opnd = rhs->children[1];
        string opLex = opNode->name.empty() ? opNode->lexeme : opNode->name;
        if (opLex == "&" && opnd && opnd->name == "IDENTIFIER") {
            Symbol* lhsSym = symbolTable.lookup(lhs->lexeme);
            Symbol* rhsSym = symbolTable.lookup(opnd->lexeme);
            if (lhsSym && rhsSym && lhsSym->pointerDepth >= 1 && rhsSym->isArray) {
                // Accept the assignment. For richer typing, we'd check array dims vs pointee dims.
                return;
            }
        }
    }

    // Special-case: function pointer assignment checking
    // LHS can be IDENTIFIER or FUNCTION_DECL (for function pointer declarations with initialization)
    // or ARRAY_ACCESS (for array element assignment)
    // or MEMBER_ACCESS (for struct member assignment)
    Node* lhsIdNode = nullptr;
    Symbol* lhsArraySym = nullptr;  // For array element assignment
    Symbol* lhsStructMemberSym = nullptr; // For struct member assignment
    string lhsStructMemberType;
    bool lhsIsFunctionPointerMember = false;
    
    if (lhs->name == "IDENTIFIER") {
        lhsIdNode = lhs;
    } else if (lhs->name == "FUNCTION_DECL") {
        // Extract identifier from FUNCTION_DECL structure
        lhsIdNode = findIdentifierInDeclarator(lhs);
    } else if (lhs->name == "ARRAY_ACCESS" && lhs->children.size() >= 1) {
        // Check if this is an array of function pointers
        Node* arrayBase = lhs->children[0];
        if (arrayBase && arrayBase->name == "IDENTIFIER") {
            lhsArraySym = symbolTable.lookup(arrayBase->lexeme);
        }
    } else if (lhs->name == "MEMBER_ACCESS" || lhs->name == "PTR_MEMBER_ACCESS") {
        // Handle struct member assignment (e.g., calc.operation = multiply or calcs[0].operation = multiply)
        if (lhs->children.size() >= 2) {
            Node* baseNode = lhs->children[0];
            Node* memberNode = lhs->children[1];
            
            Symbol* baseSym = nullptr;
            string baseType;
            
            // Case 1: Direct struct variable (calc.operation)
            if (baseNode && baseNode->name == "IDENTIFIER" && memberNode && memberNode->name == "IDENTIFIER") {
                baseSym = symbolTable.lookup(baseNode->lexeme);
                if (baseSym) {
                    baseType = baseSym->type;
                }
            }
            // Case 2: Array element (calcs[0].operation)
            else if (baseNode && baseNode->name == "ARRAY_ACCESS" && memberNode && memberNode->name == "IDENTIFIER") {
                // Get array base identifier
                if (baseNode->children.size() >= 1 && baseNode->children[0]->name == "IDENTIFIER") {
                    baseSym = symbolTable.lookup(baseNode->children[0]->lexeme);
                    if (baseSym && baseSym->isArray) {
                        // Remove array dimension from type to get element type
                        baseType = baseSym->type;
                        size_t bracketPos = baseType.find('[');
                        if (bracketPos != string::npos) {
                            baseType = baseType.substr(0, bracketPos);
                        }
                    }
                }
            }
            
            if (!baseType.empty()) {
                // Get the member type from the struct definition
                string memberName = memberNode->lexeme;
                lhsStructMemberType = getMemberType(baseType, memberName);
                
                // Check if this member is a function pointer
                if (lhsStructMemberType.find("(*)") != string::npos) {
                    lhsIsFunctionPointerMember = true;
                    lhsStructMemberSym = baseSym; // Store for later use
                }
            }
        }
    }
    
    if (op == "=" && (lhsIdNode || lhsArraySym || lhsIsFunctionPointerMember)) {
        Symbol* lhsSym = lhsIdNode ? symbolTable.lookup(lhsIdNode->lexeme) : lhsArraySym;
        
        // Handle struct member function pointer assignment
        if (lhsIsFunctionPointerMember) {
            Symbol* rhsFuncSym = nullptr;
            Node* r = rhs;
            // Unwrap EXPR_LIST
            if (r && r->name == "EXPR_LIST" && !r->children.empty()) r = r->children[0];
            
            if (r && r->name == "UNARY_OP" && r->children.size() == 2 && r->children[0]->name == "&") {
                Node* opnd = r->children[1];
                if (opnd && opnd->name == "IDENTIFIER") {
                    rhsFuncSym = symbolTable.lookup(opnd->lexeme);
                }
            } else if (r && r->name == "IDENTIFIER") {
                rhsFuncSym = symbolTable.lookup(r->lexeme);
            } else if (r && ((r->name == "INTEGER_CONSTANT" && r->lexeme == "0") || 
                             r->name == "NULLPTR_CONSTANT")) {
                // Allow null/nullptr assignment
                return;
            }

            if (!rhsFuncSym || (!rhsFuncSym->isFunction && !rhsFuncSym->isFunctionPointer)) {
                addError("Cannot assign non-function to struct function pointer member");
                return;
            }
            
            // Extract expected return type from struct member type
            // Format: "int (*)(int, int)"
            string expectedRet;
            size_t pos = lhsStructMemberType.find(" (*)");
            if (pos != string::npos) {
                expectedRet = lhsStructMemberType.substr(0, pos);
            }
            
            // Compare signatures - handle both functions and function pointers
            string rhsRetType = rhsFuncSym->isFunction ? rhsFuncSym->type : rhsFuncSym->funcPtrReturnType;
            
            if (!expectedRet.empty() && expectedRet != rhsRetType) {
                addError("Function pointer return type mismatch: expected '" + expectedRet + "' but got '" + rhsRetType + "'");
                return;
            }
            
            // TODO: Also check parameter types if needed
            return; // Assignment is valid
        }
        
        if (lhsSym && (lhsSym->isFunctionPointer || (lhsSym->isArray && lhsSym->isFunctionPointer))) {
            // Accept assigning function name or &function to function pointer, or 0 (NULL)
            Symbol* rhsFuncSym = nullptr;
            Node* r = rhs;
            // Unwrap EXPR_LIST
            if (r && r->name == "EXPR_LIST" && !r->children.empty()) r = r->children[0];
            
            if (r && r->name == "UNARY_OP" && r->children.size() == 2 && r->children[0]->name == "&") {
                Node* opnd = r->children[1];
                if (opnd && opnd->name == "IDENTIFIER") {
                    rhsFuncSym = symbolTable.lookup(opnd->lexeme);
                }
            } else if (r && r->name == "IDENTIFIER") {
                rhsFuncSym = symbolTable.lookup(r->lexeme);
            } else if (r && ((r->name == "INTEGER_CONSTANT" && r->lexeme == "0") || 
                             r->name == "NULLPTR_CONSTANT")) {
                // Allow null/nullptr assignment
                return;
            }

            if (!rhsFuncSym || (!rhsFuncSym->isFunction && !rhsFuncSym->isFunctionPointer)) {
                string targetDesc = lhsIdNode ? ("function pointer '" + lhsIdNode->lexeme + "'") : "function pointer array element";
                addError("Cannot assign non-function to " + targetDesc);
                return;
            }
            
            // Compare signatures - handle both functions and function pointers
            string expectedRet = lhsSym->funcPtrReturnType;
            string rhsRetType = rhsFuncSym->isFunction ? rhsFuncSym->type : rhsFuncSym->funcPtrReturnType;
            vector<string> rhsParamTypes = rhsFuncSym->isFunction ? rhsFuncSym->paramTypes : rhsFuncSym->funcPtrParamTypes;
            bool rhsVariadic = rhsFuncSym->isFunction ? rhsFuncSym->isVariadic : rhsFuncSym->funcPtrIsVariadic;
            vector<bool> rhsParamIsRef = rhsFuncSym->isFunction ? rhsFuncSym->paramIsReference : rhsFuncSym->funcPtrParamIsReference;
            
            if (expectedRet != rhsRetType) {
                addError("Function pointer return type mismatch: expected '" + expectedRet + "' but got '" + rhsRetType + "'");
                return;
            }
            if (lhsSym->funcPtrIsVariadic != rhsVariadic) {
                // For simplicity, require exact variadic flag match
                addError("Function pointer variadic property mismatch");
                return;
            }
            if (lhsSym->funcPtrParamTypes.size() != rhsParamTypes.size()) {
                addError("Function pointer parameter count mismatch: expected " + to_string(lhsSym->funcPtrParamTypes.size()) +
                         ", got " + to_string(rhsParamTypes.size()));
                return;
            }
            for (size_t i = 0; i < lhsSym->funcPtrParamTypes.size(); ++i) {
                if (lhsSym->funcPtrParamTypes[i] != rhsParamTypes[i]) {
                    addError("Function pointer parameter " + to_string(i+1) + " type mismatch: expected '" +
                             lhsSym->funcPtrParamTypes[i] + "', got '" + rhsParamTypes[i] + "'");
                    return;
                }
                bool expRef = (i < lhsSym->funcPtrParamIsReference.size() && lhsSym->funcPtrParamIsReference[i]);
                bool gotRef = (i < rhsParamIsRef.size() && rhsParamIsRef[i]);
                if (expRef != gotRef) {
                    addError("Function pointer parameter " + to_string(i+1) + " reference qualifier mismatch");
                    return;
                }
            }
            // Signature OK
            return;
        }
    }
    
    // Skip if we couldn't determine types
    if (lhsType.empty() || rhsType.empty()) return;

    // Handle compound assignment operators with proper arithmetic semantics
    if (op == "+=" || op == "-=" || op == "*=" || op == "/=" || op == "%=" ||
        op == "&=" || op == "|=" || op == "^=" || op == "<<=" || op == ">>=") {
       
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
    if ((lhsType == "bool" && isNumericType(rhsType)) ||(rhsType == "bool" && isNumericType(lhsType))) {
        return;
    }
    
      if ((lhsType.find("enum") == 0 && isIntegerType(rhsType)) ||
        (rhsType.find("enum") == 0 && isIntegerType(lhsType))) {
        return ;
    }
    
    // Enum to enum of same type
    if (lhsType.find("enum") == 0 && rhsType.find("enum") == 0) {
        return ;  // Same enum type or compatible enum types
    }
    
    // Pointer to pointer of same base type
    if (isPointerType(lhsType) && isPointerType(rhsType)) {
        checkPointerAssignment(/*node, */lhsType, rhsType);
        return;
    }
    
    // NULL (integer constant 0) can be assigned to pointers; other integers cannot
    if (isPointerType(lhsType) && isIntegerType(rhsType)) {
        // Allow only the literal 0 as a null-pointer constant
        if (rhs && (rhs->name == "INTEGER_CONSTANT" || 
                rhs->name == "NULLPTR_CONSTANT" ||    // ← existing
                rhs->name == "NULL_CONSTANT")) {
          
             if (rhs->name == "INTEGER_CONSTANT" && rhs->lexeme == "0") {
            return; // ok: ptr = 0;
        }
        if (rhs->name == "NULLPTR_CONSTANT" || rhs->name == "NULL_CONSTANT") {  // ← ADD THIS
            return; // ok: ptr = nullptr or ptr = null;
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
    // Handle pointer-to-array types: int(*)[4] or int(*)[3][4]
    // Extract what the pointer points to
    size_t openParen = type.find('(');
    size_t closeParen = type.find(')');
    if (openParen != string::npos && closeParen != string::npos && closeParen > openParen) {
        // This is a pointer-to-array: int(*)[4]
        // Return base type + array dimensions: int[4]
        string baseType = type.substr(0, openParen);
        string arrayPart = type.substr(closeParen + 1);
        return baseType + arrayPart;
    }
    
    // Regular pointer type: int* or int**
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

string SemanticAnalyzer::getMemberType(const string& structTypeName, const string& memberName) {
    // Extract struct name from type (e.g., "struct Calculator" -> "Calculator")
    string structName = structTypeName;
    if (structName.find("struct ") == 0) {
        structName = structName.substr(7);
    } else if (structName.find("union ") == 0) {
        structName = structName.substr(6);
    }
    
    // Look up struct symbol
    Symbol* structSym = symbolTable.lookup(structName);
    if (!structSym || (!structSym->isStruct && !structSym->isUnion)) {
        return "";  // Not a struct/union
    }
    
    // Return member type if it exists
    auto it = structSym->structMembers.find(memberName);
    if (it != structSym->structMembers.end()) {
        return it->second;
    }
    
    return "";  // Member not found
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
       
        
        if (type == "bool") return true;
        if (isNumericType(type)) return true;  // int, char, short, long, bool, etc.
        if (isPointerType(type)) return true;
        if (type == "float" || type == "double" || type == "long double") return true;  // floating types allowed!
        if (type.find("struct ") == 0 || type.find("union ") == 0) {
            return false;  // aggregate types not allowed
        }
        
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
    
    // ✅ CHECK: Void pointer increment/decrement is forbidden
    if (isPointerType(operandType)) {
        string baseType = operandType.substr(0, operandType.length() - 1);
        if (baseType == "void" || baseType.find("void*") == 0) {
            addError("Cannot " + string((operation == "++") ? "increment" : "decrement") + 
                     " 'void*' pointer (void has no size)");
            return;
        }
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
    
    // ✅ CHECK: Void pointer increment/decrement is forbidden
    if (isPointerType(operandType)) {
        string baseType = operandType.substr(0, operandType.length() - 1);
        if (baseType == "void" || baseType.find("void*") == 0) {
            addError("Cannot " + string((operation == "++") ? "increment" : "decrement") + 
                     " 'void*' pointer (void has no size)");
            return;
        }
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

void SemanticAnalyzer::checkSizeof(Node* node) {
    if (!node) return;
    
    // sizeof(type) - SIZEOF_TYPE node
    if (node->name == "SIZEOF_TYPE") {
        // Find SPEC_QUAL_LIST -> TYPE_NAME
        for (Node* child : node->children) {
            if (child->name == "SPEC_QUAL_LIST") {
                for (Node* specChild : child->children) {
                    if (specChild->name == "TYPE_NAME" && !specChild->lexeme.empty()) {
                        string typeName = specChild->lexeme;
                        
                        // Check if it's a typedef that needs to be resolved
                        Symbol* td = symbolTable.lookup(typeName);
                        if (td && td->isTypedef) {
                            // Typedef exists in current scope - valid
                            return;
                        }
                        
                        // Check if it's a primitive type
                        if (typeName == "int" || typeName == "char" || typeName == "float" || 
                            typeName == "double" || typeName == "void" || typeName == "bool" ||
                            typeName == "short" || typeName == "long" || typeName == "signed" || 
                            typeName == "unsigned") {
                            return;
                        }
                        
                        // Check if it's a struct/union/enum
                        if (typeName.find("struct ") == 0 || typeName.find("union ") == 0 || 
                            typeName.find("enum ") == 0) {
                            return;
                        }
                        
                        // Type not found - error
                        addError("Undeclared type '" + typeName + "' in sizeof");
                        return;
                    }
                }
            }
        }
    }
    // sizeof expression - no type checking needed for expressions
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
if (node->name == "NULLPTR_CONSTANT") return false;  // ← existing
if (node->name == "NULL_CONSTANT") return false;     // ← ADD THIS LINE
    
    
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
    
    // Function pointer typedef support
    bool isFunctionPointer = false;
    string funcPtrReturnType;
    vector<string> funcPtrParamTypes;
    vector<bool> funcPtrParamIsRef;
    bool funcPtrIsVariadic = false;
    
    // ✅ ADD: Anonymous struct/union/enum detection
    bool anonStruct = false;
    map<string, string> anonStructMembers;
    map<string, Node*> anonStructMemberNodes;
    vector<string> anonStructMemberOrder;

    bool anonEnum = false;
    map<string, int> anonEnumValues;
    map<string, Node*> anonEnumNodes;
    
    // Extract base type from DECL_SPECIFIERS
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
            baseType = extractTypeFromDeclSpecifiers(child);
            cout << "DEBUG processTypedef: Extracted base type = '" << baseType << "'\n";
            
            // ✅ PROCESS STRUCT/UNION/ENUM DEFINITIONS
            for (auto specChild : child->children) {
                if (specChild->name == "STRUCT_OR_UNION_SPECIFIER") {
                    bool hasDecl = false;
                    bool hasIdent = false;
                    for (auto bodyChild : specChild->children) {
                        if (bodyChild->name == "DECLARATION") {
                            hasDecl = true;
                        }
                        if (bodyChild->name == "IDENTIFIER") {
                            hasIdent = true;
                        }
                    }
                    
                    if (hasDecl && hasIdent) {
                        // Named struct/union with body - process it
                        processStruct(specChild);
                    } else if (hasDecl && !hasIdent) {
                        // ✅ ANONYMOUS STRUCT/UNION - extract members
                        anonStruct = true;
                        for (auto bodyChild : specChild->children) {
                            if (bodyChild->name == "DECLARATION") {
                                extractStructMembers(bodyChild, anonStructMembers, 
                                                   anonStructMemberNodes, anonStructMemberOrder);
                            }
                        }
                    }
                } 
                else if (specChild->name == "ENUM_SPECIFIER") {
                    if (isEnumDefinition(specChild)) {
                        bool hasIdent = false;
                        for (auto c2 : specChild->children) {
                            if (c2->name == "IDENTIFIER") { 
                                hasIdent = true; 
                                break; 
                            }
                        }
                        
                        if (hasIdent) {
                            // Named enum - process it
                            processEnum(specChild);
                        } else {
                            // ✅ ANONYMOUS ENUM - extract values
                            anonEnum = true;
                            int currentValue = 0;
                            for (auto enumChild : specChild->children) {
                                if (enumChild->name == "ENUMERATOR_LIST") {
                                    for (auto enumeratorNode : enumChild->children) {
                                        if (enumeratorNode->name == "ENUMERATOR") {
                                            if (!enumeratorNode->children.empty() && 
                                                enumeratorNode->children[0]->name == "IDENTIFIER") {
                                                string enumIdent = enumeratorNode->children[0]->lexeme;
                                                int value = currentValue;
                                                
                                                // Check for explicit value
                                                for (auto ec : enumeratorNode->children) {
                                                    if (ec->name == "CONSTANT_EXPR") {
                                                        value = evaluateConstantExpression(ec);
                                                        break;
                                                    }
                                                }
                                                anonEnumValues[enumIdent] = value;
                                                anonEnumNodes[enumIdent] = enumeratorNode->children[0];
                                                currentValue = value + 1;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto declChild : child->children) {
                if (!declChild->children.empty()) {
                    Node* firstChild = declChild->children[0];
                    
                    // Check if this is a function pointer typedef
                    Node* funcDecl = findFunctionDeclarator(firstChild);
                    if (funcDecl && isFunctionPointerDeclarator(firstChild)) {
                        isFunctionPointer = true;
                        cout << "DEBUG processTypedef: Detected function pointer typedef\n";
                        
                        // Extract function pointer name
                        std::function<void(Node*)> findIdentifier = [&](Node* n) {
                            if (!n || !aliasName.empty()) return;
                            if (n->name == "IDENTIFIER") {
                                aliasName = n->lexeme;
                                nodee = n;
                            }
                            for (auto c : n->children) findIdentifier(c);
                        };
                        findIdentifier(funcDecl);
                        
                        // Extract return type (baseType from DECL_SPECIFIERS)
                        funcPtrReturnType = baseType;
                        
                        // Extract parameter types
                        funcPtrParamTypes = extractFunctionParameters(funcDecl, &funcPtrParamIsRef);
                        
                        // Check for variadic
                        std::function<bool(Node*)> hasEllipsis = [&](Node* n) -> bool {
                            if (!n) return false;
                            if (n->name == "ELLIPSIS") return true;
                            for (auto c : n->children) if (hasEllipsis(c)) return true;
                            return false;
                        };
                        funcPtrIsVariadic = hasEllipsis(funcDecl);
                        
                        cout << "DEBUG processTypedef: Function pointer '" << aliasName 
                             << "' returns '" << funcPtrReturnType << "' with " 
                             << funcPtrParamTypes.size() << " parameters\n";
                    }
                    else if (firstChild->name == "IDENTIFIER") {
                        nodee=firstChild;
                        aliasName = firstChild->lexeme;
                    }
                    else if (firstChild->name == "DECLARATOR") {
                        analyzeDeclarator(firstChild, aliasName, pointerDepth, isArray, arrayDims);
                        nodee = findIdentifierInDeclarator(firstChild);
                    }
                    else if (firstChild->name == "ARRAY") {
                        isArray = true;
                        arrayDims = extractArrayDimensions(firstChild, aliasName);
                        nodee = findIdentifierInArray(firstChild);
                    }
                }
            }
        }
    }
    
    if (aliasName.empty() || (baseType.empty() && !isFunctionPointer)) {
        addError("Invalid typedef declaration");
        return;
    }
    
    // Handle function pointer typedef
    if (isFunctionPointer) {
        // Create a typedef symbol for the function pointer
        if (!symbolTable.addSymbol(aliasName, funcPtrReturnType, nodee, false, {}, 
                                   false, {}, 0, false, false, false,
                                   true, funcPtrReturnType)) {
            addError("Redefinition of typedef '" + aliasName + "'");
            return;
        }
        
        Symbol* sym = symbolTable.lookupCurrentScope(aliasName);
        if (sym) {
            // Mark as function pointer typedef and store signature
            sym->isFunctionPointer = true;
            sym->funcPtrReturnType = funcPtrReturnType;
            sym->funcPtrParamTypes = funcPtrParamTypes;
            sym->funcPtrParamIsReference = funcPtrParamIsRef;
            sym->funcPtrIsVariadic = funcPtrIsVariadic;
            
            if (nodee) {
                nodee->symbol = sym;
                cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
            }
            
            cout << "SUCCESS: Function pointer typedef '" << aliasName 
                 << "' = " << funcPtrReturnType << " (*)(";
            for (size_t i = 0; i < funcPtrParamTypes.size(); i++) {
                if (i > 0) cout << ", ";
                cout << funcPtrParamTypes[i];
            }
            if (funcPtrIsVariadic) cout << ", ...";
            cout << ")\n";
        }
        return;
    }
    
    // ✅ BUILD ALIASED TYPE - handle anonymous types
    string fullAliasedType = baseType;
    if (anonStruct) {
        if (baseType.find("union") == 0) 
            fullAliasedType = string("union ") + aliasName;
        else 
            fullAliasedType = string("struct ") + aliasName;
    }
    if (anonEnum) {
        fullAliasedType = string("enum ") + aliasName;
    }
    
    cout << "DEBUG: Processing typedef: " << aliasName << " = " << fullAliasedType;
    if (isArray) {
        for (int dim : arrayDims) {
            cout << "[" << (dim == -1 ? "" : to_string(dim)) << "]";
        }
    }
    cout << "\n";
    
    // ✅ ADD SYMBOL WITH PROPER METADATA
    if (!symbolTable.addSymbol(aliasName, fullAliasedType, nodee, false, {}, 
                               isArray, arrayDims, pointerDepth, false, false, false,
                               true, fullAliasedType)) {
        // Handle tag namespace overlap (C allows typedef struct S S;)
        Symbol* existing = symbolTable.lookupCurrentScope(aliasName);
        if (existing && (existing->isStruct || existing->isUnion || existing->isEnum) && !existing->isTypedef) {
            existing->isTypedef = true;
            existing->aliasedType = fullAliasedType;
            
            // ✅ ATTACH ANONYMOUS STRUCT/UNION MEMBERS
            if (anonStruct) {
                existing->isStruct = true;
                existing->structMembers = anonStructMembers;
                existing->structMemberOrder = anonStructMemberOrder;
                existing->aliasedType = string((baseType.find("union") == 0) ? "union " : "struct ") + aliasName;
            }
            
            // ✅ ATTACH ANONYMOUS ENUM VALUES
            if (anonEnum) {
                existing->isEnum = true;
                existing->enumValues = anonEnumValues;
                existing->aliasedType = string("enum ") + aliasName;
                
                // Add enum constants to scope
                for (const auto& p : anonEnumValues) {
                    Node* enumNode = anonEnumNodes[p.first];
                    string enumConstType = string("enum ") + aliasName;
                    if (!symbolTable.addSymbol(p.first, enumConstType, enumNode)) {
                        addError("Redefinition of enumerator '" + p.first + "'");
                    } else {
                        Symbol* es = symbolTable.lookupCurrentScope(p.first);
                        if (es && enumNode) enumNode->symbol = es;
                    }
                }
            }
            
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
        if (sym) {
            // ✅ ATTACH ANONYMOUS STRUCT/UNION METADATA
            if (anonStruct) {
                sym->isStruct = true;
                sym->structMembers = anonStructMembers;
                sym->structMemberOrder = anonStructMemberOrder;
                sym->aliasedType = string((baseType.find("union") == 0) ? "union " : "struct ") + aliasName;
            }
            
            // ✅ ATTACH ANONYMOUS ENUM METADATA
            if (anonEnum) {
                sym->isEnum = true;
                sym->enumValues = anonEnumValues;
                
                // Add enum constants to scope
                for (const auto& p : anonEnumValues) {
                    Node* enumNode = anonEnumNodes[p.first];
                    string enumConstType = string("enum ") + aliasName;
                    if (!symbolTable.addSymbol(p.first, enumConstType, enumNode)) {
                        addError("Redefinition of enumerator '" + p.first + "'");
                    } else {
                        Symbol* es = symbolTable.lookupCurrentScope(p.first);
                        if (es && enumNode) enumNode->symbol = es;
                    }
                }
                sym->aliasedType = string("enum ") + aliasName;
            }
            
            if (nodee) {
                nodee->symbol = sym;
                cout << "DEBUG: Attached symbol " << sym->name << " to AST node" << endl;
            }
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
            // For function pointer typedefs, return a special representation
            if (sym->isFunctionPointer) {
                string result = sym->funcPtrReturnType + " (*)(";
                for (size_t i = 0; i < sym->funcPtrParamTypes.size(); i++) {
                    if (i > 0) result += ", ";
                    result += sym->funcPtrParamTypes[i];
                }
                if (sym->funcPtrIsVariadic) result += ", ...";
                result += ")";
                cout << "  Resolving function pointer typedef '" << type << "' -> '" << result << "'\n";
                return result;
            }
            
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
    
    // Per C standard 6.8.4.1/6.8.5: controlling expression shall have SCALAR type
    // Scalar types include: arithmetic types (int, float, double) AND pointer types
    // Zero = false, non-zero = true (for ALL scalar types)
    
    // Check if the type can be used in a boolean context
    // Valid: bool, integers (char, int, long, etc.), floats, doubles, pointers
    bool isValid = (condType == "bool" || isIntegerType(condType) || isPointerType(condType) ||
                    condType == "float" || condType == "double" || condType == "long double");
    
    // Structs/unions by value cannot be used in boolean context; pointers to them are allowed
    if ((condType.rfind("struct ", 0) == 0 || condType.rfind("union ", 0) == 0) && !isPointerType(condType)) {
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
                    // Distinguish function pointer declarator vs prototype
                    if (isFunctionPointerDeclarator(initDecl)) {
                        // This is a variable (pointer to function), not a prototype
                        continue;
                    }
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
    Node* funcDeclContainer = nullptr; // root under which FUNCTION_DECL resides
    bool declIsVariadic = false;
    
    // Extract return type
    for (auto child : declNode->children) {
        if (child->name == "DECL_SPECIFIERS") {
            returnType = extractTypeFromDeclSpecifiers(child);
        }
        else if (child->name == "INIT_DECL_LIST") {
            for (auto initDecl : child->children) {
                funcDeclNode = findFunctionDeclarator(initDecl);
                if (funcDeclNode) {
                    funcDeclContainer = initDecl;
                    // Get function name
                    if (!funcDeclNode->children.empty() && 
                        funcDeclNode->children[0]->name == "IDENTIFIER") {
                        nodee=funcDeclNode->children[0];
                        funcName = funcDeclNode->children[0]->lexeme;
                    }
                    {
                        std::vector<bool> _tmpRefFlags;
                        paramTypes = extractFunctionParameters(funcDeclNode, &_tmpRefFlags);
                    }
                    // detect '...'
                    std::function<bool(Node*)> hasEllipsis = [&](Node* n) -> bool {
                        if (!n) return false;
                        if (n->name == "ELLIPSIS") return true;
                        for (auto c : n->children) if (hasEllipsis(c)) return true;
                        return false;
                    };
                    declIsVariadic = hasEllipsis(funcDeclNode);
                    break;
                }
            }
        }
    }
    
    if (returnType.empty()) {
        returnType = "int";  // Default
    }
    
    if (!funcName.empty()) {
        // Build full return type by accounting for pointer stars in the declarator
        string fullReturnType = returnType;
        if (funcDeclContainer && funcDeclNode) {
            // Count pointer depth on the path from container to FUNCTION_DECL
            std::function<int(Node*)> countPointerStars = [&](Node* n) -> int {
                if (!n) return 0;
                int total = (n->name == "POINTER") ? 1 : 0;
                for (auto c : n->children) total += countPointerStars(c);
                return total;
            };
            std::function<int(Node*, int)> findPointerDepth = [&](Node* current, int depthAccum) -> int {
                if (!current) return -1;
                if (current == funcDeclNode) return depthAccum;
                int working = depthAccum;
                for (auto c : current->children) {
                    if (c == funcDeclNode) return working;
                    if (c->name == "POINTER") {
                        int foundInPtr = findPointerDepth(c, working + 1);
                        if (foundInPtr != -1) return foundInPtr;
                        working += countPointerStars(c);
                    } else {
                        int found = findPointerDepth(c, working);
                        if (found != -1) return found;
                    }
                }
                return -1;
            };
            int ptrDepth = findPointerDepth(funcDeclContainer, 0);
            for (int i = 0; i < std::max(0, ptrDepth); ++i) fullReturnType += "*";
        }

        // Add function declaration to symbol table (use fullReturnType)
        bool added = symbolTable.addSymbol(funcName, fullReturnType, nodee, true, paramTypes);
        if (!added) {
            Symbol* existing = symbolTable.lookupCurrentScope(funcName);
            if (existing && existing->isFunction) {
                // Check if signatures match
                if (existing->paramTypes != paramTypes || existing->type != fullReturnType || existing->isVariadic != declIsVariadic) {
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

            // Mark variadic based on presence of ELLIPSIS in declarator
            if (sym && funcDeclNode) {
                std::function<bool(Node*)> hasEllipsis = [&](Node* n) -> bool {
                    if (!n) return false;
                    if (n->name == "ELLIPSIS") return true;
                    for (auto c : n->children) if (hasEllipsis(c)) return true;
                    return false;
                };
                sym->isVariadic = hasEllipsis(funcDeclNode);
                // Attach parameter reference flags
                std::vector<bool> paramIsRef2;
                extractFunctionParameters(funcDeclNode, &paramIsRef2);
                sym->paramIsReference = paramIsRef2;
            }

          cout << "DEBUG: Added function prototype: " << funcName 
              << " [" << (funcDeclContainer?string("") : string("")) << fullReturnType << "] with " << paramTypes.size() << " parameter(s)\n";
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

Node* SemanticAnalyzer::findFunctionDeclaratorInNode(Node* node) {
    return findFunctionDeclarator(node);
}

bool SemanticAnalyzer::isFunctionPointerDeclarator(Node* initDeclNode) {
    if (!initDeclNode) return false;
    Node* funcDecl = findFunctionDeclarator(initDeclNode);
    if (!funcDecl) return false;
    if (funcDecl->children.empty()) return false;
    // If the left child subtree of FUNCTION_DECL contains a POINTER, this is a pointer-to-function
    std::function<bool(Node*)> containsPointer = [&](Node* n) -> bool {
        if (!n) return false;
        if (n->name == "POINTER") return true;
        for (auto c : n->children) if (containsPointer(c)) return true;
        return false;
    };
    return containsPointer(funcDecl->children[0]);
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
            
            // Parse flags/width/precision
            i++;
            while (i < format.length() && (isdigit(format[i]) || format[i] == '.' || format[i] == '-' || format[i] == '+' || format[i] == ' ')) {
                i++;
            }

            // Parse length modifier (hh, h, l, ll, z)
            string lengthMod;
            if (i < format.length()) {
                if (format[i] == 'h') {
                    if (i + 1 < format.length() && format[i + 1] == 'h') { lengthMod = "hh"; i += 2; }
                    else { lengthMod = "h"; i += 1; }
                } else if (format[i] == 'l') {
                    if (i + 1 < format.length() && format[i + 1] == 'l') { lengthMod = "ll"; i += 2; }
                    else { lengthMod = "l"; i += 1; }
                } else if (format[i] == 'z') {
                    lengthMod = "z"; i += 1;
                }
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
            
            auto matchOneOf = [&](const vector<string>& accepted) -> bool {
                for (const auto& t : accepted) if (argType == t) return true;
                return false;
            };

            switch (specifier) {
                case 'd': case 'i': { // signed integers
                    // Determine expected based on length modifier
                    if (isScanf) {
                        if (lengthMod == "hh") { expectedType = "signed char"; typeMatch = (argType == "signed char" || argType == "char"); }
                        else if (lengthMod == "h") { expectedType = "short"; typeMatch = (argType == "short"); }
                        else if (lengthMod == "l") { expectedType = "long"; typeMatch = (argType == "long"); }
                        else if (lengthMod == "ll") { expectedType = "long long"; typeMatch = (argType == "long long"); }
                        else { expectedType = "int"; typeMatch = (argType == "int"); }
                    } else {
                        if (lengthMod == "l") { expectedType = "long"; typeMatch = (argType == "long"); }
                        else if (lengthMod == "ll") { expectedType = "long long"; typeMatch = (argType == "long long"); }
                        else { expectedType = "int"; typeMatch = matchOneOf({"int","short","char"}); /* default promotions */ }
                    }
                    break;
                }
                case 'u': case 'o': case 'x': case 'X': { // unsigned integers
                    if (isScanf) {
                        if (lengthMod == "hh") { expectedType = "unsigned char"; typeMatch = (argType == "unsigned char"); }
                        else if (lengthMod == "h") { expectedType = "unsigned short"; typeMatch = (argType == "unsigned short"); }
                        else if (lengthMod == "l") { expectedType = "unsigned long"; typeMatch = (argType == "unsigned long"); }
                        else if (lengthMod == "ll") { expectedType = "unsigned long long"; typeMatch = (argType == "unsigned long long"); }
                        else if (lengthMod == "z") { expectedType = "size_t"; typeMatch = (argType == "unsigned long" || argType == "unsigned int"); }
                        else { expectedType = "unsigned int"; typeMatch = (argType == "unsigned int"); }
                    } else {
                        if (lengthMod == "l") { expectedType = "unsigned long"; typeMatch = (argType == "unsigned long"); }
                        else if (lengthMod == "ll") { expectedType = "unsigned long long"; typeMatch = (argType == "unsigned long long"); }
                        else if (lengthMod == "z") { expectedType = "size_t"; typeMatch = (argType == "unsigned long" || argType == "unsigned int"); }
                        else { expectedType = "unsigned int"; typeMatch = matchOneOf({"unsigned int","int","unsigned short","unsigned char","short","char"}); }
                    }
                    break;
                }
                case 'f': case 'e': case 'g': case 'F': case 'E': case 'G': { // floating point
                    if (isScanf) {
                        if (lengthMod == "l") { expectedType = "double"; typeMatch = (argType == "double"); }
                        else { expectedType = "float"; typeMatch = (argType == "float"); }
                    } else {
                        // In printf, float is promoted to double; both %f and %lf expect double
                        expectedType = "double";
                        typeMatch = (argType == "double" || argType == "float");
                    }
                    break;
                }
                case 'c': { // character
                    if (isScanf) { expectedType = "char"; typeMatch = (argType == "char"); }
                    else { expectedType = "char"; typeMatch = (argType == "char" || argType == "int"); }
                    break;
                }
                case 's': { // string
                    if (isScanf) { expectedType = "char"; typeMatch = (argType == "char"); }
                    else {
                        expectedType = "char*";
                        typeMatch = (argType == "char*" || (isPointerType(argTypes[argIndex]) && argType == "char"));
                    }
                    break;
                }
                case 'p': { // pointer (printf only commonly)
                    expectedType = "pointer";
                    typeMatch = isPointerType(argTypes[argIndex]);
                    break;
                }
                default:
                    // Unknown specifier - skip validation for this one
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


// Add static linkage validation
void SemanticAnalyzer::checkStaticRedeclaration(const string& name, bool isStatic) {
    Symbol* existing = symbolTable.lookupCurrentScope(name);
    
    if (existing) {
        // ✅ Static vs non-static mismatch
        if (existing->isStatic != isStatic) {
            if (isStatic) {
                addError("Static declaration of '" + name + 
                         "' follows non-static declaration");
            } else {
                addError("Non-static declaration of '" + name + 
                         "' follows static declaration");
            }
        }
    }
}
