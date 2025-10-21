#include "ir_generator.h"
#include <fstream>
#include <sstream>

using namespace std;

// TACInstruction methods
void TACInstruction::print(ostream& out) const {
    out << toString() << endl;
}

string TACInstruction::toString() const {
    stringstream ss;
    
    switch(opcode) {
        case TACOp::LABEL:
            ss << result << ":";
            break;
            
        case TACOp::GOTO:
            ss << "goto " << result;
            break;
            
        case TACOp::IF_GOTO:
            ss << "if " << operand1 << " goto " << result;
            break;
            
        case TACOp::IF_FALSE_GOTO:
            ss << "if_false " << operand1 << " goto " << result;
            break;
            
        case TACOp::ASSIGN:
            ss << result << " = " << operand1;
            break;
            
        case TACOp::CONST:
            ss << result << " = CONST " << operand1;
            break;
            
        case TACOp::CALL:
            ss << result << " = call " << operand1;
            break;
            
        case TACOp::RETURN:
            if (!operand1.empty()) {
                ss << "return " << operand1;
            } else {
                ss << "return";
            }
            break;
            
        case TACOp::PARAM:
            ss << "param " << operand1;
            break;
            
        // Binary operations
        case TACOp::ADD: case TACOp::SUB: case TACOp::MUL: case TACOp::DIV:
        case TACOp::MOD: case TACOp::EQ: case TACOp::NE: case TACOp::LT:
        case TACOp::LE: case TACOp::GT: case TACOp::GE: case TACOp::AND: 
        case TACOp::OR: case TACOp::BIT_AND: case TACOp::BIT_OR: 
        case TACOp::BIT_XOR: case TACOp::SHL: case TACOp::SHR: 
            {
                string opStr;
                switch(opcode) {
                    case TACOp::ADD: opStr = "+"; break;
                    case TACOp::SUB: opStr = "-"; break;
                    case TACOp::MUL: opStr = "*"; break;
                    case TACOp::DIV: opStr = "/"; break;
                    case TACOp::MOD: opStr = "%"; break;
                    case TACOp::EQ: opStr = "=="; break;
                    case TACOp::NE: opStr = "!="; break;
                    case TACOp::LT: opStr = "<"; break;
                    case TACOp::LE: opStr = "<="; break;
                    case TACOp::GT: opStr = ">"; break;
                    case TACOp::GE: opStr = ">="; break;
                    case TACOp::AND: opStr = "&&"; break;
                    case TACOp::OR: opStr = "||"; break;
                    case TACOp::BIT_AND: opStr = "&"; break;
                    case TACOp::BIT_OR: opStr = "|"; break;
                    case TACOp::BIT_XOR: opStr = "^"; break;
                    case TACOp::SHL: opStr = "<<"; break;
                    case TACOp::SHR: opStr = ">>"; break;
                     // case TACOp::NOT: opStr = "!"; break;
                        //case TACOp::NEG: opStr = "-"; break;
                    default: opStr = "?";
                }
                ss << result << " = " << operand1 << " " << opStr << " " << operand2;
            }
            break;
            
        // Unary operations
       
case TACOp::NEG: case TACOp::NOT: case TACOp::BIT_NOT:
    {
        string opStr;
        switch(opcode) {
            case TACOp::NEG: opStr = "-"; break;
            case TACOp::NOT: opStr = "!"; break;
            case TACOp::BIT_NOT: opStr = "~"; break;
            default: opStr = "?";
        }
        ss << result << " = " << opStr << operand1;
    }
    break;
            
        // Memory operations
        case TACOp::LOAD:
            ss << result << " = *" << operand1;
            break;
            
        case TACOp::STORE:
            ss << "*" << result << " = " << operand1;
            break;
            
        case TACOp::ADDRESS:
            ss << result << " = &" << operand1;
            break;
            
        // Array operations
        case TACOp::ARRAY_LOAD:
            ss << result << " = " << operand1 << "[" << operand2 << "]";
            break;
            
        case TACOp::ARRAY_STORE:
            ss << operand1 << "[" << operand2 << "] = " << result;
            break;
        case TACOp::MEMBER_ACCESS:
    ss << result << " = " << operand1 << "." << operand2;
    break;

case TACOp::MEMBER_STORE:
    ss << operand1 << "." << operand2 << " = " << result;
    break;

case TACOp::PTR_MEMBER_ACCESS:
    ss << result << " = " << operand1 << "->" << operand2;
    break;

case TACOp::PTR_MEMBER_STORE:
    ss << operand1 << "->" << operand2 << " = " << result;
    break;    
        default:
            ss << "UNKNOWN_OP";
    }
    
    return ss.str();
}

// IRGenerator implementation

IRGenerator::IRGenerator(SymbolTable& symTab) 
    : symbolTable(symTab), tempCounter(0), labelCounter(0), 
      currentFunction(""), currentBreakLabel(""), currentContinueLabel("") {}
bool IRGenerator::generateIR(Node* ast) {
    if (!ast) return false;
    
    cout << "=== Generating TAC IR ===" << endl;
    
    // Reset state
    instructions.clear();
    tempCounter = 0;
    labelCounter = 0;
    currentFunction = "";
     enumConstants.clear();  // Add this
    
    // First pass: collect enum constants
    collectEnumConstants(ast);
   
    traverseAST(ast);
    cout << "Generated " << instructions.size() << " TAC instructions" << endl;
    return true;
}

void IRGenerator::traverseAST(Node* node) {
    if (!node) return;
    
    // Process all children at this level
    for (auto child : node->children) {
        if (!child) continue;
        
        // Handle function definitions
        if (child->name == "FUNCTION_DEFINITION") {
            // Check if it's a wrapper node
            bool hasNestedFunction = false;
            for (auto grandchild : child->children) {
                if (grandchild->name == "FUNCTION_DEFINITION") {
                    hasNestedFunction = true;
                    break;
                }
            }
            
            if (hasNestedFunction) {
                // Wrapper node - traverse its children
                traverseAST(child);
            } else {
                // Actual function - generate IR
                generateFunction(child);
            }
            continue;
        }
        
        // Handle global declarations
        if (child->name == "DECLARATION") {
            // Check if it's a type declaration (typedef, struct def, enum def)
            bool isTypeDecl = false;
            
            for (auto declChild : child->children) {
                if (declChild->name == "TYPEDEF") {
                    isTypeDecl = true;
                    break;
                }
                
                if (declChild->name == "DECL_SPECIFIERS") {
                    for (auto specChild : declChild->children) {
                        if (specChild->name == "TYPEDEF" || 
                            specChild->name == "STORAGE_CLASS_SPECIFIER" ||
                            (specChild->name == "STRUCT_OR_UNION_SPECIFIER" && hasStructBody(specChild)) ||
                            specChild->name == "ENUM_SPECIFIER") {
                            isTypeDecl = true;
                            break;
                        }
                    }
                }
            }
            
            // Only generate IR for actual variable declarations with initialization
            if (!isTypeDecl && !currentFunction.empty()) {
                // Local declaration inside function
                generateDeclaration(child);
            } else if (!isTypeDecl && currentFunction.empty()) {
                // Global declaration - generate initialization code
                generateDeclaration(child);
            }
            // Skip type declarations - no IR needed
            continue;
        }
        
        // Handle struct/union definitions (optional - emit metadata)
        if (child->name == "STRUCT_OR_UNION_SPECIFIER") {
            string structName = findIdentifier(child);
            if (!structName.empty()) {
                // Emit a label for struct (optional metadata)
                cout << "DEBUG: Found struct " << structName << " (skipping IR)" << endl;
            }
            continue;
        }
        
        // Handle enum definitions (optional - emit constants)
        if (child->name == "ENUM_SPECIFIER") {
            cout << "DEBUG: Found enum definition (skipping IR)" << endl;
            // You could emit enum constants here if needed
            continue;
        }
        
        // Handle typedef (skip - no runtime code)
        if (child->name == "TYPEDEF") {
            continue;
        }
        
        // For other nodes, traverse deeper
        traverseAST(child);
    }
}

void IRGenerator::generateStatement(Node* node) {
    if (!node) return;
    
    cout << "DEBUG: Generating statement for " << node->name << " with " << node->children.size() << " children" << endl;
    
    if (node->name == "EXPR_LIST") {
        for (auto child : node->children) {
            generateStatement(child);
        }
    }
    else if (node->name == "EXPRESSION_STATEMENT") {
        // Expression statements wrap an expression
        if (!node->children.empty()) {
            generateExpression(node->children[0]);
        }
    }
    else if (node->name == "ASSIGN_EXPR" || node->name == "FUNC_CALL") {
        generateExpression(node);
    }
    else if (node->name == "COMPOUND_STMT") {
        cout << "DEBUG: Processing compound statement with " << node->children.size() << " children" << endl;
        for (auto child : node->children) {
            generateStatement(child);
        }
    }
    else if (node->name == "BLOCK_ITEM_LIST") {
        cout << "DEBUG: Processing block item list with " << node->children.size() << " children" << endl;
        for (auto child : node->children) {
            generateStatement(child);
        }
    }
    else if (node->name == "IF_STMT" || node->name == "IF_ELSE_STMT") {
        generateIfStatement(node);
    }
    else if (node->name == "WHILE_STMT") {
        generateWhileStatement(node);
    }
    else if (node->name == "FOR_STMT_2" || node->name == "FOR_STMT_3") {
    generateForStatement(node);
}
else if (node->name == "DO_WHILE_STMT") {
    generateDoWhileStatement(node);
}
else if (node->name == "UNTIL_STMT") {
    generateUntilStatement(node);
}
    else if (node->name == "RETURN_STMT") {
        generateReturnStatement(node);
    }
    else if (node->name == "DECLARATION") {
        generateDeclaration(node);
    }
    else if (node->name == "FUNCTION_DEFINITION") {
        generateFunction(node);
    }
else if (node->name == "SWITCH_STMT") {
    generateSwitchStatement(node);
}
else if (node->name == "BREAK_STMT") {
    generateBreakStatement(node);
}
else if (node->name == "CONTINUE_STMT") {
    generateContinueStatement(node);
}
else if (node->name == "GOTO_STMT") {
    generateGotoStatement(node);
}
else if (node->name == "LABELED_STMT" || node->name == "LABEL") {
    generateLabeledStatement(node);
}
    else {
        // For other statement types, process children
        cout << "DEBUG: Falling back to processing children for " << node->name << endl;
        for (auto child : node->children) {
            generateStatement(child);
        }
    }
}
// Helper to check if struct/union has a body (is a definition vs declaration)
bool IRGenerator::hasStructBody(Node* node) {
    if (!node) return false;
    
    for (auto child : node->children) {
        if (child->name == "STRUCT_DECL_LIST" || child->name == "DECLARATION") {
            return true;  // Has body - it's a definition
        }
    }
    return false;  // Just a declaration/usage
}

// Helper to find identifier in a subtree
string IRGenerator::findIdentifier(Node* node) {
    if (!node) return "";
    
    if (node->name == "IDENTIFIER") {
        return node->lexeme;
    }
    
    for (auto child : node->children) {
        string id = findIdentifier(child);
        if (!id.empty()) return id;
    }
    
    return "";
}
string IRGenerator::createTemp() {
    return "t" + to_string(tempCounter++);
}

string IRGenerator::createLabel(const string& prefix) {
    return prefix + to_string(labelCounter++);
}

void IRGenerator::printIR(ostream& out) const {
    out << "\n=== Three Address Code (TAC) ===" << endl;
    for (const auto& instr : instructions) {
        instr.print(out);
    }
}

void IRGenerator::writeToFile(const string& filename) const {
    ofstream file(filename);
    if (file.is_open()) {
        for (const auto& instr : instructions) {
            file << instr.toString() << endl;
        }
        file.close();
        cout << "TAC written to " << filename << endl;
    }
}


string IRGenerator::generateExpression(Node* node) {
    if (!node) return "";
    
    cout << "DEBUG: Generating expression for " << node->name << endl;
  
   if (node->name == "IDENTIFIER") {
    // Check if it's an enum constant
    if (enumConstants.find(node->lexeme) != enumConstants.end()) {
        string temp = createTemp();
        instructions.emplace_back(TACOp::CONST, temp,to_string(enumConstants[node->lexeme]));
        return temp;
    }
    return node->lexeme;
}
    else if (node->name == "INTEGER_CONSTANT" || node->name == "FLOAT_CONSTANT" || 
             node->name == "CHAR_LITERAL" || node->name == "BOOL_LITERAL") {
        string temp = createTemp();
        instructions.emplace_back(TACOp::CONST, temp, node->lexeme);
        return temp;
    }
    else if (node->name == "STRING_LITERAL") {
    string temp = createTemp();
    // Store the string literal (including quotes for now)
    instructions.emplace_back(TACOp::CONST, temp, node->lexeme);
    return temp;
}
    else if (node->name == "ADD_EXPR" || node->name == "SUB_EXPR" || 
             node->name == "MUL_EXPR" || node->name == "DIV_EXPR" ||
             node->name == "MOD_EXPR") {
        return generateBinaryExpr(node, getBinaryOp(node->name));
    }
    else if (node->name == "EQ_EXPR" || node->name == "NEQ_EXPR" ||
             node->name == "LT_EXPR" || node->name == "LE_EXPR" ||
             node->name == "GT_EXPR" || node->name == "GE_EXPR") {
        return generateBinaryExpr(node, getBinaryOp(node->name));
    }
    else if (node->name == "BIT_AND" || node->name == "BIT_OR" ||
             node->name == "BIT_XOR" || node->name == "LSHIFT_EXPR" ||
             node->name == "RSHIFT_EXPR") {
        return generateBinaryExpr(node, getBinaryOp(node->name));
    }
    // Add these to generateExpression method
else if (node->name == "LOGICAL_AND") {
    return generateLogicalAnd(node);
}
else if (node->name == "LOGICAL_OR") {
    return generateLogicalOr(node);
}
else if (node->name == "COMPOUND_ASSIGN") {
    return generateCompoundAssignment(node);
}
else if (node->name == "ASSIGN_EXPR") {
    if (node->children.size() >= 3) {
        Node* lhs = node->children[0];
        Node* rhs = node->children[2];
        
        // Check if this is a pointer dereference store: *ptr = value
        if (lhs->name == "UNARY_OP" && lhs->children.size() >= 2) {
            Node* opNode = lhs->children[0];
            string op = opNode->name;
            if (op.empty()) op = opNode->lexeme;
            
            if (op == "*") {
                // *ptr = value
                Node* ptrNode = lhs->children[1];
                string ptrTemp = generateExpression(ptrNode);
                string valueTemp = generateExpression(rhs);
                instructions.emplace_back(TACOp::STORE, ptrTemp, valueTemp);
                return valueTemp;
            }
        }
        
        // Check if this is an array store: arr[i] = value
        if (lhs->name == "ARRAY_ACCESS") {
            return generateArrayStore(lhs, rhs);
        }
        
        // Check if this is a struct member store: struct.member = value
        if (lhs->name == "MEMBER_ACCESS" || lhs->name == "DIRECT_MEMBER" || lhs->name == "DOT") {
            return generateMemberStore(lhs, rhs, false);
        }
        
        // Check if this is a pointer member store: ptr->member = value
        if (lhs->name == "PTR_MEMBER_ACCESS" || lhs->name == "ARROW") {
            return generateMemberStore(lhs, rhs, true);
        }
        
        // Check if it's a compound assignment
        if (node->children[1]->name == "OP") {
            string op = node->children[1]->lexeme;
            if (op != "=") {
                return generateCompoundAssignment(node);
            }
        }
    }
    return generateAssignment(node);
}
 
    else if (node->name == "FUNC_CALL") {
        return generateFunctionCall(node);
    }
    else if (node->name == "ARRAY_ACCESS") {
        return generateArrayAccess(node);
    }
       else if (node->name == "PRE_INC") {
    return generatePreIncrement(node);
}
else if (node->name == "PRE_DEC") {
    return generatePreDecrement(node);
}
else if (node->name == "POST_INC") {
    return generatePostIncrement(node);
}
else if (node->name == "POST_DEC") {
    return generatePostDecrement(node);
}
else if (node->name == "TERNARY_EXPR" || node->name == "COND_EXPR") {
    return generateTernaryOperator(node);
}
else if (node->name == "COND_EXPR") {
    return generateTernaryOperator(node);
}
else if (node->name == "MEMBER_ACCESS" || node->name == "DIRECT_MEMBER" || node->name == "DOT") {
    return generateMemberAccess(node, false);
}
else if (node->name == "PTR_MEMBER_ACCESS" || node->name == "ARROW") {
    return generateMemberAccess(node, true);
}
else if (node->name == "SIZEOF" || node->name == "SIZEOF_TYPE") {
    return generateSizeof(node);
}
   
  else if (node->name == "UNARY_OP") {
    if (node->children.size() >= 2) {
        Node* opNode = node->children[0];
        Node* operandNode = node->children[1];
        
        // Get operator - check both name and lexeme
        string op = opNode->name;
        if (op == "OP" || op.empty()) {
            op = opNode->lexeme;
        }
        
        string operandTemp = generateExpression(operandNode);
        string resultTemp = createTemp();
        
        if (op == "&") {
            instructions.emplace_back(TACOp::ADDRESS, resultTemp, operandTemp);
        } else if (op == "*") {
            instructions.emplace_back(TACOp::LOAD, resultTemp, operandTemp);
        } else if (op == "-") {
            instructions.emplace_back(TACOp::NEG, resultTemp, operandTemp);
        } else if (op == "!") {
            instructions.emplace_back(TACOp::NOT, resultTemp, operandTemp);
        } else if (op == "~") {
            instructions.emplace_back(TACOp::BIT_NOT, resultTemp, operandTemp);
        }
        
        return resultTemp;
    }
}
    
    // For expressions we don't handle yet, try processing children
    if (!node->children.empty()) {
        // For comparison expressions that might be wrapped in EXPR_LIST
        if (node->children[0]->name == "GT_EXPR" || node->children[0]->name == "LT_EXPR" ||
            node->children[0]->name == "EQ_EXPR" || node->children[0]->name == "NEQ_EXPR") {
            return generateExpression(node->children[0]);
        }
        return generateExpression(node->children[0]);
    }
    
    return "";
}
// Add these new methods:
string IRGenerator::generateLogicalAnd(Node* node) {
    if (!node || node->children.size() < 2) return "";
    
    string leftTemp = generateExpression(node->children[0]);
    string resultTemp = createTemp();
    string falseLabel = createLabel("logical_false");
    string endLabel = createLabel("logical_end");
    
    // Check left operand
    instructions.emplace_back(TACOp::IF_FALSE_GOTO, falseLabel, leftTemp);
    
    // Left is true, evaluate right
    string rightTemp = generateExpression(node->children[1]);
    instructions.emplace_back(TACOp::ASSIGN, resultTemp, rightTemp);
    instructions.emplace_back(TACOp::GOTO, endLabel);
    
    // False case
    instructions.emplace_back(TACOp::LABEL, falseLabel);
    instructions.emplace_back(TACOp::ASSIGN, resultTemp, "0");
    
    // End
    instructions.emplace_back(TACOp::LABEL, endLabel);
    
    return resultTemp;
}
string IRGenerator::generateLogicalOr(Node* node) {
    if (!node || node->children.size() < 2) return "";
    
    string leftTemp = generateExpression(node->children[0]);
    string resultTemp = createTemp();
    string trueLabel = createLabel("logical_true");
    string endLabel = createLabel("logical_end");
    
    // If left is true (non-zero), jump to true label
    instructions.emplace_back(TACOp::IF_GOTO, trueLabel, leftTemp);
    
    // Evaluate right side (only if left is false)
    string rightTemp = generateExpression(node->children[1]);
    
    // Both are false - result is right side (or 0)
    instructions.emplace_back(TACOp::ASSIGN, resultTemp, rightTemp);
    instructions.emplace_back(TACOp::GOTO, endLabel);
    
    // True label - result is 1
    instructions.emplace_back(TACOp::LABEL, trueLabel);
    instructions.emplace_back(TACOp::ASSIGN, resultTemp, "1");
    
    // End label
    instructions.emplace_back(TACOp::LABEL, endLabel);
    
    return resultTemp;
}
// Add these to ir_generator.cpp after the existing methods


TACOp IRGenerator::getBinaryOp(const string& nodeName) {
    if (nodeName == "ADD_EXPR") return TACOp::ADD;
    if (nodeName == "SUB_EXPR") return TACOp::SUB;
    if (nodeName == "MUL_EXPR") return TACOp::MUL;
    if (nodeName == "DIV_EXPR") return TACOp::DIV;
    if (nodeName == "MOD_EXPR") return TACOp::MOD;
    if (nodeName == "EQ_EXPR") return TACOp::EQ;
    if (nodeName == "NEQ_EXPR") return TACOp::NE;
    if (nodeName == "LT_EXPR") return TACOp::LT;
    if (nodeName == "LE_EXPR") return TACOp::LE;
    if (nodeName == "GT_EXPR") return TACOp::GT;
    if (nodeName == "GE_EXPR") return TACOp::GE;
    if (nodeName == "LOGICAL_AND") return TACOp::AND;
    if (nodeName == "LOGICAL_OR") return TACOp::OR;
    if (nodeName == "BIT_AND") return TACOp::BIT_AND;
    if (nodeName == "BIT_OR") return TACOp::BIT_OR;
    if (nodeName == "BIT_XOR") return TACOp::BIT_XOR;
    if (nodeName == "LSHIFT_EXPR") return TACOp::SHL;
    if (nodeName == "RSHIFT_EXPR") return TACOp::SHR;
    return TACOp::ADD; // default
}

TACOp IRGenerator::getUnaryOp(const string& nodeName) {
    if (nodeName == "NEG") return TACOp::NEG;
    if (nodeName == "NOT") return TACOp::NOT;
    return TACOp::NEG; // default
}

// Binary expression generation
string IRGenerator::generateBinaryExpr(Node* node, TACOp op) {
    if (!node || node->children.size() < 2) return "";
    
    string leftTemp = generateExpression(node->children[0]);
    string rightTemp = generateExpression(node->children[1]);
    string resultTemp = createTemp();
    
    instructions.emplace_back(op, resultTemp, leftTemp, rightTemp);
    return resultTemp;
}

// Unary expression generation
string IRGenerator::generateUnaryExpr(Node* node, TACOp op) {
    if (!node || node->children.empty()) return "";
    
    string operandTemp = generateExpression(node->children[0]);
    string resultTemp = createTemp();
    
    instructions.emplace_back(op, resultTemp, operandTemp);
    return resultTemp;
}


string IRGenerator::generateAssignment(Node* node) {
    if (!node || node->children.size() < 3) return "";
    
    Node* lhs = node->children[0];
    Node* rhs = node->children[2];
    
    if (lhs->name == "IDENTIFIER") {
        string rhsTemp = generateExpression(rhs);
        
        // Optimize: if RHS is a constant, assign directly without temp
        if (rhs->name == "INTEGER_CONSTANT" || rhs->name == "FLOAT_CONSTANT" || 
            rhs->name == "CHAR_LITERAL" || rhs->name == "BOOL_LITERAL") {
            instructions.emplace_back(TACOp::ASSIGN, lhs->lexeme, rhs->lexeme);
            return lhs->lexeme;
        } else {
            instructions.emplace_back(TACOp::ASSIGN, lhs->lexeme, rhsTemp);
            return lhs->lexeme;
        }
    }
    
    return "";
}

// Function call generation
string IRGenerator::generateFunctionCall(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* funcNode = node->children[0];
    string funcName = (funcNode->name == "IDENTIFIER") ? funcNode->lexeme : "";
    
    if (funcName.empty()) return "";
    
    // Handle arguments if present
    if (node->children.size() > 1) {
        Node* argList = node->children[1];
        for (auto arg : argList->children) {
            string argTemp = generateExpression(arg);
            instructions.emplace_back(TACOp::PARAM, "", argTemp);
        }
    }
    
    string resultTemp = createTemp();
    instructions.emplace_back(TACOp::CALL, resultTemp, funcName);
    return resultTemp;
}



// Add these to ir_generator.cpp
void IRGenerator::generateDeclaration(Node* node) {
    if (!node) return;
    
    cout << "DEBUG: Generating declaration" << endl;
    
    // Check for storage class specifiers (static, extern, etc.)
    bool isStatic = false;
    for (auto child : node->children) {
        if (child->name == "DECL_SPECIFIERS") {
            for (auto spec : child->children) {
                if (spec->name == "STATIC" || 
                    (spec->name == "STORAGE_CLASS_SPECIFIER" && 
                     spec->lexeme == "static")) {
                    isStatic = true;
                    break;
                }
            }
        }
    }
    
    // Look for INIT_DECL_LIST in children
    for (auto child : node->children) {
        if (child->name == "INIT_DECL_LIST") {
            for (auto decl : child->children) {
                if (decl->name == "ASSIGN_EXPR" && decl->children.size() >= 3) {
                    Node* varNode = decl->children[0];
                    Node* initNode = decl->children[2];
                    
                    string varName = "";
                    
                    if (varNode->name == "IDENTIFIER") {
                        varName = varNode->lexeme;
                    }
                    else if (varNode->name == "DECLARATOR") {
                        varName = getDeclaratorName(varNode);
                    }
                    else if (varNode->name == "ARRAY") {
                        if (varNode->children.size() >= 2 && 
                            varNode->children[0]->name == "IDENTIFIER") {
                            string arrayName = varNode->children[0]->lexeme;
                            string arraySize = varNode->children[1]->lexeme;
                            cout << "DEBUG: Array declaration: " << arrayName 
                                 << "[" << arraySize << "]" << endl;
                        }
                        continue;
                    }
                    
                    // Track static variables
                    if (isStatic && !varName.empty()) {
                        staticVariables.insert(varName);
                        cout << "DEBUG: Static variable: " << varName << endl;
                    }
                    
                    // Generate initialization if present
                    if (!varName.empty() && initNode->name != "EMPTY") {
                        string initTemp = generateExpression(initNode);
                        if (!initTemp.empty()) {
                            instructions.emplace_back(TACOp::ASSIGN, varName, initTemp);
                            cout << "Generated assignment: " << varName << " = " << initTemp << endl;
                        }
                    } else if (!varName.empty()) {
                        cout << "DEBUG: Uninitialized variable " << varName << endl;
                    }
                }
            }
        }
    }
}

void IRGenerator::generateFunction(Node* node) {
    if (!node) return;
    
    cout << "DEBUG: Generating function: " << node->name << endl;
    
    // Handle nested FUNCTION_DEFINITION structure
    Node* actualFunctionDef = node;
    if (node->children.size() > 0 && node->children[0]->name == "FUNCTION_DEFINITION") {
        actualFunctionDef = node->children[0];
    }
    
    // Extract function name from declarator
    string funcName;
    Node* declarator = nullptr;
    Node* body = nullptr;
     Node* paramList = nullptr;
    // Find declarator and body in the children
    for (auto child : actualFunctionDef->children) {
        if (child->name == "FUNCTION_DECL" || child->name == "IDENTIFIER") {
            declarator = child;
             // Look for parameter list in function declarator
            for (auto declChild : child->children) {
                if (declChild->name == "PARAM_TYPE_LIST" || declChild->name == "PARAM_LIST") {
                    paramList = declChild;
                }
            }
        } else if (child->name == "COMPOUND_STMT") {
            body = child;
        } else if (child->name == "DECL_SPECIFIERS") {
            // This contains the return type, we can ignore for now
            continue;
        }
    }
    
    // Extract function name
    if (declarator) {
        funcName = getDeclaratorName(declarator);
    }
    
    if (funcName.empty()) {
        funcName = "unknown_function";
    }
    
    currentFunction = funcName;
    string label = "func_" + funcName;
    instructions.emplace_back(TACOp::LABEL, label);
    
    cout << "Generating function: " << funcName << endl;
    
    if (paramList) {
        generateParameterHandling(paramList);
    }
    
    // Generate function body
    if (body) {
        generateStatement(body);
    }
    
    // Add implicit return if needed
    if (currentFunction != "main" && 
        (instructions.empty() || instructions.back().opcode != TACOp::RETURN)) {
        instructions.emplace_back(TACOp::RETURN);
    }
    
    currentFunction = "";
}
void IRGenerator::generateParameterHandling(Node* paramList) {
    if (!paramList) return;
    
    cout << "DEBUG: Generating parameter handling" << endl;
    
    // Process all parameters in the list
    for (auto param : paramList->children) {
        if (param->name == "PARAM_DECL") {
            // Extract parameter name
            string paramName = findParameterName(param);
            if (!paramName.empty()) {
                cout << "DEBUG: Found parameter: " << paramName << endl;
                // In a real implementation, we'd generate code to handle parameters
                // For now, just note that we found them
            }
        }
    }
}

string IRGenerator::findParameterName(Node* paramDecl) {
    if (!paramDecl) return "";
    
    // Look for declarator in parameter declaration
    for (auto child : paramDecl->children) {
        if (child->name == "DECLARATOR" || child->name == "IDENTIFIER") {
            return getDeclaratorName(child);
        }
    }
    
    return "";
}
void IRGenerator::generateIfStatement(Node* node) {
    if (!node) return;
    
    cout << "DEBUG: Generating if statement" << endl;
    
    if (node->name == "IF_STMT") {
        // IF without ELSE: condition, then-statement
        if (node->children.size() >= 2) {
            string conditionTemp = generateExpression(node->children[0]);
            string endLabel = createLabel("endif");
            
            // If condition is false, jump to end
            instructions.emplace_back(TACOp::IF_FALSE_GOTO, endLabel, conditionTemp);
            
            // Generate then branch
            generateStatement(node->children[1]);
            
            // End label
            instructions.emplace_back(TACOp::LABEL, endLabel);
        }
    }
    else if (node->name == "IF_ELSE_STMT") {
        // IF with ELSE: condition, then-statement, else-statement
        if (node->children.size() >= 3) {
            string conditionTemp = generateExpression(node->children[0]);
            string elseLabel = createLabel("else");
            string endLabel = createLabel("endif");
            
            // If condition is false, jump to else branch
            instructions.emplace_back(TACOp::IF_FALSE_GOTO, elseLabel, conditionTemp);
            
            // Generate then branch
            generateStatement(node->children[1]);
            
            // Jump to end after then branch (skip else)
            instructions.emplace_back(TACOp::GOTO, endLabel);
            
            // Else label
            instructions.emplace_back(TACOp::LABEL, elseLabel);
            
            // Generate else branch
            generateStatement(node->children[2]);
            
            // End label
            instructions.emplace_back(TACOp::LABEL, endLabel);
        }
    }
}


void IRGenerator::generateWhileStatement(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    cout << "DEBUG: Generating while statement" << endl;
    
    string startLabel = createLabel("while_start");
    string conditionLabel = createLabel("while_cond");
    string endLabel = createLabel("while_end");
    
    // Save previous labels
    string prevBreakLabel = currentBreakLabel;
    string prevContinueLabel = currentContinueLabel;
    
    // Set current labels for break/continue
    currentBreakLabel = endLabel;
    currentContinueLabel = conditionLabel;
    
    // Start of loop - jump to condition check
    instructions.emplace_back(TACOp::GOTO, conditionLabel);
    
    // Loop body label
    instructions.emplace_back(TACOp::LABEL, startLabel);
    
    // Generate loop body
    generateStatement(node->children[1]);
    
    // Condition check label
    instructions.emplace_back(TACOp::LABEL, conditionLabel);
    
    // Generate condition and check
    string conditionTemp = generateExpression(node->children[0]);
    instructions.emplace_back(TACOp::IF_GOTO, startLabel, conditionTemp);
    
    // End of loop
    instructions.emplace_back(TACOp::LABEL, endLabel);
    
    // Restore previous labels
    currentBreakLabel = prevBreakLabel;
    currentContinueLabel = prevContinueLabel;
}


void IRGenerator::generateReturnStatement(Node* node) {
    cout << "DEBUG: Generating return statement" << endl;
    
    if (!node->children.empty()) {
        string returnValue = generateExpression(node->children[0]);
        instructions.emplace_back(TACOp::RETURN, "", returnValue);
    } else {
        instructions.emplace_back(TACOp::RETURN);
    }
}

// Helper function to extract declarator name (similar to your parser)
string IRGenerator::getDeclaratorName(Node* node) {
    if (!node) return "";
    
    if (node->name == "IDENTIFIER") {
        return node->lexeme;
    }
    
    for (auto child : node->children) {
        string name = getDeclaratorName(child);
        if (!name.empty()) return name;
    }
    
    return "";
}

void IRGenerator::generateForStatement(Node* node) {
    if (!node || node->children.size() < 3) return;
    
    cout << "DEBUG: Generating for statement" << endl;
    
    string startLabel = createLabel("for_start");
    string condLabel = createLabel("for_cond");
    string updateLabel = createLabel("for_update");
    string endLabel = createLabel("for_end");
    
    // Save previous labels
    string prevBreakLabel = currentBreakLabel;
    string prevContinueLabel = currentContinueLabel;
    
    // Set current labels
    currentBreakLabel = endLabel;
    currentContinueLabel = updateLabel;
    
    // Generate initialization (child 0)
    if (node->children[0]->name != "EMPTY") {
        generateStatement(node->children[0]);
    }
    
    // Jump to condition
    instructions.emplace_back(TACOp::GOTO, condLabel);
    
    // Loop body label
    instructions.emplace_back(TACOp::LABEL, startLabel);
    
    // Generate loop body
    int bodyIndex = (node->children.size() == 4) ? 3 : 2;
    if (bodyIndex < node->children.size()) {
        generateStatement(node->children[bodyIndex]);
    }
    
    // Update label
    instructions.emplace_back(TACOp::LABEL, updateLabel);
    
    // Generate update expression
    if (node->children.size() == 4 && node->children[2]->name != "EMPTY") {
        generateExpression(node->children[2]);
    }
    
    // Condition label
    instructions.emplace_back(TACOp::LABEL, condLabel);
    
    // Generate condition
    if (node->children[1]->name != "EMPTY") {
        string condTemp = generateExpression(node->children[1]);
        instructions.emplace_back(TACOp::IF_GOTO, startLabel, condTemp);
    } else {
        instructions.emplace_back(TACOp::GOTO, startLabel);
    }
    
    // End label
    instructions.emplace_back(TACOp::LABEL, endLabel);
    
    // Restore previous labels
    currentBreakLabel = prevBreakLabel;
    currentContinueLabel = prevContinueLabel;
}

void IRGenerator::generateDoWhileStatement(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    cout << "DEBUG: Generating do-while statement" << endl;
    
    string startLabel = createLabel("do_start");
    string endLabel = createLabel("do_end");
    
    // Loop body label
    instructions.emplace_back(TACOp::LABEL, startLabel);
    
    // Generate loop body (child 0)
    generateStatement(node->children[0]);
    
    // Generate condition (child 1)
    string condTemp = generateExpression(node->children[1]);
    
    // If condition is true, jump back to start
    instructions.emplace_back(TACOp::IF_GOTO, startLabel, condTemp);
    
    // End label
    instructions.emplace_back(TACOp::LABEL, endLabel);
}


string IRGenerator::generatePreIncrement(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* operand = node->children[0];
    if (operand->name != "IDENTIFIER") return "";
    
    string varName = operand->lexeme;
    string oneTemp = createTemp();
    string resultTemp = createTemp();
    
    // Create constant 1
    instructions.emplace_back(TACOp::CONST, oneTemp, "1");
    
    // var = var + 1
    instructions.emplace_back(TACOp::ADD, resultTemp, varName, oneTemp);
    instructions.emplace_back(TACOp::ASSIGN, varName, resultTemp);
    
    // Return the new value
    return varName;
}

string IRGenerator::generatePreDecrement(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* operand = node->children[0];
    if (operand->name != "IDENTIFIER") return "";
    
    string varName = operand->lexeme;
    string oneTemp = createTemp();
    string resultTemp = createTemp();
    
    // Create constant 1
    instructions.emplace_back(TACOp::CONST, oneTemp, "1");
    
    // var = var - 1
    instructions.emplace_back(TACOp::SUB, resultTemp, varName, oneTemp);
    instructions.emplace_back(TACOp::ASSIGN, varName, resultTemp);
    
    // Return the new value
    return varName;
}

string IRGenerator::generatePostIncrement(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* operand = node->children[0];
    if (operand->name != "IDENTIFIER") return "";
    
    string varName = operand->lexeme;
    string oldValue = createTemp();
    string oneTemp = createTemp();
    string newValue = createTemp();
    
    // Save old value
    instructions.emplace_back(TACOp::ASSIGN, oldValue, varName);
    
    // Create constant 1
    instructions.emplace_back(TACOp::CONST, oneTemp, "1");
    
    // var = var + 1
    instructions.emplace_back(TACOp::ADD, newValue, varName, oneTemp);
    instructions.emplace_back(TACOp::ASSIGN, varName, newValue);
    
    // Return the old value (before increment)
    return oldValue;
}

string IRGenerator::generatePostDecrement(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* operand = node->children[0];
    if (operand->name != "IDENTIFIER") return "";
    
    string varName = operand->lexeme;
    string oldValue = createTemp();
    string oneTemp = createTemp();
    string newValue = createTemp();
    
    // Save old value
    instructions.emplace_back(TACOp::ASSIGN, oldValue, varName);
    
    // Create constant 1
    instructions.emplace_back(TACOp::CONST, oneTemp, "1");
    
    // var = var - 1
    instructions.emplace_back(TACOp::SUB, newValue, varName, oneTemp);
    instructions.emplace_back(TACOp::ASSIGN, varName, newValue);
    
    // Return the old value (before decrement)
    return oldValue;
}

string IRGenerator::generateCompoundAssignment(Node* node) {
    if (!node || node->children.size() < 3) return "";
    
    Node* lhs = node->children[0];
    Node* op = node->children[1];
    Node* rhs = node->children[2];
    
    if (lhs->name != "IDENTIFIER") return "";
    
    string varName = lhs->lexeme;
    string rhsTemp = generateExpression(rhs);
    string resultTemp = createTemp();
    
    // Determine the operation based on operator
    string opStr = op->lexeme;
    
    if (opStr == "+=") {
        instructions.emplace_back(TACOp::ADD, resultTemp, varName, rhsTemp);
    }
    else if (opStr == "-=") {
        instructions.emplace_back(TACOp::SUB, resultTemp, varName, rhsTemp);
    }
    else if (opStr == "*=") {
        instructions.emplace_back(TACOp::MUL, resultTemp, varName, rhsTemp);
    }
    else if (opStr == "/=") {
        instructions.emplace_back(TACOp::DIV, resultTemp, varName, rhsTemp);
    }
    else if (opStr == "%=") {
        instructions.emplace_back(TACOp::MOD, resultTemp, varName, rhsTemp);
    }
    else if (opStr == "&=") {
        instructions.emplace_back(TACOp::BIT_AND, resultTemp, varName, rhsTemp);
    }
    else if (opStr == "|=") {
        instructions.emplace_back(TACOp::BIT_OR, resultTemp, varName, rhsTemp);
    }
    else if (opStr == "^=") {
        instructions.emplace_back(TACOp::BIT_XOR, resultTemp, varName, rhsTemp);
    }
    else if (opStr == "<<=") {
        instructions.emplace_back(TACOp::SHL, resultTemp, varName, rhsTemp);
    }
    else if (opStr == ">>=") {
        instructions.emplace_back(TACOp::SHR, resultTemp, varName, rhsTemp);
    }
    else {
        cout << "WARNING: Unknown compound operator: " << opStr << endl;
        return "";
    }
    
    // Store result back to variable
    instructions.emplace_back(TACOp::ASSIGN, varName, resultTemp);
    
    return varName;
}
void IRGenerator::generateSwitchStatement(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    cout << "DEBUG: Generating switch statement" << endl;
    
    Node* switchExpr = node->children[0];
    Node* caseList = node->children[1];
    
    // Generate switch expression (unwrap EXPR_LIST if needed)
    string switchTemp;
    if (switchExpr->name == "EXPR_LIST" && !switchExpr->children.empty()) {
        switchTemp = generateExpression(switchExpr->children[0]);
    } else {
        switchTemp = generateExpression(switchExpr);
    }
    
    string endLabel = createLabel("switch_end");
    
    // Store the end label for break statements
    string prevBreakLabel = currentBreakLabel;
    currentBreakLabel = endLabel;
    
    // Collect all case information
    vector<CaseInfo> cases;
    
    // Process CASE_LIST
    if (caseList->name == "CASE_LIST") {
        for (auto caseNode : caseList->children) {
            if (caseNode->name == "CASE_ELEMENT") {
                // CASE_ELEMENT has: INTEGER_CONSTANT, STATEMENT_LIST
                if (!caseNode->children.empty()) {
                    CaseInfo info;
                    info.value = caseNode->children[0]->lexeme;  // Get case value
                    info.label = createLabel("case");
                    info.isDefault = false;
                    info.node = caseNode;
                    cases.push_back(info);
                }
            }
            else if (caseNode->name == "DEFAULT_ELEMENT") {
                // DEFAULT_ELEMENT has: STATEMENT_LIST
                CaseInfo info;
                info.value = "";
                info.label = createLabel("default");
                info.isDefault = true;
                info.node = caseNode;
                cases.push_back(info);
            }
        }
    }
    
    // Generate comparison code for each case
    for (const auto& caseInfo : cases) {
        if (caseInfo.isDefault) continue;  // Skip default in comparisons
        
        string condTemp = createTemp();
        string caseValueTemp = createTemp();
        
        // Load case constant
        instructions.emplace_back(TACOp::CONST, caseValueTemp, caseInfo.value);
        
        // Compare: condTemp = (switchTemp == caseValue)
        instructions.emplace_back(TACOp::EQ, condTemp, switchTemp, caseValueTemp);
        instructions.emplace_back(TACOp::IF_GOTO, caseInfo.label, condTemp);
    }
    
    // If no case matched, jump to default or end
    bool hasDefault = false;
    string defaultLabel = "";
    for (const auto& caseInfo : cases) {
        if (caseInfo.isDefault) {
            hasDefault = true;
            defaultLabel = caseInfo.label;
            break;
        }
    }
    
    if (hasDefault) {
        instructions.emplace_back(TACOp::GOTO, defaultLabel);
    } else {
        instructions.emplace_back(TACOp::GOTO, endLabel);
    }
    
    // Generate code for each case body
    for (const auto& caseInfo : cases) {
        // Emit case label
        instructions.emplace_back(TACOp::LABEL, caseInfo.label);
        
        // Generate statements for this case
        // CASE_ELEMENT/DEFAULT_ELEMENT has STATEMENT_LIST as child
        if (caseInfo.node->children.size() > 1) {
            Node* stmtList = caseInfo.node->children[1];  // STATEMENT_LIST
            generateStatementList(stmtList);
        } else if (caseInfo.node->children.size() == 1 && caseInfo.isDefault) {
            // DEFAULT_ELEMENT only has STATEMENT_LIST
            Node* stmtList = caseInfo.node->children[0];
            generateStatementList(stmtList);
        }
    }
    
    // End label
    instructions.emplace_back(TACOp::LABEL, endLabel);
    
    // Restore previous break label
    currentBreakLabel = prevBreakLabel;
}

void IRGenerator::generateStatementList(Node* node) {
    if (!node) return;
    
    if (node->name == "STATEMENT_LIST") {
        for (auto child : node->children) {
            if (child->name == "EXPR_LIST") {
                // Process expressions in EXPR_LIST
                for (auto expr : child->children) {
                    generateExpression(expr);
                }
            } else {
                generateStatement(child);
            }
        }
    } else {
        generateStatement(node);
    }
}



void IRGenerator::generateBreakStatement(Node* node) {
    cout << "DEBUG: Generating break statement" << endl;
    
    if (!currentBreakLabel.empty()) {
        instructions.emplace_back(TACOp::GOTO, currentBreakLabel);
    } else {
        cout << "ERROR: break statement outside of loop/switch" << endl;
    }
}

void IRGenerator::generateContinueStatement(Node* node) {
    cout << "DEBUG: Generating continue statement" << endl;
    
    if (!currentContinueLabel.empty()) {
        instructions.emplace_back(TACOp::GOTO, currentContinueLabel);
    } else {
        cout << "ERROR: continue statement outside of loop" << endl;
    }
}

string IRGenerator::generateTernaryOperator(Node* node) {
    if (!node || node->children.size() < 3) return "";
    
    cout << "DEBUG: Generating ternary operator" << endl;
    
    Node* condition = node->children[0];
    Node* trueExpr = node->children[1];
    Node* falseExpr = node->children[2];
    
    string condTemp = generateExpression(condition);
    string resultTemp = createTemp();
    string trueLabel = createLabel("ternary_true");
    string falseLabel = createLabel("ternary_false");
    string endLabel = createLabel("ternary_end");
    
    // If condition is false, jump to false branch
    instructions.emplace_back(TACOp::IF_FALSE_GOTO, falseLabel, condTemp);
    
    // True branch
    instructions.emplace_back(TACOp::LABEL, trueLabel);
    string trueTemp = generateExpression(trueExpr);
    instructions.emplace_back(TACOp::ASSIGN, resultTemp, trueTemp);
    instructions.emplace_back(TACOp::GOTO, endLabel);
    
    // False branch
    instructions.emplace_back(TACOp::LABEL, falseLabel);
    string falseTemp = generateExpression(falseExpr);
    instructions.emplace_back(TACOp::ASSIGN, resultTemp, falseTemp);
    
    // End
    instructions.emplace_back(TACOp::LABEL, endLabel);
    
    return resultTemp;
}

string IRGenerator::generateArrayStore(Node* arrayNode, Node* valueNode) {
    if (!arrayNode || arrayNode->children.size() < 2) return "";
    
    cout << "DEBUG: Generating array store" << endl;
    
    Node* arrayBase = arrayNode->children[0];
    Node* indexExpr = arrayNode->children[1];
    
    string arrayTemp = generateExpression(arrayBase);
    string indexTemp = generateExpression(indexExpr);
    string valueTemp = generateExpression(valueNode);
    
    // Format: arr[index] = value
    instructions.emplace_back(TACOp::ARRAY_STORE, valueTemp, arrayTemp, indexTemp);
    
    return valueTemp;
}

string IRGenerator::generateArrayAccess(Node* node) {
    if (!node || node->children.size() < 2) return "";
    
    Node* arrayBase = node->children[0];
    Node* indexExpr = node->children[1];
    
    // Check if base is itself an array access (multi-dimensional)
    string baseTemp;
    if (arrayBase->name == "ARRAY_ACCESS") {
        // Multi-dimensional: arr[i][j]
        baseTemp = generateArrayAccess(arrayBase);
    } else {
        baseTemp = generateExpression(arrayBase);
    }
    
    string indexTemp = generateExpression(indexExpr);
    string resultTemp = createTemp();
    
    instructions.emplace_back(TACOp::ARRAY_LOAD, resultTemp, baseTemp, indexTemp);
    return resultTemp;
}

string IRGenerator::generateMemberAccess(Node* node, bool isPointer) {
    if (!node || node->children.size() < 2) return "";
    
    cout << "DEBUG: Generating " << (isPointer ? "pointer" : "direct") 
         << " member access" << endl;
    
    Node* structNode = node->children[0];
    Node* memberNode = node->children[1];
    
    string structTemp = generateExpression(structNode);
    string memberName = "";
    
    if (memberNode->name == "IDENTIFIER") {
        memberName = memberNode->lexeme;
    }
    
    if (memberName.empty()) return "";
    
    string resultTemp = createTemp();
    
    if (isPointer) {
        instructions.emplace_back(TACOp::PTR_MEMBER_ACCESS, resultTemp, structTemp, memberName);
    } else {
        instructions.emplace_back(TACOp::MEMBER_ACCESS, resultTemp, structTemp, memberName);
    }
    
    return resultTemp;
}

string IRGenerator::generateMemberStore(Node* memberNode, Node* valueNode, bool isPointer) {
    if (!memberNode || memberNode->children.size() < 2) return "";
    
    cout << "DEBUG: Generating " << (isPointer ? "pointer" : "direct") 
         << " member store" << endl;
    
    Node* structNode = memberNode->children[0];
    Node* memberIdNode = memberNode->children[1];
    
    string structTemp = generateExpression(structNode);
    string valueTemp = generateExpression(valueNode);
    string memberName = "";
    
    if (memberIdNode->name == "IDENTIFIER") {
        memberName = memberIdNode->lexeme;
    }
    
    if (memberName.empty()) return "";
    
    if (isPointer) {
        instructions.emplace_back(TACOp::PTR_MEMBER_STORE, valueTemp, structTemp, memberName);
    } else {
        instructions.emplace_back(TACOp::MEMBER_STORE, valueTemp, structTemp, memberName);
    }
    
    return valueTemp;
}

void IRGenerator::collectEnumConstants(Node* node) {
    if (!node) return;
    
    // Look for ENUM_SPECIFIER
    if (node->name == "ENUM_SPECIFIER") {
        int currentValue = 0;
        
        for (auto child : node->children) {
            if (child->name == "ENUMERATOR_LIST") {
                for (auto enumerator : child->children) {
                    if (enumerator->name == "ENUMERATOR") {
                        string enumName = "";
                        int enumValue = currentValue;
                        
                        // Get enumerator name and optional value
                        for (auto enumChild : enumerator->children) {
                            if (enumChild->name == "IDENTIFIER") {
                                enumName = enumChild->lexeme;
                            }
                            else if (enumChild->name == "INTEGER_CONSTANT") {
                                enumValue = stoi(enumChild->lexeme);
                                currentValue = enumValue;
                            }
                        }
                        
                        if (!enumName.empty()) {
                            enumConstants[enumName] = enumValue;
                            cout << "DEBUG: Enum constant " << enumName 
                                 << " = " << enumValue << endl;
                        }
                        
                        currentValue++;
                    }
                }
            }
        }
    }
    
    // Recursively search for enums in children
    for (auto child : node->children) {
        collectEnumConstants(child);
    }
}

void IRGenerator::generateGotoStatement(Node* node) {
    if (!node || node->children.empty()) return;
    
    cout << "DEBUG: Generating goto statement" << endl;
    
    Node* labelNode = node->children[0];
    string labelName = "";
    
    if (labelNode->name == "IDENTIFIER") {
        labelName = labelNode->lexeme;
    }
    
    if (!labelName.empty()) {
        instructions.emplace_back(TACOp::GOTO, "label_" + labelName);
    }
}

void IRGenerator::generateLabeledStatement(Node* node) {
    if (!node || node->children.empty()) return;
    
    cout << "DEBUG: Generating labeled statement" << endl;
    
    Node* labelNode = node->children[0];
    string labelName = "";
    
    if (labelNode->name == "IDENTIFIER") {
        labelName = labelNode->lexeme;
    }
    
    if (!labelName.empty()) {
        instructions.emplace_back(TACOp::LABEL, "label_" + labelName);
    }
    
    // Generate the statement after the label
    if (node->children.size() > 1) {
        generateStatement(node->children[1]);
    }
}

void IRGenerator::generateUntilStatement(Node* node) {
    if (!node || node->children.size() < 2) return;
    
    cout << "DEBUG: Generating until statement" << endl;
    
    string startLabel = createLabel("until_start");
    string conditionLabel = createLabel("until_cond");
    string endLabel = createLabel("until_end");
    
    // Save previous labels
    string prevBreakLabel = currentBreakLabel;
    string prevContinueLabel = currentContinueLabel;
    
    // Set current labels for break/continue
    currentBreakLabel = endLabel;
    currentContinueLabel = conditionLabel;
    
    // Jump to condition check first
    instructions.emplace_back(TACOp::GOTO, conditionLabel);
    
    // Loop body label
    instructions.emplace_back(TACOp::LABEL, startLabel);
    
    // Generate loop body
    generateStatement(node->children[1]);
    
    // Condition check label
    instructions.emplace_back(TACOp::LABEL, conditionLabel);
    
    // Generate condition - UNTIL loops while condition is FALSE
    // So we invert: if condition is FALSE (0), jump to start
    string conditionTemp = generateExpression(node->children[0]);
    instructions.emplace_back(TACOp::IF_FALSE_GOTO, startLabel, conditionTemp);
    
    // End of loop
    instructions.emplace_back(TACOp::LABEL, endLabel);
    
    // Restore previous labels
    currentBreakLabel = prevBreakLabel;
    currentContinueLabel = prevContinueLabel;
}


string IRGenerator::generateSizeof(Node* node) {
    if (!node || node->children.empty()) return "";
    
    cout << "DEBUG: Generating sizeof" << endl;
    
    int size = 0;
    
    if (node->name == "SIZEOF_TYPE") {
        // sizeof(type): SIZEOF_TYPE -> SPEC_QUAL_LIST or TYPE_NAME
        Node* typeNode = node->children[0];
        size = getSizeofType(typeNode);
    }
    else if (node->name == "SIZEOF") {
        // sizeof(expr): SIZEOF -> EXPR_LIST -> expression
        Node* exprList = node->children[0];
        if (exprList && !exprList->children.empty()) {
            Node* expr = exprList->children[0];
            size = getSizeofExpression(expr);
        }
    }
    
    // Generate constant
    string temp = createTemp();
    instructions.emplace_back(TACOp::CONST, temp, to_string(size));
    return temp;
}

int IRGenerator::getSizeofType(Node* typeNode) {
    if (!typeNode) return 4;  // default
    
    string typeName = "";
    int pointerDepth = 0;
     for (auto child : typeNode->children) {
      
        if (child->name == "POINTER") {
            return 8;
        }
    }
    
    // Extract type from SPEC_QUAL_LIST or TYPE_NAME
    for (auto child : typeNode->children) {
        if (child->name == "TYPE_SPECIFIER") {
            typeName = child->lexeme;
        }
        else if (child->name == "SPEC_QUAL_LIST") {
            return getSizeofType(child);  // Recurse
        }
        else if (child->name == "POINTER") {
            pointerDepth++;
        }
    }
    
    // Any pointer is 8 bytes
    if (pointerDepth > 0) return 8;
    
    // Base type sizes
    if (typeName == "char") return 1;
    if (typeName == "short") return 2;
    if (typeName == "int") return 4;
    if (typeName == "long") return 8;
    if (typeName == "float") return 4;
    if (typeName == "double") return 8;
    if (typeName == "bool") return 1;
    
    return 4;  // default
}

int IRGenerator::getSizeofExpression(Node* exprNode) {
    if (!exprNode) return 4;
    
    // ✅ FIX: Handle member access (p.x or ptr->member)
    if (exprNode->name == "MEMBER_ACCESS" || exprNode->name == "PTR_MEMBER_ACCESS") {
        if (exprNode->children.size() < 2) return 4;
        
        Node* baseNode = exprNode->children[0];
        Node* memberNode = exprNode->children[1];
        
        if (memberNode->name != "IDENTIFIER") return 4;
        
        string memberName = memberNode->lexeme;
        
        // Get the base struct/union type
        string baseType = "";
        
        if (baseNode->name == "IDENTIFIER") {
            Symbol* baseSym = baseNode->symbol;
            
            if (baseSym) {
                baseType = baseSym->type;
                
                // Remove pointer level if arrow access
                if (exprNode->name == "PTR_MEMBER_ACCESS" && baseSym->pointerDepth > 0) {
                    // Already a pointer, just use base type
                }
            }
        }
        
        cout << "DEBUG sizeof: Member access base type = '" << baseType << "'" << endl;
        
        // Remove "struct " or "union " prefix
        if (baseType.find("struct ") == 0) {
            baseType = baseType.substr(7);
        } else if (baseType.find("union ") == 0) {
            baseType = baseType.substr(6);
        }
        
        // Look up struct and get member type
        Symbol* structSym = symbolTable.lookuph(baseType);
        
        if (structSym && structSym->isStruct) {
            auto it = structSym->structMembers.find(memberName);
            if (it != structSym->structMembers.end()) {
                string memberType = it->second;
                cout << "DEBUG sizeof: Member '" << memberName 
                     << "' has type '" << memberType << "'" << endl;
                
                // Check if member is a pointer
                if (memberType.find("*") != string::npos) {
                    return 8;
                }
                
                // Return member's base type size
                return getBaseTypeSize(memberType);
            } else {
                cout << "DEBUG sizeof: Member '" << memberName 
                     << "' not found in struct" << endl;
            }
        } else {
            cout << "DEBUG sizeof: Struct '" << baseType << "' not found" << endl;
        }
        
        return 4;  // fallback
    }
    
    // ✅ Handle array access (arr[i])
    if (exprNode->name == "ARRAY_ACCESS") {
        if (exprNode->children.empty()) return 4;
        
        Node* arrayBase = exprNode->children[0];
        
        // Find the base identifier
        while (arrayBase && arrayBase->name == "ARRAY_ACCESS") {
            if (arrayBase->children.empty()) break;
            arrayBase = arrayBase->children[0];
        }
        
        if (arrayBase && arrayBase->name == "IDENTIFIER") {
            Symbol* sym = arrayBase->symbol;
            if (sym) {
                // Array access gives element type
                int elemSize = getBaseTypeSize(sym->type);
                
                // If multi-dimensional, need to calculate correctly
                // For now, return element size
                return elemSize;
            }
        }
        
        return 4;  // fallback
    }
    
    // ✅ Handle pointer dereference (*ptr)
    if (exprNode->name == "UNARY_OP" && exprNode->children.size() >= 2) {
        Node* op = exprNode->children[0];
        Node* operand = exprNode->children[1];
         string opStr = op->name.empty() ? op->lexeme : op->name;
   
        if (op->name == "*") {  // Dereference
            if (operand->name == "IDENTIFIER") {
                Symbol* sym = operand->symbol;
                if (sym && sym->pointerDepth > 0) {
                    // Dereferencing reduces pointer level by 1
                    if (sym->pointerDepth == 1) {
                        // Now we have the base type
                        return getBaseTypeSize(sym->type);
                    } else {
                        // Still a pointer
                        return 8;
                    }
                }
            }
        }
         else if (opStr == "&") {
        // Address-of operator - always returns pointer (8 bytes)
        return 8;
    }
    else if (opStr == "+" || opStr == "-") {
        // Unary +/- preserves the type
        return getSizeofExpression(operand);
    }
    else if (opStr == "!" || opStr == "~") {
        // Logical/bitwise NOT returns int
        return 4;
    }
    }
    // Handle function calls - return type size
if (exprNode->name == "FUNC_CALL") {
    if (!exprNode->children.empty() && exprNode->children[0]->name == "IDENTIFIER") {
        string funcName = exprNode->children[0]->lexeme;
        Symbol* funcSym = symbolTable.lookup(funcName);
        
        if (funcSym && funcSym->isFunction) {
            return getBaseTypeSize(funcSym->type);
        }
    }
    return 4;  // Default to int
}
  // Handle assignment expressions (return LHS type size)
if (exprNode->name == "ASSIGN_EXPR") {
    if (!exprNode->children.empty()) {
        return getSizeofExpression(exprNode->children[0]);  // LHS type
    }
}  
// Handle ternary operator (return common type of true/false branches)
if (exprNode->name == "TERNARY_EXPR" || exprNode->name == "COND_EXPR") {
    if (exprNode->children.size() >= 3) {
        int trueSize = getSizeofExpression(exprNode->children[1]);
        int falseSize = getSizeofExpression(exprNode->children[2]);
        return max(trueSize, falseSize);  // Simplified: use larger type
    }
}
// Handle increment/decrement
if (exprNode->name == "POST_INC" || exprNode->name == "POST_DEC" ||
    exprNode->name == "PRE_INC" || exprNode->name == "PRE_DEC") {
    if (!exprNode->children.empty()) {
        return getSizeofExpression(exprNode->children[0]);
    }
}
    // For identifiers, look up in symbol table
    if (exprNode->name == "IDENTIFIER") {
        string varName = exprNode->lexeme;
        Symbol* sym = exprNode->symbol;
        
        if (sym != NULL) {
            cout << "DEBUG sizeof: Found symbol " << varName 
                 << " isArray=" << sym->isArray 
                 << " pointerDepth=" << sym->pointerDepth << endl;
            
            // If it's an array, return total size
            if (sym->isArray && !sym->arrayDimensions.empty()) {
                int elemSize = getBaseTypeSize(sym->type);
                int totalSize = elemSize;
                
                cout << "DEBUG sizeof: Base element size = " << elemSize << endl;
                
                for (int dim : sym->arrayDimensions) {
                    if (dim > 0) {
                        totalSize *= dim;
                        cout << "DEBUG sizeof: After dim " << dim 
                             << ", totalSize = " << totalSize << endl;
                    }
                }
                
                return totalSize;
            }
            
            // If it's a pointer, return 8
            if (sym->pointerDepth > 0) {
                cout << "DEBUG sizeof: Returning pointer size 8" << endl;
                return 8;
            }
            // In getSizeofExpression, after checking sym->type:
if (sym->isStruct || sym->isUnion) {
    // For structs/unions, calculate total size
    int totalSize = 0;
    for (const auto& member : sym->structMembers) {
        int memberSize = getBaseTypeSize(member.second);
        
        if (sym->isUnion) {
            // Union: size is max of all members
            totalSize = max(totalSize, memberSize);
        } else {
            // Struct: sum of all members (ignoring padding for now)
            totalSize += memberSize;
        }
    }
    return totalSize > 0 ? totalSize : 4;
}
            // Otherwise return base type size
            int size = getBaseTypeSize(sym->type);
            cout << "DEBUG sizeof: Returning base type size " << size << endl;
            return size;
        } else {
            cout << "DEBUG sizeof: Symbol " << varName << " not found!" << endl;
        }
    }
    
    // ✅ For other expression types, try to determine type
    // (add more cases as needed: string literals, constants, etc.)
    if (exprNode->name == "STRING_LITERAL") {
        // String literal is char array
        return exprNode->lexeme.length() + 1;  // +1 for null terminator
    }
    
    if (exprNode->name == "INTEGER_CONSTANT") {
        return 4;  // int
    }
    
    if (exprNode->name == "FLOAT_CONSTANT") {
        return 8;  // double (unless suffixed with 'f')
    }
    
    if (exprNode->name == "CHAR_LITERAL") {
        return 1;  // char
    }
    // Add after literal checks in getSizeofExpression:

// Handle binary arithmetic operations
if (exprNode->name == "ADD_EXPR" || exprNode->name == "SUB_EXPR" ||
    exprNode->name == "MUL_EXPR" || exprNode->name == "DIV_EXPR" ||
    exprNode->name == "MOD_EXPR") {
    
    if (exprNode->children.size() >= 2) {
        int leftSize = getSizeofExpression(exprNode->children[0]);
        int rightSize = getSizeofExpression(exprNode->children[1]);
        
        // Arithmetic promotion rules (simplified):
        // double > float > long > int
        if (leftSize == 8 || rightSize == 8) return 8;  // double or long
        if (leftSize == 4 || rightSize == 4) return 4;  // float or int
        if (leftSize == 2 || rightSize == 2) return 2;  // short
        return 1;  // char
    }
}

// Handle comparison operations (always return int/bool size)
if (exprNode->name == "EQ_EXPR" || exprNode->name == "NEQ_EXPR" ||
    exprNode->name == "LT_EXPR" || exprNode->name == "GT_EXPR" ||
    exprNode->name == "LE_EXPR" || exprNode->name == "GE_EXPR") {
    return 4;  // Comparisons return bool/int
}

// Handle logical operations (return int/bool size)
if (exprNode->name == "LOGICAL_AND" || exprNode->name == "LOGICAL_OR") {
    return 4;  // Logical ops return bool/int
}

// Handle bitwise operations (return integer type size)
if (exprNode->name == "BIT_AND" || exprNode->name == "BIT_OR" ||
    exprNode->name == "BIT_XOR" || exprNode->name == "LSHIFT_EXPR" ||
    exprNode->name == "RSHIFT_EXPR") {
    
    if (exprNode->children.size() >= 2) {
        int leftSize = getSizeofExpression(exprNode->children[0]);
        int rightSize = getSizeofExpression(exprNode->children[1]);
        return max(leftSize, rightSize);  // Return larger operand size
    }
}
    
    return 4;  // default
}

int IRGenerator::getBaseTypeSize(const string& typeName) {
    if (typeName == "char") return 1;
    if (typeName == "short") return 2;
    if (typeName == "int") return 4;
    if (typeName == "long") return 8;
    if (typeName == "float") return 4;
    if (typeName == "double") return 8;
    if (typeName == "bool") return 1;
    
    return 4;  // default
}
