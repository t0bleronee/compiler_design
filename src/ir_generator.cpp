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
            
        default:
            ss << "UNKNOWN_OP";
    }
    
    return ss.str();
}

// IRGenerator implementation
IRGenerator::IRGenerator(SymbolTable& symTab) 
    : symbolTable(symTab), tempCounter(0), labelCounter(0), currentFunction("") {}

bool IRGenerator::generateIR(Node* ast) {
    if (!ast) return false;
    
    cout << "=== Generating TAC IR ===" << endl;
    
    // Reset state
    instructions.clear();
    tempCounter = 0;
    labelCounter = 0;
    currentFunction = "";
    
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
            bool hasNestedFunction = false;
            for (auto grandchild : child->children) {
                if (grandchild->name == "FUNCTION_DEFINITION") {
                    hasNestedFunction = true;
                    break;
                }
            }
            
            if (hasNestedFunction) {
                traverseAST(child);
            } else {
                generateFunction(child);
            }
            continue;
        }
        
        // Handle global declarations
        if (child->name == "DECLARATION") {
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
            
            if (!isTypeDecl && !currentFunction.empty()) {
                generateDeclaration(child);
            } else if (!isTypeDecl && currentFunction.empty()) {
                generateDeclaration(child);
            }
            continue;
        }
        
        // Handle struct/union definitions
        if (child->name == "STRUCT_OR_UNION_SPECIFIER") {
            string structName = findIdentifier(child);
            if (!structName.empty()) {
                cout << "DEBUG: Found struct " << structName << " (skipping IR)" << endl;
            }
            continue;
        }
        
        // Handle enum definitions
        if (child->name == "ENUM_SPECIFIER") {
            cout << "DEBUG: Found enum definition (skipping IR)" << endl;
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
    else if (node->name == "RETURN_STMT") {
        generateReturnStatement(node);
    }
    else if (node->name == "DECLARATION") {
        generateDeclaration(node);
    }
    else if (node->name == "FUNCTION_DEFINITION") {
        generateFunction(node);
    }
    else {
        cout << "DEBUG: Falling back to processing children for " << node->name << endl;
        for (auto child : node->children) {
            generateStatement(child);
        }
    }
}

bool IRGenerator::hasStructBody(Node* node) {
    if (!node) return false;
    
    for (auto child : node->children) {
        if (child->name == "STRUCT_DECL_LIST" || child->name == "DECLARATION") {
            return true;
        }
    }
    return false;
}

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
    else if (node->name == "ASSIGN_EXPR") {
        return generateAssignment(node);
    }
    else if (node->name == "FUNC_CALL") {
        return generateFunctionCall(node);
    }
    else if (node->name == "UNARY_OP") {
        if (node->children.size() >= 2) {
            Node* opNode = node->children[0];
            Node* operandNode = node->children[1];
            
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
        if (node->children[0]->name == "GT_EXPR" || node->children[0]->name == "LT_EXPR" ||
            node->children[0]->name == "EQ_EXPR" || node->children[0]->name == "NEQ_EXPR") {
            return generateExpression(node->children[0]);
        }
        return generateExpression(node->children[0]);
    }
    
    return "";
}

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
    return TACOp::ADD;
}

TACOp IRGenerator::getUnaryOp(const string& nodeName) {
    if (nodeName == "NEG") return TACOp::NEG;
    if (nodeName == "NOT") return TACOp::NOT;
    return TACOp::NEG;
}

string IRGenerator::generateBinaryExpr(Node* node, TACOp op) {
    if (!node || node->children.size() < 2) return "";
    
    string leftTemp = generateExpression(node->children[0]);
    string rightTemp = generateExpression(node->children[1]);
    string resultTemp = createTemp();
    
    instructions.emplace_back(op, resultTemp, leftTemp, rightTemp);
    return resultTemp;
}

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

void IRGenerator::generateDeclaration(Node* node) {
    if (!node) return;
    
    cout << "DEBUG: Generating declaration" << endl;
    
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
    
    for (auto child : actualFunctionDef->children) {
        if (child->name == "FUNCTION_DECL" || child->name == "IDENTIFIER") {
            declarator = child;
            for (auto declChild : child->children) {
                if (declChild->name == "PARAM_TYPE_LIST" || declChild->name == "PARAM_LIST") {
                    paramList = declChild;
                }
            }
        } else if (child->name == "COMPOUND_STMT") {
            body = child;
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
    
    for (auto param : paramList->children) {
        if (param->name == "PARAM_DECL") {
            string paramName = findParameterName(param);
            if (!paramName.empty()) {
                cout << "DEBUG: Found parameter: " << paramName << endl;
            }
        }
    }
}

string IRGenerator::findParameterName(Node* paramDecl) {
    if (!paramDecl) return "";
    
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
        if (node->children.size() >= 2) {
            string conditionTemp = generateExpression(node->children[0]);
            string endLabel = createLabel("endif");
            
            instructions.emplace_back(TACOp::IF_FALSE_GOTO, endLabel, conditionTemp);
            generateStatement(node->children[1]);
            instructions.emplace_back(TACOp::LABEL, endLabel);
        }
    }
    else if (node->name == "IF_ELSE_STMT") {
        if (node->children.size() >= 3) {
            string conditionTemp = generateExpression(node->children[0]);
            string elseLabel = createLabel("else");
            string endLabel = createLabel("endif");
            
            instructions.emplace_back(TACOp::IF_FALSE_GOTO, elseLabel, conditionTemp);
            generateStatement(node->children[1]);
            instructions.emplace_back(TACOp::GOTO, endLabel);
            instructions.emplace_back(TACOp::LABEL, elseLabel);
            generateStatement(node->children[2]);
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
    
    // Jump to condition check
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
