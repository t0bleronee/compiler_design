#include "ir_generator.h"
#include <fstream>
#include <sstream>
#include <functional>
#include <cctype>


// Helper: find the base IDENTIFIER inside nested ARRAY nodes
Node* IRGenerator::findIdentifierInArray(Node* arrayNode) {
    if (!arrayNode) return nullptr;
    Node* current = arrayNode;
    while (current && current->name == "ARRAY") {
        if (current->children.empty()) break;
        Node* first = current->children[0];
        if (!first) break;
        if (first->name == "IDENTIFIER") return first;
        // Handle pointer-to-array: ARRAY -> DECLARATOR -> POINTER + IDENTIFIER
        if (first->name == "DECLARATOR") {
            // Recursively search for IDENTIFIER in DECLARATOR
            std::function<Node*(Node*)> findId = [&](Node* n) -> Node*{
                if (!n) return nullptr;
                if (n->name == "IDENTIFIER") return n;
                for (auto c : n->children) {
                    Node* r = findId(c);
                    if (r) return r;
                }
                return nullptr;
            };
            return findId(first);
        }
        current = first;
    }
    return nullptr;
}

using namespace std;

// Global IR debug toggle. Set to true to enable verbose IR generation logs.
static const bool kIrDebug = false;

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
           if (paramCount > 0) {
            ss << result << " = call " << operand1 << ", " << paramCount;
              } else {
                  ss << result << " = call " << operand1;
              }
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
        case TACOp::FUNC_BEGIN:
            ss << "func_begin " << result;
            break;
            
        case TACOp::FUNC_END:
            ss << "func_end " << result;
            break;
            
        case TACOp::GET_PARAM:
            ss << result << " = param[" << operand1 << "]";
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
        case TACOp::CAST:
            ss << result << " = (" << operand2 << ")" << operand1;
            break;
        
        case TACOp::COMMENT:
            ss << "# " << result;
            break;
        
        case TACOp::NOP:
            ss << "nop";
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

// Basic utilities
void IRGenerator::printIR(std::ostream& out) const {
    for (const auto& inst : instructions) {
        inst.print(out);
    }
}

void IRGenerator::writeToFile(const std::string& filename) const {
    std::string outFile = filename.empty() ? std::string("output.3ac") : filename;
    std::ofstream ofs(outFile);
    if (!ofs) {
        std::cerr << "Failed to open IR output file: " << outFile << std::endl;
        return;
    }
    printIR(ofs);
}

std::string IRGenerator::createTemp() {
    return std::string("t") + std::to_string(++tempCounter);
}

std::string IRGenerator::createLabel(const std::string& prefix) {
    return prefix + std::string("_") + std::to_string(++labelCounter);
}

// Helpers used by traverse/statement generation
bool IRGenerator::hasStructBody(Node* node) {
    if (!node) return false;
    // Heuristic: look for an explicit member list node or braces content
    std::function<bool(Node*)> anyStructMembers = [&](Node* n) -> bool {
        if (!n) return false;
        if (n->name == "STRUCT_DECLARATION_LIST" || n->name == "STRUCT_DECLARATION" ||
            n->name == "MEMBER_LIST" || n->name == "MEMBER_DECLARATION") return true;
        for (auto c : n->children) if (anyStructMembers(c)) return true;
        return false;
    };
    return anyStructMembers(node);
}

bool IRGenerator::hasEnumBody(Node* node) {
    if (!node) return false;
    std::function<bool(Node*)> anyEnumers = [&](Node* n) -> bool {
        if (!n) return false;
        if (n->name == "ENUMERATOR_LIST" || n->name == "ENUMERATOR") return true;
        for (auto c : n->children) if (anyEnumers(c)) return true;
        return false;
    };
    return anyEnumers(node);
}

std::string IRGenerator::findIdentifier(Node* node) {
    if (!node) return "";
    if (node->name == "IDENTIFIER") return node->lexeme;
    for (auto c : node->children) {
        std::string r = findIdentifier(c);
        if (!r.empty()) return r;
    }
    return "";
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

static unsigned char decodeCharLiteralIR(const std::string& literal) {
    if (literal.size() < 3) return 0;
    size_t q = literal.find('\'');
    if (q == std::string::npos) q = literal.find('"');
    if (q == std::string::npos || q + 1 >= literal.size()) return 0;
    size_t p = q + 1;
    if (literal[p] != '\\') return static_cast<unsigned char>(literal[p]);
    if (p + 1 >= literal.size()) return 0;
    char e = literal[p + 1];
    if (e == 'x') {
        unsigned int v = 0; bool any = false; size_t i = p + 2;
        while (i < literal.size() && isHex(literal[i])) { any = true; v = (v << 4) | (unsigned)hexVal(literal[i]); ++i; }
        return any ? (unsigned char)(v & 0xFF) : 0;
    }
    if (e == 'u') {
        unsigned int v = 0; size_t i = p + 2;
        for (int k = 0; k < 4 && i < literal.size(); ++k, ++i) { if (!isHex(literal[i])) return 0; v = (v << 4) | (unsigned)hexVal(literal[i]); }
        return (unsigned char)(v & 0xFF);
    }
    if (e == 'U') {
        unsigned int v = 0; size_t i = p + 2;
        for (int k = 0; k < 8 && i < literal.size(); ++k, ++i) { if (!isHex(literal[i])) return 0; v = (v << 4) | (unsigned)hexVal(literal[i]); }
        return (unsigned char)(v & 0xFF);
    }
    if (e >= '0' && e <= '7') {
        unsigned int v = 0; size_t i = p + 1; int cnt = 0;
        while (i < literal.size() && cnt < 3 && literal[i] >= '0' && literal[i] <= '7') { v = (v << 3) | (unsigned)(literal[i] - '0'); ++i; ++cnt; }
        return (unsigned char)(v & 0xFF);
    }
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
        default: return (unsigned char)e;
    }
}

static std::string stripQuotes(const std::string& s) {
    if (s.size() >= 2 && ((s.front() == '"' && s.back() == '"') || (s.front() == '\'' && s.back() == '\''))) {
        return s.substr(1, s.size() - 2);
    }
    // Also handle optional prefix like L"..." or u8"..." or u"..."
    size_t first = s.find('"');
    if (first != std::string::npos && s.back() == '"' && first + 1 < s.size()) {
        return s.substr(first + 1, s.size() - first - 2);
    }
    return s;
}

static std::vector<unsigned char> decodeCString(const std::string& rawWithQuotes) {
    std::string raw = stripQuotes(rawWithQuotes);
    std::vector<unsigned char> out;
    for (size_t i = 0; i < raw.size(); ++i) {
        unsigned char c = raw[i];
        if (c != '\\') { out.push_back(c); continue; }
        if (i + 1 >= raw.size()) { out.push_back('\\'); break; }
        char e = raw[++i];
        switch (e) {
            case '\\': out.push_back('\\'); break;
            case '\'': out.push_back('\''); break;
            case '"': out.push_back('"'); break;
            case 'n': out.push_back('\n'); break;
            case 't': out.push_back('\t'); break;
            case 'r': out.push_back('\r'); break;
            case '0': out.push_back('\0'); break;
            case 'b': out.push_back('\b'); break;
            case 'f': out.push_back('\f'); break;
            case 'v': out.push_back('\v'); break;
            case 'a': out.push_back('\a'); break;
            case '?': out.push_back('?'); break;
            case 'x': {
                unsigned int v = 0; bool any = false;
                while (i + 1 < raw.size() && isHex(raw[i+1])) { any = true; v = (v << 4) | (unsigned)hexVal(raw[++i]); }
                out.push_back(any ? (unsigned char)(v & 0xFF) : (unsigned char)0);
                break;
            }
            case 'u': {
                unsigned int v = 0; int k = 0; while (k < 4 && i + 1 < raw.size() && isHex(raw[i+1])) { v = (v << 4) | (unsigned)hexVal(raw[++i]); ++k; }
                out.push_back((unsigned char)(v & 0xFF));
                break;
            }
            case 'U': {
                unsigned int v = 0; int k = 0; while (k < 8 && i + 1 < raw.size() && isHex(raw[i+1])) { v = (v << 4) | (unsigned)hexVal(raw[++i]); ++k; }
                out.push_back((unsigned char)(v & 0xFF));
                break;
            }
            default: {
                if (e >= '0' && e <= '7') {
                    unsigned int v = (unsigned)(e - '0'); int cnt = 1;
                    while (cnt < 3 && i + 1 < raw.size() && raw[i+1] >= '0' && raw[i+1] <= '7') { v = (v << 3) | (unsigned)(raw[++i] - '0'); ++cnt; }
                    out.push_back((unsigned char)(v & 0xFF));
                } else {
                    out.push_back((unsigned char)e);
                }
            }
        }
    }
    return out;
}

}

// Produce a unique IR name for an identifier using its bound Symbol
std::string IRGenerator::getUniqueNameFor(Node* idNode, const std::string& fallback) {
    if (!idNode) return fallback;
    
    std::string base = idNode->lexeme.empty() ? fallback : idNode->lexeme;
    
    // If this node has a Symbol attached, use symbol-based lookup first
    // This ensures each Symbol (even with same name in different scopes) gets unique name
    if (idNode->symbol) {
        // Check symbol-based map first
        auto it = symbolUniqueNames.find(idNode->symbol);
        if (it != symbolUniqueNames.end()) {
            return it->second;
        }
        
        // CRITICAL FIX: For parameters, check functionLocalNames before creating new name
        // This handles the case where struct parameters are received as separate members
        // but the parameter variable itself needs to be referenced later
        if (!currentFunction.empty() && !idNode->symbol->isLocal) {
            // This is a parameter (not a local variable)
            auto localIt = functionLocalNames.find(base);
            if (localIt != functionLocalNames.end()) {
                // Reuse the parameter's SSA name and register the symbol
                symbolUniqueNames[idNode->symbol] = localIt->second;
                return localIt->second;
            }
        }
        
        // GLOBAL VARIABLE FIX: Don't add scope suffix to global variables
        // Globals need fixed names so code generator can place them in .data section
        std::string unique;
        if (currentFunction.empty() || (!idNode->symbol->isLocal && idNode->symbol->isStatic)) {
            // Global variable or file-scope static - use base name without suffix
            unique = base;
        } else {
            // Local variable or parameter - create unique name: baseName#N
            unique = base + "#" + std::to_string(++symbolNameCounter);
        }
        symbolUniqueNames[idNode->symbol] = unique;
        
        // For local variables in current function, also update the name map
        // This allows shadowed variables to each have their own unique name
        if (!currentFunction.empty() && idNode->symbol->isLocal) {
            // Don't check if name exists - just update it to the NEW symbol's unique name
            // This handles shadowing: inner scope's variable gets a new unique name
            functionLocalNames[base] = unique;
        }
        return unique;
    }
    
    // Fallback: check function-local name map (for cases where Symbol* is not attached)
    if (!currentFunction.empty()) {
        auto localIt = functionLocalNames.find(base);
        if (localIt != functionLocalNames.end()) {
            return localIt->second;
        }
    }
    
    return fallback;
}
bool IRGenerator::generateIR(Node* ast) {
    if (!ast) return false;
    
    cout << "=== Generating TAC IR ===" << endl;
    
    // Reset state
    instructions.clear();
    tempCounter = 0;
    labelCounter = 0;
    currentFunction = "";
     enumConstants.clear();  // Add this
    // Reset string literal pool
    stringLiteralCounter = 0;
    stringLiterals.clear();
    
    // First pass: collect enum constants
    collectEnumConstants(ast);
   
    traverseAST(ast);
    
    // OPTIMIZATION: Peephole optimization pass to remove redundant instructions
    optimizeTAC();
    
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
                            (specChild->name == "ENUM_SPECIFIER" && hasEnumBody(specChild))) {
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
    else if (node->name == "PRINTF") {
        // Explicit handling for printf statements
        generatePrintfStatement(node);
    }
    else if (node->name == "SCANF") {
        // Explicit handling for scanf statements
        generateScanfStatement(node);
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
    else if (node->name == "DECLARATION") {
        // Handle local declarations inside function bodies
        bool isTypeDecl = false;
        for (auto declChild : node->children) {
            if (declChild->name == "TYPEDEF") {
                isTypeDecl = true;
                break;
            }
            if (declChild->name == "DECL_SPECIFIERS") {
                for (auto specChild : declChild->children) {
                    if (specChild->name == "TYPEDEF" || 
                        specChild->name == "STORAGE_CLASS_SPECIFIER" ||
                        (specChild->name == "STRUCT_OR_UNION_SPECIFIER" && hasStructBody(specChild)) ||
                        (specChild->name == "ENUM_SPECIFIER" && hasEnumBody(specChild))) {
                        isTypeDecl = true;
                        break;
                    }
                }
            }
        }
        if (!isTypeDecl) {
            generateDeclaration(node);
        }
    }
    
    else if (node->name == "IF_STMT" || node->name == "IF_ELSE_STMT") {
        generateIfStatement(node);
    }
    else if (node->name == "WHILE_STMT") {
        generateWhileStatement(node);
    }
    else if (node->name == "DO_WHILE_STMT") {
        generateDoWhileStatement(node);
    }
    else if (node->name == "UNTIL_STMT") {
        generateUntilStatement(node);
    }
    else if (node->name == "FOR_STMT_2" || node->name == "FOR_STMT_3") {
        generateForStatement(node);
    }
    else if (node->name == "SWITCH_STMT") {
        generateSwitchStatement(node);
    }
    else if (node->name == "UNARY_OP") {
        // Unary operations as statements: evaluate for side effects only
        if (!node->children.empty()) {
            generateExpression(node);
        }
    }
    // Ensure standalone ++/-- statements generate side effects
    else if (node->name == "PRE_INC" || node->name == "PRE_DEC" ||
             node->name == "POST_INC" || node->name == "POST_DEC") {
        generateExpression(node);
    }
    else if (node->name == "RETURN_STMT") {
        generateReturnStatement(node);
    }
    else if (node->name == "BREAK_STMT") {
        generateBreakStatement();
    }
    else if (node->name == "CONTINUE_STMT") {
        generateContinueStatement();
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
string IRGenerator::generateExpression(Node* node) {
    if (!node) return "";
    
    cout << "DEBUG: Generating expression for " << node->name << endl;
  
   // Handle expression list: evaluate left-to-right and return last value
   if (node->name == "EXPR_LIST") {
       string lastTemp;
       for (auto child : node->children) {
           lastTemp = generateExpression(child);
       }
       return lastTemp;
   }

    if (node->name == "IDENTIFIER") {
    // Check if it's an enum constant
    if (enumConstants.find(node->lexeme) != enumConstants.end()) {
        string temp = createTemp();
        instructions.emplace_back(TACOp::CONST, temp,to_string(enumConstants[node->lexeme]));
        return temp;
    }
    // Function identifiers decay to function addresses in expressions
    if (node->symbol && node->symbol->isFunction) {
        // Generate explicit address-of for clarity in TAC
        string temp = createTemp();
        instructions.emplace_back(TACOp::ADDRESS, temp, node->lexeme);
        return temp;
    }
    // Array-to-pointer decay: when an array name is used in an expression context,
    // it decays to a pointer to its first element
    if (node->symbol && node->symbol->isArray) {
        string arrayName = getUniqueNameFor(node, node->lexeme);
        string temp = createTemp();
        instructions.emplace_back(TACOp::ADDRESS, temp, arrayName);
        // Mark temp as pointer to array element type
        tempPointerDepth[temp] = 1;  // Pointer to element
        return temp;
    }
    // References hold addresses; reading a reference as rvalue
    // - scalar refs: LOAD through address
    // - array refs: return the address directly (decays to pointer)
    if (node->symbol && node->symbol->isReference) {
        string addrVar = getUniqueNameFor(node, node->lexeme);
        if (node->symbol->isArray) {
            return addrVar;
        } else {
            string temp = createTemp();
            instructions.emplace_back(TACOp::LOAD, temp, addrVar);
            return temp;
        }
    }
    // Use unique name per bound symbol (handles shadowing)
    return getUniqueNameFor(node, node->lexeme);
}
    else if (node->name == "INTEGER_CONSTANT" || node->name == "FLOAT_CONSTANT" || 
             node->name == "CHAR_LITERAL" || node->name == "BOOL_LITERAL") {
        string temp = createTemp();
        if (node->name == "CHAR_LITERAL") {
            unsigned char ch = decodeCharLiteralIR(node->lexeme);
            instructions.emplace_back(TACOp::CONST, temp, to_string((unsigned int)ch));
        } else {
            instructions.emplace_back(TACOp::CONST, temp, node->lexeme);
        }
        return temp;
    }
   else if (node->name == "NULLPTR_CONSTANT" || node->name == "NULL_CONSTANT") {
    // nullptr/null are treated as 0 (null pointer constant)
    string temp = createTemp();
    instructions.emplace_back(TACOp::CONST, temp, "0");
    return temp;
}
  
    else if (node->name == "STRING_LITERAL") {
    // Generate a label for this string literal and return its address as a CONST label
    string label = "str" + to_string(++stringLiteralCounter);
    stringLiterals[label] = node->lexeme; // keep raw (with quotes)
    string temp = createTemp();
    instructions.emplace_back(TACOp::CONST, temp, label);
    return temp;
}
    // Basic cast expression handling: evaluate inner expression and pass value through
    else if (node->name == "CAST_EXPR") {
        // Find the target type from TYPE_NAME or SPEC_QUAL_LIST
        string targetType = "";
        int pointerDepth = 0;
        Node* typeNode = nullptr;
        
        for (auto child : node->children) {
            if (child && child->name == "TYPE_NAME") {
                typeNode = child;
                // Extract type from TYPE_NAME's children
                for (auto typeChild : child->children) {
                    if (typeChild && typeChild->name == "SPEC_QUAL_LIST") {
                        targetType = extractTypeFromSpecQualList(typeChild);
                    } else if (typeChild && typeChild->name == "POINTER") {
                        pointerDepth++;
                    }
                }
                break;
            } else if (child && child->name == "SPEC_QUAL_LIST") {
                targetType = extractTypeFromSpecQualList(child);
                break;
            }
        }
        
        // Add pointer stars to type
        for (int i = 0; i < pointerDepth; i++) {
            targetType += "*";
        }
        
        // Find the operand expression inside the cast (skip TYPE_NAME/SPEC_QUAL_LIST)
        Node* exprNode = nullptr;
        for (auto child : node->children) {
            if (child && child->name != "SPEC_QUAL_LIST" && child->name != "TYPE_NAME") {
                exprNode = child;
                break;
            }
        }
        if (!exprNode && !node->children.empty()) {
            exprNode = node->children.back();
        }
        
        // Unwrap EXPR_LIST to get the actual expression
        while (exprNode && exprNode->name == "EXPR_LIST" && !exprNode->children.empty()) {
            exprNode = exprNode->children[0];
        }
        
        string valTemp = exprNode ? generateExpression(exprNode) : string("");
        
        if (!valTemp.empty() && !targetType.empty()) {
            // Get the source type
            string sourceType = getExpressionResultType(exprNode);
            
            // Only emit cast if types differ
            if (normalizeTypeName(sourceType) != normalizeTypeName(targetType)) {
                string resultTemp = createTemp();
                instructions.emplace_back(TACOp::CAST, resultTemp, valTemp, targetType);
                return resultTemp;
            }
        }
        
        return valTemp;
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
        if (node->children[1]->name == "OP") {
            string op = node->children[1]->lexeme;
            if (op != "=") {
                cout << "DEBUG: Routing to generateCompoundAssignment for " << op << endl;
                return generateCompoundAssignment(node);
            }
        }
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
    else if (node->name == "VA_ARG") {
        // Children: [0] va_list expr, [1] TYPE_NAME or SPEC_QUAL_LIST (requested type)
        // Generate runtime index tracking for variadic arguments
        
        // Extract the va_list variable name (from first child which is an IDENTIFIER)
        string vaListName = "";
        if (node->children.size() > 0 && node->children[0]) {
            if (node->children[0]->name == "IDENTIFIER") {
                vaListName = getUniqueNameFor(node->children[0], node->children[0]->lexeme);
            }
        }
        
        // Get requested type
        string targetType = "int"; // default fallback
        int pointerDepth = 0;
        if (node->children.size() >= 2) {
            Node* typeNode = node->children[1];
            if (typeNode) {
                if (typeNode->name == "TYPE_NAME") {
                    for (auto c : typeNode->children) {
                        if (!c) continue;
                        if (c->name == "SPEC_QUAL_LIST") {
                            targetType = extractTypeFromSpecQualList(c);
                        } else if (c->name == "POINTER") {
                            // Count pointer depth - POINTER node itself is one level
                            function<int(Node*)> countPointers = [&](Node* n) -> int {
                                if (!n) return 0;
                                int count = 0;
                                if (n->name == "POINTER") count = 1;
                                for (auto child : n->children) {
                                    count += countPointers(child);
                                }
                                return count;
                            };
                            pointerDepth = countPointers(c);
                        }
                    }
                } else if (typeNode->name == "SPEC_QUAL_LIST") {
                    targetType = extractTypeFromSpecQualList(typeNode);
                }
            }
        }
        
        // Append pointer stars to type
        for (int i = 0; i < pointerDepth; i++) {
            targetType += "*";
        }
        
        // The va_list variable name is now being used as an index tracker
        // Read current index, fetch param, then increment
        string indexTemp = createTemp();
        instructions.emplace_back(TACOp::ASSIGN, indexTemp, vaListName);
        
        cout << "DEBUG: va_arg(" << vaListName << ", " << targetType << ") -> param[" << vaListName << "]" << endl;
        
        // Generate: result = param[indexTemp]
        string resultTemp = createTemp();
        instructions.emplace_back(TACOp::GET_PARAM, resultTemp, indexTemp);
        
        // Increment the index for next call: vaListName = vaListName + 1
        string oneTemp = createTemp();
        instructions.emplace_back(TACOp::CONST, oneTemp, "1");
        string newIndexTemp = createTemp();
        instructions.emplace_back(TACOp::ADD, newIndexTemp, vaListName, oneTemp);
        instructions.emplace_back(TACOp::ASSIGN, vaListName, newIndexTemp);
        
        // Cast to requested type if needed
        if (targetType != "int") {
            string castedTemp = createTemp();
            instructions.emplace_back(TACOp::CAST, castedTemp, resultTemp, targetType);
            return castedTemp;
        }
        
        return resultTemp;
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
        
        string resultTemp = createTemp();
        
       if (op == "&") {
            // ✅ FIX: Special case for &arr[i]
            if (operandNode->name == "ARRAY_ACCESS") {
                return generateArrayAddress(operandNode);
            }
            // ✅ NEW: &function is same as function pointer value
            if (operandNode->name == "IDENTIFIER" && operandNode->symbol && operandNode->symbol->isFunction) {
                return generateExpression(operandNode);
            }
            // ✅ NEW: &array_identifier should directly generate the address, not double-address
            if (operandNode->name == "IDENTIFIER" && operandNode->symbol && operandNode->symbol->isArray) {
                string arrayName = getUniqueNameFor(operandNode, operandNode->lexeme);
                string addrTemp = createTemp();
                instructions.emplace_back(TACOp::ADDRESS, addrTemp, arrayName);
                return addrTemp;
            }
            // ✅ NEW: Special case for &(*expr) — returns the pointer value of expr
            if (operandNode->name == "UNARY_OP" && operandNode->children.size() >= 2) {
                Node* innerOp = operandNode->children[0];
                string inner = innerOp->name.empty() ? innerOp->lexeme : innerOp->name;
                if (inner == "*") {
                    // Address-of dereference cancels: &(*E) == E (as address)
                    string ptrTemp = generateExpression(operandNode->children[1]);
                    // Ensure we pass through the address value directly
                    return ptrTemp;
                }
            }
            // ✅ NEW: Handle & on struct/union member access without loading the value
            if (operandNode->name == "MEMBER_ACCESS" || operandNode->name == "DIRECT_MEMBER" || operandNode->name == "DOT") {
                // struct.member -> take address of member
                if (operandNode->children.size() >= 2 && operandNode->children[1]->name == "IDENTIFIER") {
                    Node* structNode = operandNode->children[0];
                    string memberName = operandNode->children[1]->lexeme;
                    int offset = getMemberOffset(structNode, memberName);
                    string structTemp = generateExpression(structNode);
                    string baseAddr = createTemp();
                    string offsetTemp = createTemp();
                    string memberAddr = createTemp();
                    // For direct member access, get address of struct variable
                    instructions.emplace_back(TACOp::ADDRESS, baseAddr, structTemp);
                    instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
                    instructions.emplace_back(TACOp::ADD, memberAddr, baseAddr, offsetTemp);
                    return memberAddr;
                }
            }
            
            // ✅ NEW: Handle & on array element: &array[i] should return address, not load value
            if (operandNode->name == "ARRAY_ACCESS") {
                // Use generateArrayAddressOnly to get address without loading
                string arrayElemAddr = generateArrayAddressOnly(operandNode);
                return arrayElemAddr;
            }
            if (operandNode->name == "PTR_MEMBER_ACCESS" || operandNode->name == "ARROW") {
                // ptr->member -> base is already an address
                if (operandNode->children.size() >= 2 && operandNode->children[1]->name == "IDENTIFIER") {
                    Node* structPtrNode = operandNode->children[0];
                    string memberName = operandNode->children[1]->lexeme;
                    int offset = getMemberOffset(structPtrNode, memberName);
                    string structPtrTemp = generateExpression(structPtrNode);
                    string baseAddr = createTemp();
                    string offsetTemp = createTemp();
                    string memberAddr = createTemp();
                    instructions.emplace_back(TACOp::ASSIGN, baseAddr, structPtrTemp);
                    instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
                    instructions.emplace_back(TACOp::ADD, memberAddr, baseAddr, offsetTemp);
                    return memberAddr;
                }
            }
            
            // Normal address-of for variables
            {
                string operandTemp = generateExpression(operandNode);
                instructions.emplace_back(TACOp::ADDRESS, resultTemp, operandTemp);
            }
            return resultTemp;
        } else if (op == "*") {
            string operandTemp = generateExpression(operandNode);
            instructions.emplace_back(TACOp::LOAD, resultTemp, operandTemp);
        } else if (op == "-") {
            string operandTemp = generateExpression(operandNode);
            instructions.emplace_back(TACOp::NEG, resultTemp, operandTemp);
        } else if (op == "!") {
            string operandTemp = generateExpression(operandNode);
              string operandType = getExpressionResultType(operandNode);
    if (operandType != "bool") {
        string boolOperand = createTemp();
        instructions.emplace_back(TACOp::CAST, boolOperand, operandTemp, "bool");
        operandTemp = boolOperand;
    }
            instructions.emplace_back(TACOp::NOT, resultTemp, operandTemp);
        } else if (op == "~") {
            string operandTemp = generateExpression(operandNode);
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
     string leftType = getExpressionResultType(node->children[0]);
    
     // OPTIMIZATION: Check if last instruction was already a bool cast of leftTemp
     bool alreadyCast = false;
     if (!instructions.empty()) {
         auto& lastInstr = instructions.back();
         if (lastInstr.opcode == TACOp::CAST && lastInstr.operand2 == "bool" && lastInstr.operand1 == leftTemp) {
             // Already casted to bool, use the result
             leftTemp = lastInstr.result;
             alreadyCast = true;
         }
     }
     
     if (!alreadyCast && leftType != "bool" ) {
        string leftBool = createTemp();
        instructions.emplace_back(TACOp::CAST, leftBool, leftTemp, "bool");
        leftTemp = leftBool;
        cout << "DEBUG: Logical AND - casting left operand to bool" << endl;
    }
    
    string resultTemp = createTemp();
    string falseLabel = createLabel("logical_false");
    string endLabel = createLabel("logical_end");
    
    // Check left operand
    instructions.emplace_back(TACOp::IF_FALSE_GOTO, falseLabel, leftTemp);
    
    // Left is true, evaluate right
    string rightTemp = generateExpression(node->children[1]);
     string rightType = getExpressionResultType(node->children[1]);
    
    // Cast right operand to bool if needed
    // OPTIMIZATION: Check if last instruction was already a bool cast of rightTemp
    bool alreadyCastRight = false;
    if (!instructions.empty()) {
        auto& lastInstr = instructions.back();
        if (lastInstr.opcode == TACOp::CAST && lastInstr.operand2 == "bool" && lastInstr.operand1 == rightTemp) {
            // Already casted to bool, use the result
            rightTemp = lastInstr.result;
            alreadyCastRight = true;
        }
    }
    
    if (!alreadyCastRight && rightType != "bool") {
        string rightBool = createTemp();
        instructions.emplace_back(TACOp::CAST, rightBool, rightTemp, "bool");
        rightTemp = rightBool;
        cout << "DEBUG: Logical AND - casting right operand to bool" << endl;
    }
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
    string leftType = getExpressionResultType(node->children[0]);
    
    // Cast left operand to bool if needed
    // OPTIMIZATION: Check if last instruction was already a bool cast of leftTemp
    bool alreadyCastLeft = false;
    if (!instructions.empty()) {
        auto& lastInstr = instructions.back();
        if (lastInstr.opcode == TACOp::CAST && lastInstr.operand2 == "bool" && lastInstr.operand1 == leftTemp) {
            // Already casted to bool, use the result
            leftTemp = lastInstr.result;
            alreadyCastLeft = true;
        }
    }
    
    if (!alreadyCastLeft && leftType != "bool") {
        string leftBool = createTemp();
        instructions.emplace_back(TACOp::CAST, leftBool, leftTemp, "bool");
        leftTemp = leftBool;
        cout << "DEBUG: Logical OR - casting left operand to bool" << endl;
    }
    
    string resultTemp = createTemp();
    string trueLabel = createLabel("logical_true");
    string endLabel = createLabel("logical_end");
    
    // If left is true (non-zero), jump to true label
    instructions.emplace_back(TACOp::IF_GOTO, trueLabel, leftTemp);
    
    // Evaluate right side (only if left is false)
    string rightTemp = generateExpression(node->children[1]);
      string rightType = getExpressionResultType(node->children[1]);
    
    // Cast right operand to bool if needed
    // OPTIMIZATION: Check if last instruction was already a bool cast of rightTemp
    bool alreadyCastRight = false;
    if (!instructions.empty()) {
        auto& lastInstr = instructions.back();
        if (lastInstr.opcode == TACOp::CAST && lastInstr.operand2 == "bool" && lastInstr.operand1 == rightTemp) {
            // Already casted to bool, use the result
            rightTemp = lastInstr.result;
            alreadyCastRight = true;
        }
    }
    
    if (!alreadyCastRight && rightType != "bool" ) {
        string rightBool = createTemp();
        instructions.emplace_back(TACOp::CAST, rightBool, rightTemp, "bool");
        rightTemp = rightBool;
        cout << "DEBUG: Logical OR - casting right operand to bool" << endl;
    }
    
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
    
    Node* leftNode = node->children[0];
    Node* rightNode = node->children[1];
    
    string leftTemp = generateExpression(leftNode);
    string rightTemp = generateExpression(rightNode);
     
    // ✅ ADD THIS SECTION - Validate bitwise/modulo operations
    if (op == TACOp::MOD || op == TACOp::BIT_AND || op == TACOp::BIT_OR || 
        op == TACOp::BIT_XOR || op == TACOp::SHL || op == TACOp::SHR) {
        
        string leftType = getExpressionResultType(leftNode);
        string rightType = getExpressionResultType(rightNode);
        
        if (leftType == "float" || leftType == "double" ||
            rightType == "float" || rightType == "double") {
            cout << "ERROR: Bitwise/modulo operation on floating-point type" << endl;
            return "";
        }
    }
    
    // ✅ ADD THIS SECTION - Handle comparisons specially
    if (op == TACOp::EQ || op == TACOp::NE || op == TACOp::LT || 
        op == TACOp::LE || op == TACOp::GT || op == TACOp::GE) {
        
        string leftType = getExpressionResultType(leftNode);
        string rightType = getExpressionResultType(rightNode);
        
        // Determine common comparison type
        string compareType = "int";
        if (leftType == "double" || rightType == "double") {
            compareType = "double";
        } else if (leftType == "float" || rightType == "float") {
            compareType = "float";
        } else if (leftType == "long" || rightType == "long") {
            compareType = "long";
        }
        
        // Convert both operands to comparison type
        if (normalizeTypeName(leftType) != compareType) {
            string convertedTemp = createTemp();
            instructions.emplace_back(TACOp::CAST, convertedTemp, leftTemp, compareType);
            leftTemp = convertedTemp;
            cout << "DEBUG: Comparison - converting left: " << leftType 
                 << " -> " << compareType << endl;
        }
        
        if (normalizeTypeName(rightType) != compareType) {
            string convertedTemp = createTemp();
            instructions.emplace_back(TACOp::CAST, convertedTemp, rightTemp, compareType);
            rightTemp = convertedTemp;
            cout << "DEBUG: Comparison - converting right: " << rightType 
                 << " -> " << compareType << endl;
        }
        
        // Perform comparison
        string resultTemp = createTemp();
        instructions.emplace_back(op, resultTemp, leftTemp, rightTemp);
        
        // Convert result to bool
        string boolResult = createTemp();
        instructions.emplace_back(TACOp::CAST, boolResult, resultTemp, "bool");
        return boolResult;
    }
    // Check if this is pointer arithmetic
    bool leftIsPointer = isPointerType(leftNode) || isTempPointer(leftTemp);
    bool rightIsPointer = isPointerType(rightNode) || isTempPointer(rightTemp);
    
    // Pointer arithmetic: ptr + n or n + ptr
    if ((op == TACOp::ADD || op == TACOp::SUB) && (leftIsPointer || rightIsPointer)) {
        if (leftIsPointer && rightIsPointer && op == TACOp::SUB) {
            // Pointer difference: (ptr1 - ptr2) / sizeof(*ptr)
            return generatePointerDifference(leftNode, rightNode, leftTemp, rightTemp);
        } else if (leftIsPointer && !rightIsPointer) {
            // ptr + n or ptr - n
            return generatePointerArithmetic(leftNode, leftTemp, rightTemp, op);
        } else if (!leftIsPointer && rightIsPointer && op == TACOp::ADD) {
            // n + ptr (commutative)
            return generatePointerArithmetic(rightNode, rightTemp, leftTemp, TACOp::ADD);
        }
    }
     // Apply integer promotion for bool and char operands
    leftTemp = applyIntegerPromotion(leftTemp, leftNode);
    rightTemp = applyIntegerPromotion(rightTemp, rightNode);
    
    // Apply usual arithmetic conversions for mixed-type operations
    string leftType = getExpressionResultType(leftNode);
    string rightType = getExpressionResultType(rightNode);
    
    // Normalize types
    leftType = normalizeTypeName(leftType);
    rightType = normalizeTypeName(rightType);
    
    // If types differ, convert to the "wider" type
    if (leftType != rightType) {
        // Determine the target type based on usual arithmetic conversions
        string targetType = "";
        
        // Priority: double > float > long > int
        if (leftType == "double" || rightType == "double") {
            targetType = "double";
        } else if (leftType == "float" || rightType == "float") {
            targetType = "float";
        } else if (leftType == "long" || rightType == "long") {
            targetType = "long";
        } else {
            targetType = "int";  // Both should be int after promotion
        }
        
        // Convert operands to target type if needed
        if (leftType != targetType && (leftType == "int" || leftType == "long" || leftType == "float" || leftType == "double")) {
            string convertedTemp = createTemp();
            instructions.emplace_back(TACOp::CAST, convertedTemp, leftTemp, targetType);
            leftTemp = convertedTemp;
            cout << "DEBUG: Arithmetic conversion: " << leftTemp << " (" << leftType 
                 << ") -> " << convertedTemp << " (" << targetType << ")" << endl;
        }
        
        if (rightType != targetType && (rightType == "int" || rightType == "long" || rightType == "float" || rightType == "double")) {
            string convertedTemp = createTemp();
            instructions.emplace_back(TACOp::CAST, convertedTemp, rightTemp, targetType);
            rightTemp = convertedTemp;
            cout << "DEBUG: Arithmetic conversion: " << rightTemp << " (" << rightType 
                 << ") -> " << convertedTemp << " (" << targetType << ")" << endl;
        }
    }
    
    // Regular arithmetic
    string resultTemp = createTemp();
    instructions.emplace_back(op, resultTemp, leftTemp, rightTemp);
       if (op == TACOp::EQ || op == TACOp::NE || op == TACOp::LT || 
        op == TACOp::LE || op == TACOp::GT || op == TACOp::GE) {
        string boolResult = createTemp();
        instructions.emplace_back(TACOp::CAST, boolResult, resultTemp, "bool");
        return boolResult;
    }
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
        // Check if this is a struct-to-struct assignment
        if (lhs->symbol && lhs->symbol->isStruct && rhs->name == "IDENTIFIER" && rhs->symbol && rhs->symbol->isStruct) {
            // Generate member-by-member copy for struct assignment
            string lhsName = getUniqueNameFor(lhs, lhs->lexeme);
            string rhsName = getUniqueNameFor(rhs, rhs->lexeme);
            
            // Get struct type to find members (strip "struct " prefix)
            string structTypeName = lhs->symbol->type;
            if (structTypeName.find("struct ") == 0) {
                structTypeName = structTypeName.substr(7); // Remove "struct " prefix
            }
            Symbol* structTypeSym = symbolTable.lookuph(structTypeName);
            if (structTypeSym && structTypeSym->isStruct) {
                // Copy each member in order
                for (const string& memberName : structTypeSym->structMemberOrder) {
                    auto it = structTypeSym->structMembers.find(memberName);
                    if (it != structTypeSym->structMembers.end()) {
                        // Get member offset
                        int offset = 0;
                        for (const string& m : structTypeSym->structMemberOrder) {
                            if (m == memberName) break;
                            // Simple offset calculation (4 bytes per member for now)
                            offset += 4;
                        }
                        
                        // Load from rhs.member: addr = &rhs + offset, value = *addr
                        string rhsAddrTemp = createTemp();
                        instructions.emplace_back(TACOp::ADDRESS, rhsAddrTemp, rhsName);
                        string offsetTemp = createTemp();
                        instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
                        string rhsMemberAddrTemp = createTemp();
                        instructions.emplace_back(TACOp::ADD, rhsMemberAddrTemp, rhsAddrTemp, offsetTemp);
                        string valueTemp = createTemp();
                        instructions.emplace_back(TACOp::LOAD, valueTemp, rhsMemberAddrTemp);
                        
                        // Store to lhs.member: addr = &lhs + offset, *addr = value
                        string lhsAddrTemp = createTemp();
                        instructions.emplace_back(TACOp::ADDRESS, lhsAddrTemp, lhsName);
                        string lhsOffsetTemp = createTemp();
                        instructions.emplace_back(TACOp::CONST, lhsOffsetTemp, to_string(offset));
                        string lhsMemberAddrTemp = createTemp();
                        instructions.emplace_back(TACOp::ADD, lhsMemberAddrTemp, lhsAddrTemp, lhsOffsetTemp);
                        instructions.emplace_back(TACOp::STORE, lhsMemberAddrTemp, valueTemp);
                    }
                }
                return lhsName;
            }
        }
        
        string rhsTemp = generateExpression(rhs);
        string lhsName = getUniqueNameFor(lhs, lhs->lexeme);
        
        // Track pointer type propagation for LHS variable
        if (lhs->symbol && lhs->symbol->pointerDepth > 0) {
            tempPointerDepth[lhsName] = lhs->symbol->pointerDepth;
        }
        // Also propagate if RHS is a temp pointer
        if (isTempPointer(rhsTemp)) {
            tempPointerDepth[lhsName] = tempPointerDepth[rhsTemp];
        }
        
        // If LHS is a reference, store through its address
        if (lhs->symbol && lhs->symbol->isReference) {
            instructions.emplace_back(TACOp::STORE, lhsName, rhsTemp);
            return lhsName;
        }
        string finalValue = handleImplicitConversion(rhsTemp, rhs, lhs);
        
        // Always use the temp from generateExpression for consistency
        instructions.emplace_back(TACOp::ASSIGN, lhsName, finalValue);
        return lhsName;
    }
    
    return "";
}

// Function call generation
string IRGenerator::generateFunctionCall(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* funcNode = node->children[0];
    
    // Unwrap EXPR_LIST if present
    if (funcNode->name == "EXPR_LIST" && !funcNode->children.empty()) {
        funcNode = funcNode->children[0];
    }
    
    string callOperand;     // can be function name or temp/unique name holding address
    vector<bool> paramIsRef; // per-arg reference flags
    string debugName = "<expr>";
    bool isIndirectCall = false;  // Track if this is an indirect call
    
    // Determine callee info
    if (funcNode->name == "IDENTIFIER") {
        debugName = funcNode->lexeme;
        
        // Handle varargs helpers
        if (debugName == "va_start") {
            // va_start(args, last_named_param)
            // Initialize the va_list variable to the index of the first variadic argument
            if (node->children.size() > 1) {
                Node* argList = node->children[1];
                if (argList && argList->children.size() >= 1) {
                    Node* vaListArg = argList->children[0];
                    if (vaListArg && vaListArg->name == "IDENTIFIER") {
                        string vaListName = getUniqueNameFor(vaListArg, vaListArg->lexeme);
                        
                        // Find the function symbol to count named parameters
                        Symbol* funcSym = symbolTable.lookuph(currentFunction);
                        int namedParamCount = funcSym ? funcSym->paramTypes.size() : 0;
                        
                        // Emit comment for documentation
                        instructions.emplace_back(TACOp::COMMENT, "va_start(" + vaListName + ")");
                        
                        // Generate: vaListName = namedParamCount (starting index)
                        string indexTemp = createTemp();
                        instructions.emplace_back(TACOp::CONST, indexTemp, to_string(namedParamCount));
                        instructions.emplace_back(TACOp::ASSIGN, vaListName, indexTemp);
                        
                        cout << "DEBUG: va_start(" << vaListName << ") initialized to " 
                             << namedParamCount << endl;
                    }
                }
            }
            return createTemp(); // Return a temp to satisfy expression contexts
        }
        
        if (debugName == "va_end") {
            // va_end - emit a comment marker for documentation
            if (node->children.size() > 1) {
                Node* argList = node->children[1];
                if (argList && argList->children.size() >= 1) {
                    Node* vaListArg = argList->children[0];
                    if (vaListArg && vaListArg->name == "IDENTIFIER") {
                        string vaListName = getUniqueNameFor(vaListArg, vaListArg->lexeme);
                        instructions.emplace_back(TACOp::COMMENT, "va_end(" + vaListName + ")");
                        cout << "DEBUG: va_end(" << vaListName << ") emitted comment" << endl;
                    }
                }
            }
            return createTemp(); // Return a temp to satisfy expression contexts
        }
        
        if (debugName == "va_copy") {
            // va_copy - not fully implemented but emit comment
            instructions.emplace_back(TACOp::COMMENT, "va_copy(...)");
            return createTemp();
        }
        
        Symbol* calleeSym = symbolTable.lookuph(funcNode->lexeme);  // Use lookuph to search scope history
        if (!calleeSym) return "";
        if (calleeSym->isFunction) {
            callOperand = funcNode->lexeme; // direct call by name
            paramIsRef = calleeSym->paramIsReference;
            isIndirectCall = false;
        } else if (calleeSym->isFunctionPointer) {
            // Indirect call via function pointer stored in a variable
            callOperand = getUniqueNameFor(funcNode, funcNode->lexeme);
            paramIsRef = calleeSym->funcPtrParamIsReference;
            isIndirectCall = true;
        } else {
            return "";
        }
    } else if (funcNode->name == "UNARY_OP" && funcNode->children.size() == 2 && funcNode->children[0]->name == "*") {
        // (*fp)(...)
        isIndirectCall = true;
        Node* inner = funcNode->children[1];
        if (inner && inner->name == "IDENTIFIER") {
            debugName = inner->lexeme;
            Symbol* calleeSym = symbolTable.lookuph(inner->lexeme);  // Use lookuph to search scope history
            if (!calleeSym || !calleeSym->isFunctionPointer) return "";
            callOperand = getUniqueNameFor(inner, inner->lexeme);
            paramIsRef = calleeSym->funcPtrParamIsReference;
        } else {
            // Fallback: evaluate expression to a temp as callee address (limited support)
            callOperand = generateExpression(funcNode);
        }
    } else if (funcNode->name == "ARRAY_ACCESS") {
        // Function pointer array element: operations[0](...)
        // The array element holds a function pointer, so we need to dereference it
        debugName = "<array element>";
        isIndirectCall = true;
        
        // Generate array access to get the function pointer
        callOperand = generateArrayAccess(funcNode);
        
        // Try to get parameter info from the array symbol if available
        if (funcNode->children.size() > 0 && funcNode->children[0]->name == "IDENTIFIER") {
            Symbol* arraySym = symbolTable.lookuph(funcNode->children[0]->lexeme);
            if (arraySym && arraySym->isFunctionPointer) {
                paramIsRef = arraySym->funcPtrParamIsReference;
            }
        }
    } else if (funcNode->name == "MEMBER_ACCESS" || funcNode->name == "DIRECT_MEMBER" || funcNode->name == "DOT" ||
               funcNode->name == "PTR_MEMBER_ACCESS" || funcNode->name == "ARROW") {
        // Struct member function pointer: obj.func_ptr(...) or ptr->func_ptr(...)
        isIndirectCall = true;
        debugName = "<member function pointer>";
        
        // Generate member access to get the function pointer
        bool isArrow = (funcNode->name == "PTR_MEMBER_ACCESS" || funcNode->name == "ARROW");
        callOperand = generateMemberAccess(funcNode, isArrow);
        
        // Try to get parameter info from member if available
        // Note: This would require struct member metadata, which may not be fully implemented
    } else {
        // Unsupported callee
        return "";
    }
    
    // Track function calls (only for named functions)
    functionCallCounts[debugName]++;
    if (debugName == currentFunction) {
        cout << "DEBUG: Recursive call detected in " << currentFunction << endl;
    }
    
    // ✅ FIX: Evaluate ALL arguments FIRST in left-to-right order
    // This ensures side effects happen in correct sequence
    vector<string> evaluatedArgs;
    
    // Track actual parameter count (may be > argList size for structs passed by value)
    int actualParamCount = 0;
    
    if (node->children.size() > 1) {
        Node* argList = node->children[1];
        
        cout << "DEBUG: Evaluating " << argList->children.size() 
             << " arguments for " << debugName << " in left-to-right order" << endl;
        
        // Step 1+2 combined: For each argument, either compute address (for ref) or value
for (size_t i = 0; i < argList->children.size(); i++) {
    Node* arg = argList->children[i];
    bool passByRef = (i < paramIsRef.size() && paramIsRef[i]);
    string toPass;
    if (passByRef) {
        toPass = generateLValueAddress(arg);
    } else {
        string argTemp = generateExpression(arg);
        
        // Check if this argument is a struct being passed by value
        bool isStructArg = false;
        Symbol* argSym = nullptr;
        string structTypeName = "";
        
        if (arg->name == "IDENTIFIER" && arg->symbol) {
            argSym = arg->symbol;
            
            // Check if it's directly a struct
            if (argSym->isStruct) {
                isStructArg = true;
                structTypeName = argSym->type;
            } 
            // Check if it's a typedef to a struct
            else {
                Symbol* typedefSym = symbolTable.lookuph(argSym->type);
                if (typedefSym && typedefSym->isTypedef && typedefSym->isStruct) {
                    isStructArg = true;
                    structTypeName = typedefSym->type;
                }
            }
        }
        
        if (isStructArg && argSym) {
            // Struct passed by value - pass each member as a separate parameter
            string structName = argTemp;
            
            // Get struct type definition
            if (structTypeName.find("struct ") == 0) {
                structTypeName = structTypeName.substr(7);
            }
            Symbol* structTypeSym = symbolTable.lookuph(structTypeName);
            
            if (structTypeSym && structTypeSym->isStruct) {
                // Pass each member as a separate parameter
                for (const string& memberName : structTypeSym->structMemberOrder) {
                    int offset = 0;
                    for (const string& m : structTypeSym->structMemberOrder) {
                        if (m == memberName) break;
                        offset += 4;  // 4 bytes per member
                    }
                    
                    // Load member: addr = &struct + offset, value = *addr
                    string addrTemp = createTemp();
                    instructions.emplace_back(TACOp::ADDRESS, addrTemp, structName);
                    string offsetTemp = createTemp();
                    instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
                    string memberAddrTemp = createTemp();
                    instructions.emplace_back(TACOp::ADD, memberAddrTemp, addrTemp, offsetTemp);
                    string memberValueTemp = createTemp();
                    instructions.emplace_back(TACOp::LOAD, memberValueTemp, memberAddrTemp);
                    
                    // Emit param for this member
                    instructions.emplace_back(TACOp::PARAM, "", memberValueTemp);
                    actualParamCount++;
                }
                continue;  // Skip the normal param emission below
            }
        }
        
        // ✅ NEW: Convert argument to parameter type if function info available
        if (funcNode->name == "IDENTIFIER" && funcNode->symbol && 
            funcNode->symbol->isFunction && i < funcNode->symbol->paramTypes.size()) {
            
            string expectedType = normalizeTypeName(funcNode->symbol->paramTypes[i]);
            string actualType = normalizeTypeName(getExpressionResultType(arg));
            
            if (expectedType != actualType) {
                string convertedTemp = createTemp();
                instructions.emplace_back(TACOp::CAST, convertedTemp, argTemp, expectedType);
                argTemp = convertedTemp;
                cout << "DEBUG: Converting argument " << i << ": " 
                     << actualType << " -> " << expectedType << endl;
            }
        }
        
        if (!argTemp.empty()) {
            bool isTemp = (argTemp[0] == 't' && (argTemp.size() == 1 || isdigit(argTemp[1])));
            bool isConst = (argTemp[0] == '"' || isdigit(argTemp[0]) || 
                           (argTemp[0] == '-' && argTemp.size()>1 && isdigit(argTemp[1])));
            if (!isTemp && !isConst) {
                string preservedTemp = createTemp();
                instructions.emplace_back(TACOp::ASSIGN, preservedTemp, argTemp);
                toPass = preservedTemp;
            } else {
                toPass = argTemp;
            }
        }
    }
    cout << "DEBUG: Emitting param " << i << ": " << toPass << endl;
    instructions.emplace_back(TACOp::PARAM, "", toPass);
    actualParamCount++;
}
    }
    
    // Step 3: Emit the CALL instruction (direct or indirect)
    string resultTemp = createTemp();
   
string callTarget = isIndirectCall ? ("*" + callOperand) : callOperand;

// ✅ Use actual parameter count (accounts for struct members passed individually)
int paramCount = actualParamCount;

// ✅ Emit CALL with param count
instructions.emplace_back(TACOp::CALL, resultTemp, callTarget, "", paramCount);

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
                    
                    // Case 1: Array with initializer list (handle both direct ARRAY and DECLARATOR-wrapped ARRAY)
                    Node* arrayNodeCandidate = nullptr;
                    Node* idNode = nullptr;
                    std::function<Node*(Node*)> findArrayInDeclarator;
                    findArrayInDeclarator = [&](Node* n) -> Node* {
                        if (!n) return nullptr;
                        if (n->name == "ARRAY") return n;
                        for (auto c : n->children) {
                            Node* r = findArrayInDeclarator(c);
                            if (r) return r;
                        }
                        return nullptr;
                    };
                    if (varNode->name == "ARRAY") {
                        arrayNodeCandidate = varNode;
                        idNode = findIdentifierInArray(varNode);
                    } else if (varNode->name == "DECLARATOR") {
                        arrayNodeCandidate = findArrayInDeclarator(varNode);
                        if (arrayNodeCandidate) {
                            idNode = findIdentifierInArray(arrayNodeCandidate);
                        }
                    } else if (varNode->name == "IDENTIFIER" && varNode->symbol && varNode->symbol->isArray) {
                        // Typedef array case: IntArray5 arr = {...}
                        // The varNode is just an IDENTIFIER, but its symbol indicates it's an array
                        idNode = varNode;
                        // No explicit arrayNodeCandidate, but we'll proceed with array initialization
                        arrayNodeCandidate = varNode; // Use the identifier itself as a marker
                    }
                    if (arrayNodeCandidate && (initNode->name == "INIT_LIST" || initNode->name == "STRING_LITERAL")) {
                        if (idNode) {
                            string arrayName = getUniqueNameFor(idNode, idNode->lexeme);
                            int declaredSize = -1; // legacy fallback for 1D explicit size only
                            if (arrayNodeCandidate->children.size() > 1 && arrayNodeCandidate->children[1]->name == "INTEGER_CONSTANT") {
                                declaredSize = stoi(arrayNodeCandidate->children[1]->lexeme);
                            }
                            cout << "DEBUG: Array declaration with init: " << arrayName << endl;
                            // Guard local static array initialization to run once
                            string endLabel_guard;
                            string flagName_guard;
                            bool guarded_static_array = false;
                            if (isStatic && !currentFunction.empty()) {
                                guarded_static_array = true;
                                endLabel_guard = createLabel("static_init_end");
                                flagName_guard = arrayName + "__inited";
                                instructions.emplace_back(TACOp::IF_GOTO, endLabel_guard, flagName_guard);
                            }
                            
                            // Compute base address & element size
                            string baseAddr = createTemp();
                            instructions.emplace_back(TACOp::ADDRESS, baseAddr, arrayName);
                            int elemSize = 4;
                            if (idNode->symbol) {
                                // If the array's element type is a pointer (e.g., char* arr[]),
                                // each element is pointer-sized.
                                if (idNode->symbol->pointerDepth > 0) {
                                    elemSize = 4;
                                } else {
                                    elemSize = getBaseTypeSize(idNode->symbol->type);
                                }
                            }
                            string elemSizeTemp = createTemp();
                            instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
                            
                            // Prepare dimension info from symbol
                            vector<int> dims;
                            if (idNode->symbol && idNode->symbol->isArray) {
                                dims = idNode->symbol->arrayDimensions;
                            }
                            // Compute strides in elements (not bytes)
                            vector<int> strideElems;
                            if (!dims.empty() && dims[0] > 0) {
                                strideElems.resize(dims.size(), 1);
                                for (int di = static_cast<int>(dims.size()) - 2; di >= 0; --di) {
                                    int next = (dims[di + 1] > 0 ? dims[di + 1] : 1);
                                    strideElems[di] = strideElems[di + 1] * next;
                                }
                            }

                            // Helpers to emit a store at a linear element index
                            auto emitStoreAtIndex = [&](int elemIndex, const string& valueTemp) {
                                string idxTemp = createTemp();
                                string offsetTemp = createTemp();
                                string elemAddr = createTemp();
                                instructions.emplace_back(TACOp::CONST, idxTemp, to_string(elemIndex));
                                instructions.emplace_back(TACOp::MUL, offsetTemp, idxTemp, elemSizeTemp);
                                instructions.emplace_back(TACOp::ADD, elemAddr, baseAddr, offsetTemp);
                                instructions.emplace_back(TACOp::STORE, elemAddr, valueTemp);
                            };

                            auto emitZeroRange = [&](int startElem, int countElems) {
                                if (countElems <= 0) return;
                                string zeroTemp = createTemp();
                                instructions.emplace_back(TACOp::CONST, zeroTemp, "0");
                                for (int k = 0; k < countElems; ++k) {
                                    int idx = startElem + k;
                                    string idxTemp = createTemp();
                                    string offsetTemp = createTemp();
                                    string elemAddr = createTemp();
                                    instructions.emplace_back(TACOp::CONST, idxTemp, to_string(idx));
                                    instructions.emplace_back(TACOp::MUL, offsetTemp, idxTemp, elemSizeTemp);
                                    instructions.emplace_back(TACOp::ADD, elemAddr, baseAddr, offsetTemp);
                                    instructions.emplace_back(TACOp::STORE, elemAddr, zeroTemp);
                                }
                            };

                            // Decide if we can honor nested braces exactly
                            function<bool(Node*, int)> needsFlatten = [&](Node* n, int dimIdx) -> bool {
                                if (!n) return true;
                                if (n->name != "INIT_LIST") return true; // scalar at non-innermost
                                if (dims.empty()) return true;
                                if (dimIdx >= static_cast<int>(dims.size()) - 1) {
                                    // innermost: scalars acceptable
                                    return false;
                                }
                                for (auto c : n->children) {
                                    if (c->name == "INIT_LIST") {
                                        if (needsFlatten(c, dimIdx + 1)) return true;
                                        continue;
                                    }
                                    // Allow string literals to initialize a whole row at the next (last) dimension for char arrays
                                    if (c->name == "STRING_LITERAL" && elemSize == 1 && dimIdx == static_cast<int>(dims.size()) - 2) {
                                        continue;
                                    }
                                    return true; // other scalars before innermost → flatten
                                }
                                return false;
                            };

                            // Special-case: string literal initializer for char arrays
                            if (initNode->name == "STRING_LITERAL") {
                                // Only handle 1-D char arrays here
                                int totalElements = 0;
                                if (!dims.empty()) {
                                    totalElements = dims[0] > 0 ? dims[0] : 0;
                                } else if (declaredSize > 0) {
                                    totalElements = declaredSize;
                                }
                                auto decodedVec = decodeCString(initNode->lexeme);
                                int writeCount = static_cast<int>(decodedVec.size());
                                // Emit characters up to array size - 1 (reserve for null), if size known
                                int limit = totalElements > 0 ? max(0, totalElements - 1) : writeCount;
                                int actualChars = (totalElements > 0) ? min(writeCount, limit) : writeCount;
                                for (int i = 0; i < actualChars; ++i) {
                                    string idxTemp = createTemp();
                                    string offTemp = createTemp();
                                    string addrTemp = createTemp();
                                    string valTemp = createTemp();
                                    instructions.emplace_back(TACOp::CONST, idxTemp, to_string(i));
                                    instructions.emplace_back(TACOp::MUL, offTemp, idxTemp, elemSizeTemp);
                                    instructions.emplace_back(TACOp::ADD, addrTemp, baseAddr, offTemp);
                                    instructions.emplace_back(TACOp::CONST, valTemp, to_string(static_cast<unsigned char>(decodedVec[i])));
                                    instructions.emplace_back(TACOp::STORE, addrTemp, valTemp);
                                }
                                // Null-terminate if space allows
                                if (totalElements > 0 && actualChars < totalElements) {
                                    string idxTemp = createTemp();
                                    string offTemp = createTemp();
                                    string addrTemp = createTemp();
                                    string zeroTemp = createTemp();
                                    instructions.emplace_back(TACOp::CONST, idxTemp, to_string(actualChars));
                                    instructions.emplace_back(TACOp::MUL, offTemp, idxTemp, elemSizeTemp);
                                    instructions.emplace_back(TACOp::ADD, addrTemp, baseAddr, offTemp);
                                    instructions.emplace_back(TACOp::CONST, zeroTemp, "0");
                                    instructions.emplace_back(TACOp::STORE, addrTemp, zeroTemp);
                                }
                                // Zero-fill remaining elements if array larger than string+null
                                if (totalElements > 0) {
                                    int start = min(totalElements, actualChars + 1);
                                    int count = totalElements - start;
                                    if (count > 0) {
                                        string zeroTemp = createTemp();
                                        instructions.emplace_back(TACOp::CONST, zeroTemp, "0");
                                        for (int k = 0; k < count; ++k) {
                                            int idx = start + k;
                                            string idxTemp = createTemp();
                                            string offTemp = createTemp();
                                            string addrTemp = createTemp();
                                            instructions.emplace_back(TACOp::CONST, idxTemp, to_string(idx));
                                            instructions.emplace_back(TACOp::MUL, offTemp, idxTemp, elemSizeTemp);
                                            instructions.emplace_back(TACOp::ADD, addrTemp, baseAddr, offTemp);
                                            instructions.emplace_back(TACOp::STORE, addrTemp, zeroTemp);
                                        }
                                    }
                                }
                                // Done handling string literal init
                                if (guarded_static_array) {
                                    string oneTempG = createTemp();
                                    instructions.emplace_back(TACOp::CONST, oneTempG, "1");
                                    instructions.emplace_back(TACOp::ASSIGN, flagName_guard, oneTempG);
                                    instructions.emplace_back(TACOp::LABEL, endLabel_guard);
                                }
                                continue;
                            }

                            bool useFlatten = dims.empty() || needsFlatten(initNode, 0);
                            cout << "DEBUG: useFlatten=" << useFlatten << ", dims.size()=" << dims.size() << endl;
                            if (dims.size() >= 2) {
                                cout << "DEBUG: Multidimensional array: dims=[";
                                for (size_t i = 0; i < dims.size(); ++i) {
                                    cout << dims[i];
                                    if (i < dims.size() - 1) cout << "][";
                                }
                                cout << "]" << endl;
                            }
                            if (useFlatten) {
                                // Flatten nested initializer lists into a linear sequence (row-major)
                                vector<Node*> flatValues;
                                function<void(Node*)> flatten = [&](Node* n){
                                    if (!n) return;
                                    if (n->name == "INIT_LIST") {
                                        for (auto c : n->children) flatten(c);
                                    } else {
                                        flatValues.push_back(n);
                                    }
                                };
                                flatten(initNode);

                                // Determine total element count
                                int totalElements = -1;
                                if (!dims.empty()) {
                                    totalElements = 1;
                                    for (int dim : dims) {
                                        if (dim <= 0) { totalElements = -1; break; }
                                        totalElements *= dim;
                                    }
                                }
                                if (totalElements < 0) {
                                    // Fallbacks: use declared first-dimension size if present (1D), else initializer count
                                    if (declaredSize > 0) {
                                        totalElements = declaredSize;
                                    } else {
                                        totalElements = static_cast<int>(flatValues.size());
                                    }
                                }

                                // Emit stores for provided initializers (bounded by totalElements)
                                int provided = static_cast<int>(flatValues.size());
                                int emitCount = min(provided, totalElements);
                            
                                for (int i = 0; i < emitCount; ++i) {
    string valueTemp = generateExpression(flatValues[i]);
    
    // ✅ NEW: Convert initializer to array element type
    if (idNode && idNode->symbol) {
        string elemType = idNode->symbol->type;
        if (idNode->symbol->pointerDepth > 0) {
            elemType = "pointer";
        }
        elemType = normalizeTypeName(elemType);
        
        string initType = normalizeTypeName(getExpressionResultType(flatValues[i]));
        
        if (elemType != initType && elemType != "pointer") {
            string convertedTemp = createTemp();
            instructions.emplace_back(TACOp::CAST, convertedTemp, valueTemp, elemType);
            valueTemp = convertedTemp;
            cout << "DEBUG: Converting array initializer[" << i << "]: " 
                 << initType << " -> " << elemType << endl;
        }
    }
    
    emitStoreAtIndex(i, valueTemp);
}
                                // Zero-fill remaining elements
                                emitZeroRange(emitCount, totalElements - emitCount);
                            } else {
                                // Honor nested braces per-dimension with zero-fill in each sub-aggregate

                                function<void(Node*, int, int)> emitNested;
                                emitNested = [&](Node* list, int dimIdx, int baseElem) {
                                    int dimSize = dims[dimIdx];
                                    int stride = (dimIdx < static_cast<int>(strideElems.size())) ? strideElems[dimIdx] : 1;
                                    int processed = 0;
                                    for (int i = 0; i < (int)list->children.size() && processed < dimSize; ++i, ++processed) {
                                        Node* child = list->children[i];
                                        if (dimIdx == (int)dims.size() - 1) {
                                            // innermost
                                            if (child->name == "INIT_LIST") {
                                                // Flatten one level for innermost braces
                                                for (auto sc : child->children) {
                                                    if (processed >= dimSize) break;
                                                    string valueTemp = generateExpression(sc);
                                                    emitStoreAtIndex(baseElem + processed, valueTemp);
                                                    ++processed;
                                                }
                                                // processed already advanced inside loop, decrement once for for-loop ++processed
                                                --processed;
                                            } else {
                                                string valueTemp = generateExpression(child);
                                                emitStoreAtIndex(baseElem + processed, valueTemp);
                                            }
                                        } else {
                                            // not innermost: must be INIT_LIST per useFlatten=false
                                            if (child->name == "INIT_LIST") {
                                                emitNested(child, dimIdx + 1, baseElem + processed * stride);
                                            } else if (child->name == "STRING_LITERAL" && elemSize == 1 && dimIdx == (int)dims.size() - 2) {
                                                // Handle string literal initializing a whole char row
                                                int rowLen = dims[dimIdx + 1];
                                                auto decodedVec = decodeCString(child->lexeme);
                                                int startElem = baseElem + processed * stride; // row start
                                                int actualChars = rowLen > 0 ? std::min((int)decodedVec.size(), std::max(0, rowLen - 1)) : (int)decodedVec.size();
                                                for (int j = 0; j < actualChars; ++j) {
                                                    string chTemp = createTemp();
                                                    instructions.emplace_back(TACOp::CONST, chTemp, to_string((unsigned char)decodedVec[j]));
                                                    emitStoreAtIndex(startElem + j, chTemp);
                                                }
                                                // Null terminate if room
                                                if (rowLen > 0 && actualChars < rowLen) {
                                                    string zeroTemp = createTemp();
                                                    instructions.emplace_back(TACOp::CONST, zeroTemp, "0");
                                                    emitStoreAtIndex(startElem + actualChars, zeroTemp);
                                                }
                                                // Zero-fill remainder of the row
                                                if (rowLen > 0) {
                                                    int filled = std::min(rowLen, actualChars + 1);
                                                    int rem = rowLen - filled;
                                                    emitZeroRange(startElem + filled, rem);
                                                }
                                            }
                                        }
                                    }
                                    // Zero-fill remainder of this subarray
                                    int filledElems = (dimIdx == (int)dims.size() - 1) ? processed : processed * stride;
                                    int totalSubElems = (dimIdx == (int)dims.size() - 1) ? dimSize : dimSize * stride;
                                    emitZeroRange(baseElem + filledElems, totalSubElems - filledElems);
                                };

                                emitNested(initNode, 0, 0);
                            }
                            if (guarded_static_array) {
                                string oneTempG = createTemp();
                                instructions.emplace_back(TACOp::CONST, oneTempG, "1");
                                instructions.emplace_back(TACOp::ASSIGN, flagName_guard, oneTempG);
                                instructions.emplace_back(TACOp::LABEL, endLabel_guard);
                            }
                            continue;
                        }
                    }
                    // Case 1b: Struct/union with initializer list: emit per-member stores
                    if ((varNode->name == "IDENTIFIER" || varNode->name == "DECLARATOR") && initNode->name == "INIT_LIST") {
                        // Resolve variable name
                        string varName = "";
                        Node* idInDecl = nullptr;
                        if (varNode->name == "IDENTIFIER") {
                            idInDecl = varNode;
                        } else {
                            // Find IDENTIFIER inside declarator
                            std::function<Node*(Node*)> findId = [&](Node* n) -> Node*{
                                if (!n) return nullptr;
                                if (n->name == "IDENTIFIER") return n;
                                for (auto c : n->children) { auto r = findId(c); if (r) return r; }
                                return nullptr;
                            };
                            idInDecl = findId(varNode);
                        }
                        varName = idInDecl ? getUniqueNameFor(idInDecl, idInDecl->lexeme) : getDeclaratorName(varNode);
                        if (!varName.empty()) {
                            // Look up variable symbol and struct type symbol using source name if possible
                            // Attempt to resolve by the raw lexeme from idInDecl when available
                            Symbol* varSym = nullptr;
                            if (idInDecl && !idInDecl->lexeme.empty()) {
                                varSym = symbolTable.lookuph(idInDecl->lexeme);
                            }
                            if (!varSym) {
                                // Fallback: try unique name (may not be found in history)
                                varSym = symbolTable.lookuph(varName);
                            }
                            if (varSym) {
                                string t = varSym->type; // e.g., "struct P"
                                bool isStructType = false;
                                string structTypeName = t;
                                if (t.rfind("struct ", 0) == 0) { isStructType = true; structTypeName = t.substr(7); }
                                else if (t.rfind("union ", 0) == 0) { isStructType = true; structTypeName = t.substr(6); }
                                if (isStructType) {
                                    Symbol* structSym = symbolTable.lookuph(structTypeName);
                                    if (structSym && (structSym->isStruct || structSym->isUnion)) {
                                        // Base address of the aggregate
                                        string baseAddr = createTemp();
                                        instructions.emplace_back(TACOp::ADDRESS, baseAddr, varName);
                                        // Guard local static struct initialization
                                        string endLabel_guard2;
                                        string flagName_guard2;
                                        bool guarded_static_struct = false;
                                        if (isStatic && !currentFunction.empty()) {
                                            guarded_static_struct = true;
                                            endLabel_guard2 = createLabel("static_init_end");
                                            flagName_guard2 = varName + "__inited";
                                            instructions.emplace_back(TACOp::IF_GOTO, endLabel_guard2, flagName_guard2);
                                        }
                                        // Iterate members in DECLARATION order using structMemberOrder
                                        size_t idx = 0;
                                        for (const auto& memberName : structSym->structMemberOrder) {
                                            // Look up member type from structMembers map
                                            auto it = structSym->structMembers.find(memberName);
                                            if (it == structSym->structMembers.end()) continue; // Safety check
                                            string memberType = it->second;  // ✅ Get expected type
   
                                            int offset = getMemberOffset((varNode->name == "IDENTIFIER") ? varNode : nullptr, memberName);
                                            string memberAddr = createTemp();
                                            string offsetTemp = createTemp();
                                            instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
                                            instructions.emplace_back(TACOp::ADD, memberAddr, baseAddr, offsetTemp);
                                            
                                            
                        string valueTemp;
    if (idx < initNode->children.size()) {
        Node* initChild = initNode->children[idx];
        
        // Check if this is a nested INIT_LIST for a struct/union member
        if (initChild->name == "INIT_LIST") {
            // Check if member type is struct or union
            string normalizedMemberType = normalizeTypeName(memberType);
            bool isNestedStruct = (normalizedMemberType.rfind("struct ", 0) == 0 || 
                                  normalizedMemberType.rfind("union ", 0) == 0);
            
            if (isNestedStruct) {
                // Handle nested struct initialization
                string structTypeName = normalizedMemberType;
                if (structTypeName.rfind("struct ", 0) == 0) structTypeName = structTypeName.substr(7);
                else if (structTypeName.rfind("union ", 0) == 0) structTypeName = structTypeName.substr(6);
                
                Symbol* nestedStructSym = symbolTable.lookuph(structTypeName);
                if (nestedStructSym && (nestedStructSym->isStruct || nestedStructSym->isUnion)) {
                    // Initialize nested struct members recursively
                    size_t nestedIdx = 0;
                    
                    // Calculate base offset of this struct member
                    int baseNestedOffset = getMemberOffset((varNode->name == "IDENTIFIER") ? varNode : nullptr, memberName);
                    
                    // Calculate offset of each member within the nested struct
                    int nestedMemberOffset = 0;
                    for (const auto& nestedMemberName : nestedStructSym->structMemberOrder) {
                        auto nit = nestedStructSym->structMembers.find(nestedMemberName);
                        if (nit == nestedStructSym->structMembers.end()) continue;
                        
                        string nestedMemType = nit->second;
                        int memberSize = getBaseTypeSize(nestedMemType);
                        int memberAlign = getMemberAlignment(nestedMemType);
                        
                        // Add padding for alignment
                        if (nestedMemberOffset % memberAlign != 0) {
                            nestedMemberOffset += memberAlign - (nestedMemberOffset % memberAlign);
                        }
                        
                        int totalOffset = baseNestedOffset + nestedMemberOffset;
                        
                        string nestedMemberAddr = createTemp();
                        string nestedOffsetTemp = createTemp();
                        instructions.emplace_back(TACOp::CONST, nestedOffsetTemp, to_string(totalOffset));
                        instructions.emplace_back(TACOp::ADD, nestedMemberAddr, baseAddr, nestedOffsetTemp);
                        
                        string nestedValueTemp;
                        if (nestedIdx < initChild->children.size()) {
                            nestedValueTemp = generateExpression(initChild->children[nestedIdx]);
                        } else {
                            nestedValueTemp = createTemp();
                            instructions.emplace_back(TACOp::CONST, nestedValueTemp, "0");
                        }
                        
                        instructions.emplace_back(TACOp::STORE, nestedMemberAddr, nestedValueTemp);
                        nestedIdx++;
                        
                        // Move to next member offset
                        nestedMemberOffset += memberSize;
                    }
                    idx++;
                    continue; // Skip the normal STORE below
                }
            }
        }
        
        // Normal case: generate expression for non-struct initializer
        valueTemp = generateExpression(initChild);
        
        // ✅ NEW: Convert to member type
        string initType = normalizeTypeName(getExpressionResultType(initChild));
        memberType = normalizeTypeName(memberType);
        
        if (memberType != initType) {
            string convertedTemp = createTemp();
            instructions.emplace_back(TACOp::CAST, convertedTemp, valueTemp, memberType);
            valueTemp = convertedTemp;
            cout << "DEBUG: Converting struct member '" << memberName 
                 << "': " << initType << " -> " << memberType << endl;
        }
    } else {
        valueTemp = createTemp();
        instructions.emplace_back(TACOp::CONST, valueTemp, "0");
    }
    
                                        
                                            instructions.emplace_back(TACOp::STORE, memberAddr, valueTemp);
                                            idx++;
                                        }
                                        if (guarded_static_struct) {
                                            string oneTempG2 = createTemp();
                                            instructions.emplace_back(TACOp::CONST, oneTempG2, "1");
                                            instructions.emplace_back(TACOp::ASSIGN, flagName_guard2, oneTempG2);
                                            instructions.emplace_back(TACOp::LABEL, endLabel_guard2);
                                        }
                                        // Skip generic scalar assignment handling
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                    
                    // Case 2: Scalar or pointer variable with initializer
                    string varName = "";
                    Node* idInDecl = nullptr;
                    if (varNode->name == "IDENTIFIER") {
                        idInDecl = varNode;
                    } else if (varNode->name == "ARRAY") {
                        // Check if this is a pointer-to-array: int (*p)[N] or int (*pc)[M][N]
                        // Structure: ARRAY -> ... -> ARRAY -> DECLARATOR -> POINTER + IDENTIFIER
                        // Navigate through nested ARRAY nodes to find DECLARATOR with POINTER
                        std::function<bool(Node*)> hasPointerInDeclarator = [&](Node* n) -> bool {
                            if (!n) return false;
                            if (n->name == "DECLARATOR") {
                                for (auto c : n->children) {
                                    if (c->name == "POINTER") return true;
                                }
                                return false;
                            }
                            if (n->name == "ARRAY" && !n->children.empty()) {
                                return hasPointerInDeclarator(n->children[0]);
                            }
                            return false;
                        };
                        
                        if (hasPointerInDeclarator(varNode)) {
                            // This is a pointer-to-array variable, not an array
                            idInDecl = findIdentifierInArray(varNode);
                        }
                    } else if (varNode->name == "DECLARATOR" || varNode->name == "FUNCTION_DECL") {
                        // Find IDENTIFIER inside declarator or function pointer declarator
                        std::function<Node*(Node*)> findId = [&](Node* n) -> Node*{
                            if (!n) return nullptr;
                            if (n->name == "IDENTIFIER") return n;
                            for (auto c : n->children) { auto r = findId(c); if (r) return r; }
                            return nullptr;
                        };
                        idInDecl = findId(varNode);
                    }
                    if (idInDecl) varName = getUniqueNameFor(idInDecl, idInDecl->lexeme);
                    else if (varNode->name == "DECLARATOR") varName = getDeclaratorName(varNode);
                    
                    // Track static variables
                    if (isStatic && !varName.empty()) {
                        staticVariables.insert(varName);
                        cout << "DEBUG: Static variable: " << varName << endl;
                    }

                    // Generate initialization if present
                    if (!varName.empty() && initNode->name != "EMPTY") {
                        // Local static inside a function: initialize once with a guard
                        if (isStatic && !currentFunction.empty()) {
                            string endLabel = createLabel("static_init_end");
                            string flagName = varName + "__inited";
                            // If flag is true, skip initialization
                            instructions.emplace_back(TACOp::IF_GOTO, endLabel, flagName);

                            string initTemp = generateExpression(initNode);
                            if (!initTemp.empty()) {
                                instructions.emplace_back(TACOp::ASSIGN, varName, initTemp);
                                cout << "Generated (once) assignment: " << varName << " = " << initTemp << endl;
                            }
                            // Set the guard flag to 1 (OPTIMIZED: use CONST directly)
                            instructions.emplace_back(TACOp::CONST, flagName, "1");
                            // End label
                            instructions.emplace_back(TACOp::LABEL, endLabel);
                        } else {
                           // Check if we're initializing a pointer with an array subscript
                           // Example: int *ptr = array[i]
                           // We need the address, not the value
                           bool needsAddress = false;
                           if (idInDecl && idInDecl->symbol && idInDecl->symbol->pointerDepth > 0) {
                               // Initializing a pointer
                               if (initNode->name == "ARRAY_ACCESS") {
                                   needsAddress = true;
                               }
                           }
                           
                           string initTemp;
                           if (needsAddress) {
                               // Generate address calculation only (without final LOAD)
                               initTemp = generateArrayAddressOnly(initNode);
                           } else {
                               initTemp = generateExpression(initNode);
                           }
                           
                              if (!initTemp.empty()) {
                                  // Handle implicit type conversion
                                  string finalValue = handleImplicitConversion(initTemp, initNode, idInDecl);
                                  
                                  // OPTIMIZATION: Check if initTemp is just a constant assignment
                                  // If the last instruction was "initTemp = CONST value", use the const directly
                                  bool optimized = false;
                                  if (!instructions.empty() && finalValue.find("#") == string::npos) { // Only optimize temps, not variables
                                      auto& lastInstr = instructions.back();
                                      if (lastInstr.opcode == TACOp::CONST && lastInstr.result == finalValue) {
                                          // Remove the temporary assignment and use constant directly
                                          string constValue = lastInstr.operand1;
                                          instructions.pop_back(); // Remove temp = CONST instruction
                                          instructions.emplace_back(TACOp::CONST, varName, constValue);
                                          cout << "Optimized assignment: " << varName << " = CONST " << constValue << endl;
                                          optimized = true;
                                      }
                                  }
                                  
                                  if (!optimized) {
                                      instructions.emplace_back(TACOp::ASSIGN, varName, finalValue);
                                      cout << "Generated assignment: " << varName << " = " << finalValue << endl;
                                  }
                              }

                        }
                    } else if (!varName.empty()) {
                        // Handle uninitialized local statics: zero-initialize once
                        if (isStatic && !currentFunction.empty()) {
                            string endLabel = createLabel("static_init_end");
                            string flagName = varName + "__inited";
                            instructions.emplace_back(TACOp::IF_GOTO, endLabel, flagName);
                            // Assign 0 once (OPTIMIZED: use CONST directly)
                            instructions.emplace_back(TACOp::CONST, varName, "0");
                            // Mark initialized (OPTIMIZED: use CONST directly)
                            instructions.emplace_back(TACOp::CONST, flagName, "1");
                            instructions.emplace_back(TACOp::LABEL, endLabel);
                        } else {
                            cout << "DEBUG: Uninitialized variable " << varName << endl;
                        }
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
        if (child->name == "DECLARATOR" || child->name == "FUNCTION_DECL" || child->name == "IDENTIFIER") {
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
            // Extract return type from DECL_SPECIFIERS (same structure as SPEC_QUAL_LIST)
            currentFunctionReturnType = extractTypeFromSpecQualList(child);
            cout << "DEBUG: Function return type: " << currentFunctionReturnType << endl;
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
    functionLocalNames.clear();  // Clear function-local name map for new function
    string label = "func_" + funcName;
    instructions.emplace_back(TACOp::LABEL, label);
    
    cout << "Generating function: " << funcName << endl;
    
    // Count parameters first
    int paramCount = 0;
    if (paramList) {
        for (auto child : paramList->children) {
            if (child->name == "PARAM_LIST") {
                for (auto param : child->children) {
                    if (param->name == "PARAM_DECL") {
                        paramCount++;
                    }
                }
            } else if (child->name == "PARAM_DECL") {
                paramCount++;
            }
        }
    }
    
    // Add function begin marker with parameter count
    instructions.emplace_back(TACOp::FUNC_BEGIN, funcName, "", "", paramCount);
    
    if (paramList) {
        generateParameterHandling(paramList);
    }
    
    if (body) {
        generateStatement(body);
    }
    
    // Emit implicit return inside the function (before func_end) only if needed
    if (currentFunction != "main" && 
        (instructions.empty() || instructions.back().opcode != TACOp::RETURN)) {
        instructions.emplace_back(TACOp::RETURN);
    }

    // Add function end marker after body and any implicit return
    instructions.emplace_back(TACOp::FUNC_END, funcName);
    
    currentFunction = "";
    currentFunctionReturnType = "";
}

void IRGenerator::generateParameterHandling(Node* paramList) {
    if (!paramList) return;
    
    cout << "DEBUG: Generating parameter handling" << endl;
    
    int paramIndex = 0;
    
    // Handle nested structure: PARAM_TYPE_LIST -> PARAM_LIST -> PARAM_DECL
    for (auto child : paramList->children) {
        if (child->name == "PARAM_LIST") {
            // Found PARAM_LIST, now process its children (PARAM_DECL nodes)
            for (auto param : child->children) {
                if (param->name == "PARAM_DECL") {
                    string paramName = findParameterName(param);
                    // Find the identifier node for the parameter name (in DECLARATOR, not in type spec)
                    Node* idNode = nullptr;
                    for (auto paramChild : param->children) {
                        if (paramChild->name == "DECLARATOR") {
                            std::function<Node*(Node*)> findId = [&](Node* n) -> Node*{
                                if (!n) return nullptr;
                                if (n->name == "IDENTIFIER") return n;
                                for (auto c : n->children) { auto r = findId(c); if (r) return r; }
                                return nullptr;
                            };
                            idNode = findId(paramChild);
                            if (idNode) break;
                        }
                    }
                    // For struct parameters, we need to use the same SSA name that will be used in the function body
                    // The idNode approach doesn't work for struct params, so look up the symbol
                    Symbol* paramSym = symbolTable.lookuph(paramName);
                    string targetName = paramName;
                    if (paramSym) {
                        // Force check: if this symbol already has a unique name, use it
                        auto it = symbolUniqueNames.find(paramSym);
                        if (it != symbolUniqueNames.end()) {
                            targetName = it->second;
                            // CRITICAL FIX: Even if reusing, add to functionLocalNames!
                            functionLocalNames[paramName] = targetName;
                        } else {
                            // Create unique name for this parameter
                            targetName = paramName + "#" + to_string(++symbolNameCounter);
                            symbolUniqueNames[paramSym] = targetName;
                            functionLocalNames[paramName] = targetName;  // Also add to function-local map
                        }
                    }
                    
                    // Extract parameter type from PARAM_DECL
                    string paramType = "";
                    for (auto paramChild : param->children) {
                        if (paramChild->name == "DECL_SPECIFIERS" || paramChild->name == "SPEC_QUAL_LIST") {
                            paramType = extractTypeFromSpecQualList(paramChild);
                            break;
                        }
                    }
                    
                    // Check if this parameter is a struct type (either directly or via typedef)
                    bool isStructParam = (paramType.find("struct ") == 0);
                    string structTypeName = "";
                    
                    if (isStructParam) {
                        structTypeName = paramType.substr(7);  // Remove "struct " prefix
                    } else {
                        // Check if paramType is a typedef that resolves to a struct
                        Symbol* typedefSym = symbolTable.lookuph(paramType);
                        if (typedefSym && typedefSym->isTypedef && typedefSym->isStruct) {
                            isStructParam = true;
                            structTypeName = typedefSym->type;
                            if (structTypeName.find("struct ") == 0) {
                                structTypeName = structTypeName.substr(7);
                            }
                        }
                    }
                    
                    if (!targetName.empty()) {
                        if (isStructParam && !structTypeName.empty()) {
                            // Struct parameter - receive each member as separate param
                            Symbol* structTypeSym = symbolTable.lookuph(structTypeName);
                            
                            if (structTypeSym && structTypeSym->isStruct) {
                                // Receive each member and store into struct
                                for (const string& memberName : structTypeSym->structMemberOrder) {
                                    int offset = 0;
                                    for (const string& m : structTypeSym->structMemberOrder) {
                                        if (m == memberName) break;
                                        offset += 4;
                                    }
                                    
                                    // Get parameter value
                                    string memberTemp = createTemp();
                                    instructions.emplace_back(TACOp::GET_PARAM, memberTemp, to_string(paramIndex));
                                    
                                    // Store into struct member: addr = &struct + offset, *addr = value
                                    string addrTemp = createTemp();
                                    instructions.emplace_back(TACOp::ADDRESS, addrTemp, targetName);
                                    string offsetTemp = createTemp();
                                    instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
                                    string memberAddrTemp = createTemp();
                                    instructions.emplace_back(TACOp::ADD, memberAddrTemp, addrTemp, offsetTemp);
                                    instructions.emplace_back(TACOp::STORE, memberAddrTemp, memberTemp);
                                    
                                    paramIndex++;
                                }
                            }
                        } else {
                            // Normal parameter
                            cout << "DEBUG: Parameter " << paramIndex << ": " << targetName << endl;
                            
                            // Generate GET_PARAM instruction into the unique name
                            instructions.emplace_back(TACOp::GET_PARAM, targetName, to_string(paramIndex));
                            
                            paramIndex++;
                        }
                    }
                }
            }
        }
        // Also handle direct PARAM_DECL (for compatibility)
        else if (child->name == "PARAM_DECL") {
            string paramName = findParameterName(child);
            Node* idNode = nullptr;
            std::function<Node*(Node*)> findId = [&](Node* n) -> Node*{
                if (!n) return nullptr;
                if (n->name == "IDENTIFIER") return n;
                for (auto c : n->children) { auto r = findId(c); if (r) return r; }
                return nullptr;
            };
            idNode = findId(child);
            string targetName = paramName;
            if (idNode) targetName = getUniqueNameFor(idNode, paramName);
            if (!targetName.empty()) {
                cout << "DEBUG: Parameter " << paramIndex << ": " << targetName << endl;
                instructions.emplace_back(TACOp::GET_PARAM, targetName, to_string(paramIndex));
                paramIndex++;
            }
        }
    }
    
    cout << "DEBUG: Total parameters processed: " << paramIndex << endl;
}
string IRGenerator::findParameterName(Node* paramDecl) {
    if (!paramDecl) return "";
    
    // FIXED: Enhanced parameter name extraction for array and pointer declarators
    // Look for declarator in parameter declaration
    for (auto child : paramDecl->children) {
        if (child->name == "DECLARATOR") {
            string name = getDeclaratorName(child);
            if (!name.empty()) return name;
        }
        // Also check direct IDENTIFIER (for simple parameters)
        if (child->name == "IDENTIFIER") {
            return child->lexeme;
        }
    }
    
    // FIXED: If not found yet, search recursively for IDENTIFIER in the entire subtree
    // This handles complex declarators like array parameters: int arr[]
    std::function<string(Node*)> findIdentifierRecursive = [&](Node* n) -> string {
        if (!n) return "";
        if (n->name == "IDENTIFIER") return n->lexeme;
        for (auto c : n->children) {
            string result = findIdentifierRecursive(c);
            if (!result.empty()) return result;
        }
        return "";
    };
    
    return findIdentifierRecursive(paramDecl);
}
void IRGenerator::generateIfStatement(Node* node) {
    if (!node) return;
    
    cout << "DEBUG: Generating if statement" << endl;
    
    if (node->name == "IF_STMT") {
        // IF without ELSE: condition, then-statement
        if (node->children.size() >= 2) {
            string conditionTemp = generateExpression(node->children[0]);
             string condType = getExpressionResultType(node->children[0]);
            if (condType != "bool") {
                string boolCond = createTemp();
                instructions.emplace_back(TACOp::CAST, boolCond, conditionTemp, "bool");
                conditionTemp = boolCond;
            }
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
             string condType = getExpressionResultType(node->children[0]);
            if (condType != "bool") {
                string boolCond = createTemp();
                instructions.emplace_back(TACOp::CAST, boolCond, conditionTemp, "bool");
                conditionTemp = boolCond;
            }
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
     string condType = getExpressionResultType(node->children[0]);
    if (condType != "bool") {
        string boolCond = createTemp();
        instructions.emplace_back(TACOp::CAST, boolCond, conditionTemp, "bool");
        conditionTemp = boolCond;
    }
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
        
        // Check if we need type conversion for the return value
        if (!currentFunction.empty() && !currentFunctionReturnType.empty()) {
            string returnType = normalizeTypeName(currentFunctionReturnType);
            Node* returnExpr = node->children[0];
            string exprType = getExpressionResultType(returnExpr);
            
            cout << "DEBUG: Return type conversion check - expr: " << exprType 
                 << " -> function return: " << returnType << endl;
            
            // If types differ, insert conversion
            if (returnType != exprType && returnType != "void" && exprType != "void") {
                string convertedTemp = createTemp();
                instructions.emplace_back(TACOp::CAST, convertedTemp, returnValue, returnType);
                cout << "DEBUG: Return type conversion: " << returnValue << " (" << exprType 
                     << ") -> " << convertedTemp << " (" << returnType << ")" << endl;
                returnValue = convertedTemp;
            }
        }
        
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
    
    // Handle FUNCTION_DECL which contains the function name
    if (node->name == "FUNCTION_DECL") {
        for (auto child : node->children) {
            if (child->name == "IDENTIFIER") {
                return child->lexeme;
            }
        }
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
    size_t bodyIndex = (node->children.size() == 4) ? 3 : 2;
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
cout << "DEBUG: For loop condition check - child[1] name: " << node->children[1]->name << endl;

// Check if condition is empty (either EMPTY or EMPTY_STMT)
if (node->children[1]->name == "EMPTY" || node->children[1]->name == "EMPTY_STMT") {
    // Empty condition means infinite loop - unconditional jump
    cout << "DEBUG: Empty for condition detected, generating infinite loop" << endl;
    instructions.emplace_back(TACOp::GOTO, startLabel);
} else {
    string condTemp = generateExpression(node->children[1]);
    
    // Check if expression actually returned a value
    if (condTemp.empty()) {
        cout << "DEBUG: Empty condition temp, treating as infinite loop" << endl;
        instructions.emplace_back(TACOp::GOTO, startLabel);
    } else {
        string condType = getExpressionResultType(node->children[1]);
        if (condType != "bool") {
            string boolCond = createTemp();
            instructions.emplace_back(TACOp::CAST, boolCond, condTemp, "bool");
            condTemp = boolCond;
        }
        instructions.emplace_back(TACOp::IF_GOTO, startLabel, condTemp);
    }
}
  /*  if (node->children[1]->name != "EMPTY") {
        string condTemp = generateExpression(node->children[1]);
        string condType = getExpressionResultType(node->children[1]);
        if (condType != "bool") {
            string boolCond = createTemp();
            instructions.emplace_back(TACOp::CAST, boolCond, condTemp, "bool");
            condTemp = boolCond;
        }
        instructions.emplace_back(TACOp::IF_GOTO, startLabel, condTemp);
    } else {
        instructions.emplace_back(TACOp::GOTO, startLabel);
    }*/
    
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
    string condLabel = createLabel("do_cond");
    string endLabel = createLabel("do_end");

    // Save previous labels
    string prevBreakLabel = currentBreakLabel;
    string prevContinueLabel = currentContinueLabel;

    // Set current labels for break/continue
    currentBreakLabel = endLabel;
    currentContinueLabel = condLabel;

    // Loop body label
    instructions.emplace_back(TACOp::LABEL, startLabel);

    // Generate loop body (child 0)
    generateStatement(node->children[0]);

    // Condition check label (targets continue)
    instructions.emplace_back(TACOp::LABEL, condLabel);

    // Generate condition (child 1)
    string condTemp = generateExpression(node->children[1]);
     string condType = getExpressionResultType(node->children[1]);
    if (condType != "bool") {
        string boolCond = createTemp();
        instructions.emplace_back(TACOp::CAST, boolCond, condTemp, "bool");
        condTemp = boolCond;
    }
    // If condition is true, jump back to start
    instructions.emplace_back(TACOp::IF_GOTO, startLabel, condTemp);

    // End label
    instructions.emplace_back(TACOp::LABEL, endLabel);

    // Restore previous labels
    currentBreakLabel = prevBreakLabel;
    currentContinueLabel = prevContinueLabel;
}


string IRGenerator::generatePreIncrement(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* operand = node->children[0];
    // Fast path: simple identifier
    if (operand->name == "IDENTIFIER" && !(operand->symbol && operand->symbol->isReference)) {
        string varName = getUniqueNameFor(operand, operand->lexeme);
        bool isPtr = isPointerType(operand);
        int stepSize = isPtr ? getPointedToSize(operand) : 1;
        string stepTemp = createTemp();
        string resultTemp = createTemp();
        instructions.emplace_back(TACOp::CONST, stepTemp, to_string(stepSize));
        instructions.emplace_back(TACOp::ADD, resultTemp, varName, stepTemp);
        instructions.emplace_back(TACOp::ASSIGN, varName, resultTemp);
        return varName;
    }
    
    // General lvalue: compute address, load, add, and store back
    string addr = generateLValueAddress(operand);
    if (addr.empty()) return "";
    int stepSize = getIncrementStepForLValue(operand);
    string oldVal = createTemp();
    string stepTemp = createTemp();
    string newVal = createTemp();
    instructions.emplace_back(TACOp::LOAD, oldVal, addr);
    instructions.emplace_back(TACOp::CONST, stepTemp, to_string(stepSize));
    instructions.emplace_back(TACOp::ADD, newVal, oldVal, stepTemp);
    instructions.emplace_back(TACOp::STORE, addr, newVal);
    return newVal;
}
string IRGenerator::generatePreDecrement(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* operand = node->children[0];
    if (operand->name == "IDENTIFIER" && !(operand->symbol && operand->symbol->isReference)) {
        string varName = getUniqueNameFor(operand, operand->lexeme);
        bool isPtr = isPointerType(operand);
        int stepSize = isPtr ? getPointedToSize(operand) : 1;
        string stepTemp = createTemp();
        string resultTemp = createTemp();
        instructions.emplace_back(TACOp::CONST, stepTemp, to_string(stepSize));
        instructions.emplace_back(TACOp::SUB, resultTemp, varName, stepTemp);
        instructions.emplace_back(TACOp::ASSIGN, varName, resultTemp);
        return varName;
    }
    string addr = generateLValueAddress(operand);
    if (addr.empty()) return "";
    int stepSize = getIncrementStepForLValue(operand);
    string oldVal = createTemp();
    string stepTemp = createTemp();
    string newVal = createTemp();
    instructions.emplace_back(TACOp::LOAD, oldVal, addr);
    instructions.emplace_back(TACOp::CONST, stepTemp, to_string(stepSize));
    instructions.emplace_back(TACOp::SUB, newVal, oldVal, stepTemp);
    instructions.emplace_back(TACOp::STORE, addr, newVal);
    return newVal;
}

string IRGenerator::generatePostIncrement(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* operand = node->children[0];
    if (operand->name == "IDENTIFIER" && !(operand->symbol && operand->symbol->isReference)) {
        string varName = getUniqueNameFor(operand, operand->lexeme);
        bool isPtr = isPointerType(operand);
        int stepSize = isPtr ? getPointedToSize(operand) : 1;
        string oldValue = createTemp();
        string stepTemp = createTemp();
        string newValue = createTemp();
        instructions.emplace_back(TACOp::ASSIGN, oldValue, varName);
        instructions.emplace_back(TACOp::CONST, stepTemp, to_string(stepSize));
        instructions.emplace_back(TACOp::ADD, newValue, varName, stepTemp);
        instructions.emplace_back(TACOp::ASSIGN, varName, newValue);
        return oldValue;
    }
    string addr = generateLValueAddress(operand);
    if (addr.empty()) return "";
    int stepSize = getIncrementStepForLValue(operand);
    string oldVal = createTemp();
    string stepTemp = createTemp();
    string newVal = createTemp();
    instructions.emplace_back(TACOp::LOAD, oldVal, addr);
    instructions.emplace_back(TACOp::CONST, stepTemp, to_string(stepSize));
    instructions.emplace_back(TACOp::ADD, newVal, oldVal, stepTemp);
    instructions.emplace_back(TACOp::STORE, addr, newVal);
    return oldVal;
}

string IRGenerator::generatePostDecrement(Node* node) {
    if (!node || node->children.empty()) return "";
    
    Node* operand = node->children[0];
    if (operand->name == "IDENTIFIER" && !(operand->symbol && operand->symbol->isReference)) {
        string varName = getUniqueNameFor(operand, operand->lexeme);
        bool isPtr = isPointerType(operand);
        int stepSize = isPtr ? getPointedToSize(operand) : 1;
        string oldValue = createTemp();
        string stepTemp = createTemp();
        string newValue = createTemp();
        instructions.emplace_back(TACOp::ASSIGN, oldValue, varName);
        instructions.emplace_back(TACOp::CONST, stepTemp, to_string(stepSize));
        instructions.emplace_back(TACOp::SUB, newValue, varName, stepTemp);
        instructions.emplace_back(TACOp::ASSIGN, varName, newValue);
        return oldValue;
    }
    string addr = generateLValueAddress(operand);
    if (addr.empty()) return "";
    int stepSize = getIncrementStepForLValue(operand);
    string oldVal = createTemp();
    string stepTemp = createTemp();
    string newVal = createTemp();
    instructions.emplace_back(TACOp::LOAD, oldVal, addr);
    instructions.emplace_back(TACOp::CONST, stepTemp, to_string(stepSize));
    instructions.emplace_back(TACOp::SUB, newVal, oldVal, stepTemp);
    instructions.emplace_back(TACOp::STORE, addr, newVal);
    return oldVal;
}

/*
string IRGenerator::generateCompoundAssignment(Node* node) {
    if (!node || node->children.size() < 3) return "";
    
    Node* lhs = node->children[0];
    Node* op = node->children[1];
    Node* rhs = node->children[2];
    
    if (lhs->name != "IDENTIFIER") return "";
    
    string varName = getUniqueNameFor(lhs, lhs->lexeme);
    
    // ✅ NEW: Get and validate LHS type
    string lhsType = normalizeTypeName(lhs->symbol ? lhs->symbol->type : "int");
    
    // ✅ NEW: Handle bool in compound assignment (treat as int)
    if (lhsType == "bool") {
        cout << "WARNING: Compound assignment on bool type, treating as int" << endl;
        lhsType = "int";
    }
    
    string rhsType = getExpressionResultType(rhs);
    string rhsTemp = generateExpression(rhs);
    
    // ✅ NEW: Get operator
    string opStr = op->lexeme;
    
    // ✅ NEW: Validate operation type compatibility
    if (opStr == "%=" || opStr == "&=" || opStr == "|=" || 
        opStr == "^=" || opStr == "<<=" || opStr == ">>=") {
        // Bitwise/modulo ops require integer types
        if (lhsType == "float" || lhsType == "double" ||
            rhsType == "float" || rhsType == "double") {
            cout << "ERROR: Bitwise/modulo compound assignment on floating type" << endl;
            return "";
        }
    }
    
    // Determine common type for operation
    string commonType = lhsType;
    
    if (rhsType == "double" || lhsType == "double") {
        commonType = "double";
    } else if (rhsType == "float" || lhsType == "float") {
        commonType = "float";
    } else if (rhsType == "long" || lhsType == "long") {
        commonType = "long";
    } else {
        commonType = "int";
    }
    
    // Convert LHS to common type if needed for operation
    string lhsOperand = varName;
    if (lhsType != commonType) {
        string convertedLhs = createTemp();
        instructions.emplace_back(TACOp::CAST, convertedLhs, varName, commonType);
        lhsOperand = convertedLhs;
        cout << "DEBUG: Compound assignment - promoting LHS: " << lhsType 
             << " -> " << commonType << endl;
    }
    
    // Convert RHS to common type if needed
    string rhsOperand = rhsTemp;
    if (rhsType != commonType) {
        string convertedRhs = createTemp();
        instructions.emplace_back(TACOp::CAST, convertedRhs, rhsTemp, commonType);
        rhsOperand = convertedRhs;
        cout << "DEBUG: Compound assignment - promoting RHS: " << rhsType 
             << " -> " << commonType << endl;
    }
    
    string resultTemp = createTemp();
    
    // Perform operation
    if (opStr == "+=") {
        instructions.emplace_back(TACOp::ADD, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "-=") {
        instructions.emplace_back(TACOp::SUB, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "*=") {
        instructions.emplace_back(TACOp::MUL, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "/=") {
        instructions.emplace_back(TACOp::DIV, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "%=") {
        instructions.emplace_back(TACOp::MOD, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "&=") {
        instructions.emplace_back(TACOp::BIT_AND, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "|=") {
        instructions.emplace_back(TACOp::BIT_OR, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "^=") {
        instructions.emplace_back(TACOp::BIT_XOR, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "<<=") {
        instructions.emplace_back(TACOp::SHL, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == ">>=") {
        instructions.emplace_back(TACOp::SHR, resultTemp, lhsOperand, rhsOperand);
    }
    else {
        cout << "WARNING: Unknown compound operator: " << opStr << endl;
        return "";
    }
    
    // Convert result back to LHS type if needed
    string finalValue = resultTemp;
    if (commonType != lhsType) {
        string castedTemp = createTemp();
        instructions.emplace_back(TACOp::CAST, castedTemp, resultTemp, lhsType);
        finalValue = castedTemp;
        cout << "DEBUG: Compound assignment - converting result: " << commonType 
             << " -> " << lhsType << endl;
    }
    
    // Store result back to variable
    instructions.emplace_back(TACOp::ASSIGN, varName, finalValue);
    
    return varName;
}*/


string IRGenerator::generateCompoundAssignment(Node* node) {
    if (!node || node->children.size() < 3) return "";
    
    Node* lhs = node->children[0];
    Node* op = node->children[1];
    Node* rhs = node->children[2];
    
    if (lhs->name != "IDENTIFIER") return "";
    
    string varName = getUniqueNameFor(lhs, lhs->lexeme);
    
    // Get operator
    string opStr = op->lexeme;
    
    // Check if LHS is a pointer type
    bool isPointer = isPointerType(lhs);
    
    // ✅ Special handling for pointer arithmetic (p += n, p -= n)
    if (isPointer && (opStr == "+=" || opStr == "-=")) {
        cout << "DEBUG: Compound assignment with pointer arithmetic" << endl;
        
        string rhsTemp = generateExpression(rhs);
        
        // Use generatePointerArithmetic to handle scaling
        TACOp arithmeticOp = (opStr == "+=") ? TACOp::ADD : TACOp::SUB;
        string resultTemp = generatePointerArithmetic(lhs, varName, rhsTemp, arithmeticOp);
        
        // Store result back to variable
        instructions.emplace_back(TACOp::ASSIGN, varName, resultTemp);
        
        return varName;
    }
    
    // ✅ Get ORIGINAL LHS type (don't modify yet!)
    string originalLhsType = normalizeTypeName(lhs->symbol ? lhs->symbol->type : "int");
    
    // ✅ Determine OPERATION type (what type to use for the arithmetic)
    string operationType = originalLhsType;
    
    // Handle bool in compound assignment (treat as int FOR OPERATION)
    if (originalLhsType == "bool" || originalLhsType == "_Bool") {
        cout << "WARNING: Compound assignment on bool type, treating as int" << endl;
        operationType = "int";  // ✅ Use int for operation, but keep original type
    }
    
    string rhsType = getExpressionResultType(rhs);
    string rhsTemp = generateExpression(rhs);
    
    // Validate operation type compatibility
    if (opStr == "%=" || opStr == "&=" || opStr == "|=" || 
        opStr == "^=" || opStr == "<<=" || opStr == ">>=") {
        // Bitwise/modulo ops require integer types
        if (operationType == "float" || operationType == "double" ||
            rhsType == "float" || rhsType == "double") {
            cout << "ERROR: Bitwise/modulo compound assignment on floating type" << endl;
            return "";
        }
    }
    
    // Determine common type for operation
    string commonType = operationType;  // ✅ Start with operation type, not original
    
    if (rhsType == "double" || operationType == "double") {
        commonType = "double";
    } else if (rhsType == "float" || operationType == "float") {
        commonType = "float";
    } else if (rhsType == "long" || operationType == "long") {
        commonType = "long";
    } else {
        commonType = "int";
    }
    
    // ✅ Convert LHS from ORIGINAL type to common type
    string lhsOperand = varName;
    if (originalLhsType != commonType) {  // ✅ Compare ORIGINAL type
        string convertedLhs = createTemp();
        instructions.emplace_back(TACOp::CAST, convertedLhs, varName, commonType);
        lhsOperand = convertedLhs;
        cout << "DEBUG: Compound assignment - promoting LHS: " << originalLhsType 
             << " -> " << commonType << endl;
    }
    
    // Convert RHS to common type if needed
    string rhsOperand = rhsTemp;
    if (rhsType != commonType) {
        string convertedRhs = createTemp();
        instructions.emplace_back(TACOp::CAST, convertedRhs, rhsTemp, commonType);
        rhsOperand = convertedRhs;
        cout << "DEBUG: Compound assignment - promoting RHS: " << rhsType 
             << " -> " << commonType << endl;
    }
    
    string resultTemp = createTemp();
    
    // Perform operation
    if (opStr == "+=") {
        instructions.emplace_back(TACOp::ADD, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "-=") {
        instructions.emplace_back(TACOp::SUB, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "*=") {
        instructions.emplace_back(TACOp::MUL, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "/=") {
        instructions.emplace_back(TACOp::DIV, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "%=") {
        instructions.emplace_back(TACOp::MOD, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "&=") {
        instructions.emplace_back(TACOp::BIT_AND, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "|=") {
        instructions.emplace_back(TACOp::BIT_OR, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "^=") {
        instructions.emplace_back(TACOp::BIT_XOR, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == "<<=") {
        instructions.emplace_back(TACOp::SHL, resultTemp, lhsOperand, rhsOperand);
    }
    else if (opStr == ">>=") {
        instructions.emplace_back(TACOp::SHR, resultTemp, lhsOperand, rhsOperand);
    }
    else {
        cout << "WARNING: Unknown compound operator: " << opStr << endl;
        return "";
    }
    
    // ✅ Convert result back to ORIGINAL LHS type
    string finalValue = resultTemp;
    if (commonType != originalLhsType) {  // ✅ Compare with ORIGINAL type
        string castedTemp = createTemp();
        instructions.emplace_back(TACOp::CAST, castedTemp, resultTemp, originalLhsType);
        finalValue = castedTemp;
        cout << "DEBUG: Compound assignment - converting result: " << commonType 
             << " -> " << originalLhsType << endl;
    }
    
    // Store result back to variable
    instructions.emplace_back(TACOp::ASSIGN, varName, finalValue);
    
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
                // CASE_ELEMENT has: CASE_VALUE, STATEMENT_LIST
                if (!caseNode->children.empty()) {
                    CaseInfo info;
                    Node* caseValueNode = caseNode->children[0];
                    
                    // ✅ FIX: Check if the case value is an enum constant
                    if (caseValueNode->name == "IDENTIFIER") {
                        string caseName = caseValueNode->lexeme;
                        if (enumConstants.find(caseName) != enumConstants.end()) {
                            // It's an enum constant - use its numeric value
                            info.value = to_string(enumConstants[caseName]);
                            cout << "DEBUG: Case " << caseName << " resolved to " 
                                 << info.value << endl;
                        } else {
                            // Unknown identifier in case (error, but use as-is)
                            info.value = caseName;
                        }
                    } else if (caseValueNode->name == "CHAR_LITERAL") {
                        // Decode char literal to numeric for robust IR
                        unsigned char ch = decodeCharLiteralIR(caseValueNode->lexeme);
                        info.value = to_string((unsigned int)ch);
                    } else if ((caseValueNode->name == "UNARY_MINUS" || caseValueNode->name == "UNARY_OP") 
                               && !caseValueNode->children.empty()) {
                        // Handle negative constants like -5, -10
                        // UNARY_OP has operator as first child, operand as second
                        Node* operand = caseValueNode->children.size() == 2 ? 
                                       caseValueNode->children[1] : caseValueNode->children[0];
                        if (operand->name == "INTEGER_CONSTANT") {
                            info.value = "-" + operand->lexeme;
                        } else {
                            info.value = "-" + operand->lexeme;
                        }
                    } else {
                        // INTEGER_CONSTANT or other literal
                        info.value = caseValueNode->lexeme;
                    }
                    
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
        
        // Load case constant (now with proper enum value)
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



void IRGenerator::generateBreakStatement() {
    cout << "DEBUG: Generating break statement" << endl;
    
    if (!currentBreakLabel.empty()) {
        instructions.emplace_back(TACOp::GOTO, currentBreakLabel);
    } else {
        cout << "ERROR: break statement outside of loop/switch" << endl;
    }
}

void IRGenerator::generateContinueStatement() {
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
      string condType = getExpressionResultType(condition);
    if (condType != "bool") {
        string boolCond = createTemp();
        instructions.emplace_back(TACOp::CAST, boolCond, condTemp, "bool");
        condTemp = boolCond;
    }
    string resultTemp = createTemp();
    string trueLabel = createLabel("ternary_true");
    string falseLabel = createLabel("ternary_false");
    string endLabel = createLabel("ternary_end");
    
    // If condition is false, jump to false branch
    instructions.emplace_back(TACOp::IF_FALSE_GOTO, falseLabel, condTemp);
     
    // ✅ NEW: Determine common result type
    string trueType = getExpressionResultType(trueExpr);
    string falseType = getExpressionResultType(falseExpr);
    
    string resultType = "int";
    if (trueType == "double" || falseType == "double") {
        resultType = "double";
    } else if (trueType == "float" || falseType == "float") {
        resultType = "float";
    } else if (trueType == "long" || falseType == "long") {
        resultType = "long";
    }
    
    cout << "DEBUG: Ternary result type: " << resultType << endl;
    
    // True branch
    instructions.emplace_back(TACOp::LABEL, trueLabel);
    string trueTemp = generateExpression(trueExpr);
      // ✅ NEW: Convert true branch to result type
    if (normalizeTypeName(trueType) != resultType) {
        string convertedTemp = createTemp();
        instructions.emplace_back(TACOp::CAST, convertedTemp, trueTemp, resultType);
        trueTemp = convertedTemp;
        cout << "DEBUG: Ternary - converting true branch: " << trueType 
             << " -> " << resultType << endl;
    }
    instructions.emplace_back(TACOp::ASSIGN, resultTemp, trueTemp);
    instructions.emplace_back(TACOp::GOTO, endLabel);
    
    // False branch
    instructions.emplace_back(TACOp::LABEL, falseLabel);
    string falseTemp = generateExpression(falseExpr);
     if (normalizeTypeName(falseType) != resultType) {
        string convertedTemp = createTemp();
        instructions.emplace_back(TACOp::CAST, convertedTemp, falseTemp, resultType);
        falseTemp = convertedTemp;
        cout << "DEBUG: Ternary - converting false branch: " << falseType 
             << " -> " << resultType << endl;
    }
    instructions.emplace_back(TACOp::ASSIGN, resultTemp, falseTemp);
    
    // End
    instructions.emplace_back(TACOp::LABEL, endLabel);
    
    return resultTemp;
}

string IRGenerator::generateArrayStore(Node* arrayNode, Node* valueNode) {
    if (!arrayNode || arrayNode->children.size() < 2) return "";
    
    if (kIrDebug) cout << "DEBUG: Generating array store" << endl;
    
    Node* arrayBase = arrayNode->children[0];
    // Unwrap EXPR_LIST for base if present (e.g., from parser wrappers)
    Node* baseExpr = arrayBase;
    if (baseExpr && baseExpr->name == "EXPR_LIST" && !baseExpr->children.empty()) {
        baseExpr = baseExpr->children[0];
    }
    Node* indexExpr = arrayNode->children[1];
    
    string valueTemp = generateExpression(valueNode);
    
    // CRITICAL FIX: Check if we need type conversion (e.g., float to int)
    // Determine the array element type
    string arrayElemType = "int";  // default
    bool arrayIsFloat = false;
    if (baseExpr->name == "IDENTIFIER" && baseExpr->symbol) {
        arrayElemType = baseExpr->symbol->type;
        // Check if it's a float array (type starts with "float" or is exactly "float")
        arrayIsFloat = (arrayElemType.find("float") == 0 || arrayElemType == "float");
    } else if (baseExpr->name == "ARRAY_ACCESS" && baseExpr->children.size() > 0) {
        // For multi-dim arrays like fMatrix[i][j], get type from the outermost base
        Node* innerBase = baseExpr->children[0];
        while (innerBase && innerBase->name == "ARRAY_ACCESS" && innerBase->children.size() > 0) {
            innerBase = innerBase->children[0];
        }
        if (innerBase && innerBase->name == "IDENTIFIER" && innerBase->symbol) {
            arrayElemType = innerBase->symbol->type;
            arrayIsFloat = (arrayElemType.find("float") == 0 || arrayElemType == "float");
        }
    }
    
    // Check if the value is a float and the array is int (need cast)
    bool valueIsFloat = (valueNode->name == "FLOAT_CONSTANT" || 
                         (valueNode->name == "IDENTIFIER" && valueNode->symbol && 
                          valueNode->symbol->type == "float"));
    
    if (valueIsFloat && !arrayIsFloat && arrayElemType == "int") {
        // Need to cast float to int ONLY if array is int type
        string castTemp = createTemp();
        instructions.emplace_back(TACOp::CAST, castTemp, valueTemp, "int");
        valueTemp = castTemp;
        if (kIrDebug) cout << "DEBUG: Casting float to int for array store" << endl;
    }
    
    // Check if this is multi-dimensional array store
    if (arrayBase->name == "ARRAY_ACCESS") {
        if (kIrDebug) cout << "DEBUG: Multi-dimensional array store" << endl;
        
        // Get address of arr[i] (sub-array)
        string subArrayAddr = generateMultiDimArrayAddress(arrayBase);
        
        // Unwrap EXPR_LIST for index
        Node* actualIndexExpr = indexExpr;
        if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
            actualIndexExpr = indexExpr->children[0];
        }
        
        // Calculate offset for second index
        string indexTemp = generateExpression(actualIndexExpr);
    int elemSize = getInnerElementSize(arrayBase);
        
        string elemSizeTemp = createTemp();
        string offsetTemp = createTemp();
        string finalAddr = createTemp();
        
        instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
        instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, elemSizeTemp);
        instructions.emplace_back(TACOp::ADD, finalAddr, subArrayAddr, offsetTemp);
        
        // Store value at calculated address
        instructions.emplace_back(TACOp::STORE, finalAddr, valueTemp);
        
        return valueTemp;
    }
    
    // Single-dimensional array store: base[i] = value
    // Special-case: array member like a.b[i] = v
    if (baseExpr->name == "MEMBER_ACCESS" || baseExpr->name == "PTR_MEMBER_ACCESS") {
        // Base address is the address of the member within the struct
        string memberAddr = generateLValueAddress(baseExpr);
        // Unwrap index
        Node* actualIndexExprM = indexExpr;
        if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
            actualIndexExprM = indexExpr->children[0];
        }
        string indexTempM = generateExpression(actualIndexExprM);
        int elemSizeM = getPointedToSize(baseExpr);
        string elemSizeTempM = createTemp();
        string offsetTempM = createTemp();
        string finalAddrM = createTemp();
        instructions.emplace_back(TACOp::CONST, elemSizeTempM, to_string(elemSizeM));
        instructions.emplace_back(TACOp::MUL, offsetTempM, indexTempM, elemSizeTempM);
        instructions.emplace_back(TACOp::ADD, finalAddrM, memberAddr, offsetTempM);
        instructions.emplace_back(TACOp::STORE, finalAddrM, valueTemp);
        return valueTemp;
    }
    // Detect if base is an array variable (needs &base) or a pointer (use its value)
    // Unwrap EXPR_LIST for index
Node* actualIndexExpr = indexExpr;
if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
    actualIndexExpr = indexExpr->children[0];
}

string indexTemp = generateExpression(actualIndexExpr);

string baseAddr = createTemp();
  // bool baseIsAddress = false;
    
  
    if (baseExpr->name == "IDENTIFIER") {
    // IDENTIFIER can be an array or a pointer
        Symbol* sym = baseExpr->symbol;
        string baseName = getUniqueNameFor(baseExpr, baseExpr->lexeme);
    if (sym && sym->isArray) {
        if (sym->isReference) {
            // Reference to array: baseName already holds base address
            instructions.emplace_back(TACOp::ASSIGN, baseAddr, baseName);
            if (kIrDebug) cout << "DEBUG: Array store on array REFERENCE IDENTIFIER, using address value" << endl;
        } else {
            // Array variable: take address of the array object
            instructions.emplace_back(TACOp::ADDRESS, baseAddr, baseName);
            if (kIrDebug) cout << "DEBUG: Array store on array IDENTIFIER, using ADDRESS" << endl;
        }
    } else if (sym && sym->pointerDepth > 0) {
        // Pointer variable: use pointer value as base address
            instructions.emplace_back(TACOp::ASSIGN, baseAddr, baseName);
        if (kIrDebug) cout << "DEBUG: Array store on pointer IDENTIFIER, using pointer value" << endl;
    } else {
        // Fallback: treat as taking address (unlikely)
            instructions.emplace_back(TACOp::ADDRESS, baseAddr, baseName);
        if (kIrDebug) cout << "DEBUG: Array store on IDENTIFIER, default ADDRESS" << endl;
    }
}
else if (baseExpr->name == "ARRAY_ACCESS") {
    // Multi-dimensional - already handled above
    cout << "ERROR: Unexpected nested ARRAY_ACCESS in store" << endl;
}
else if (expressionReturnsAddress(baseExpr)) {
    // Expression yields an address (pointer)
    string exprTemp = generateExpression(baseExpr);
    instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
    cout << "DEBUG: Expression returns address, using directly" << endl;
}
else {
    // Expression yields a value that should itself be a pointer value
    // Use it directly as the base (do not take address of a temporary)
    string exprTemp = generateExpression(baseExpr);
    instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
    cout << "DEBUG: Expression returns value, using as base" << endl;
}

    // Get element size
    int elemSize = 4; // default
    if (baseExpr->name == "IDENTIFIER" && baseExpr->symbol) {
        Symbol* sym = baseExpr->symbol;
        if (sym->isArray) {
            // Array variable: element size depends on element type
            // For array of pointers, elements are pointer-sized (4 bytes on MIPS32)
            elemSize = (sym->pointerDepth > 0) ? 4 : getBaseTypeSize(sym->type);
        } else if (sym->pointerDepth > 0) {
            // Pointer variable: element size is the pointed-to base type
            elemSize = getBaseTypeSize(sym->type);
        } else {
            elemSize = getBaseTypeSize(sym->type);
        }
    } else {
        // For non-identifier bases (e.g., pointer arithmetic expressions),
        // determine size from the pointed-to type of the base expression
        elemSize = getPointedToSize(baseExpr);
        // Special-case: when base is pointer arithmetic on an array-of-pointers
        // (e.g., (strs+2)[-1]), ensure element size is pointer-size (8).
        if ((baseExpr->name == "ADD_EXPR" || baseExpr->name == "SUB_EXPR") && baseExpr->children.size() >= 1) {
            Node* left = baseExpr->children[0];
            Node* right = (baseExpr->children.size() >= 2) ? baseExpr->children[1] : nullptr;
            if (left && left->name == "IDENTIFIER" && left->symbol && left->symbol->isArray && left->symbol->pointerDepth > 0) {
                elemSize = 4;
            } else if (right && right->name == "IDENTIFIER" && right->symbol && right->symbol->isArray && right->symbol->pointerDepth > 0) {
                elemSize = 4;
            }
        }
    }
    
    // Calculate address: baseAddr + (index * elemSize)
    string elemSizeTemp = createTemp();
    string offsetTemp = createTemp();
    string finalAddr = createTemp();
    
    instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
    instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, elemSizeTemp);
    instructions.emplace_back(TACOp::ADD, finalAddr, baseAddr, offsetTemp);
    
    // Store value
    instructions.emplace_back(TACOp::STORE, finalAddr, valueTemp);
    
    if (kIrDebug) cout << "DEBUG: Array store complete - STORE at address " << finalAddr << endl;
    
    return valueTemp;
}



string IRGenerator::generateArrayAccess(Node* node) {
    if (!node || node->children.size() < 2) return "";
    
    Node* arrayBase = node->children[0];
    // Unwrap EXPR_LIST for base if present
    Node* baseExpr = arrayBase;
    if (baseExpr && baseExpr->name == "EXPR_LIST" && !baseExpr->children.empty()) {
        baseExpr = baseExpr->children[0];
    }
    Node* indexExpr = node->children[1];
    
    // Check if base is itself an array access (multi-dimensional)
    if (baseExpr->name == "ARRAY_ACCESS") {
        if (kIrDebug) cout << "DEBUG: Multi-dimensional array access detected" << endl;
        
        string subArrayAddr = generateMultiDimArrayAddress(baseExpr);
        
        // Unwrap EXPR_LIST for index
        Node* actualIndexExpr = indexExpr;
        if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
            actualIndexExpr = indexExpr->children[0];
        }
        
        string indexTemp = generateExpression(actualIndexExpr);
        int elemSize = getInnerElementSize(arrayBase);
        
        string elemSizeTemp = createTemp();
        string offsetTemp = createTemp();
        string elemAddr = createTemp();
        string resultTemp = createTemp();
        
        instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
        instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, elemSizeTemp);
        instructions.emplace_back(TACOp::ADD, elemAddr, subArrayAddr, offsetTemp);
        instructions.emplace_back(TACOp::LOAD, resultTemp, elemAddr);
        
        return resultTemp;
    }
    
    // Single dimension - detect if base is an array (needs &base) or a pointer (use its value)
    // Special-case: array member like a.b[i]
    if (baseExpr->name == "MEMBER_ACCESS" || baseExpr->name == "PTR_MEMBER_ACCESS") {
        string memberAddr = generateLValueAddress(baseExpr);
        Node* actualIndexExprM = indexExpr;
        if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
            actualIndexExprM = indexExpr->children[0];
        }
        string indexTempM = generateExpression(actualIndexExprM);
        int elemSizeM = getPointedToSize(baseExpr);
        string elemSizeTempM = createTemp();
        string offsetTempM = createTemp();
        string elemAddrM = createTemp();
        string resultTempM = createTemp();
        instructions.emplace_back(TACOp::CONST, elemSizeTempM, to_string(elemSizeM));
        instructions.emplace_back(TACOp::MUL, offsetTempM, indexTempM, elemSizeTempM);
        instructions.emplace_back(TACOp::ADD, elemAddrM, memberAddr, offsetTempM);
        instructions.emplace_back(TACOp::LOAD, resultTempM, elemAddrM);
        return resultTempM;
    }
    Node* actualIndexExpr = indexExpr;
    if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
    actualIndexExpr = indexExpr->children[0];
}

string indexTemp = generateExpression(actualIndexExpr);
    string baseAddr = createTemp();
  
if (baseExpr->name == "IDENTIFIER") {
    Symbol* sym = baseExpr->symbol;
    string baseName = getUniqueNameFor(baseExpr, baseExpr->lexeme);
    // ✅ FIX: Decayed array parameters (e.g., int a[3][3] -> int(*)[3])
    // have both isArray=true, pointerDepth>0, AND isReference=true.
    // They should use the value, not address.
    // Local array-of-pointers (e.g., int* ptrs[3]) has isArray=true, pointerDepth>0,
    // but isReference=false, so it should use ADDRESS.
    if (sym && sym->isArray && sym->pointerDepth > 0 && sym->isReference) {
        // Decayed array parameter: already a pointer, use its value
        instructions.emplace_back(TACOp::ASSIGN, baseAddr, baseName);
        if (kIrDebug) cout << "DEBUG: Array access on decayed array parameter, using pointer value" << endl;
    } else if (sym && sym->isArray) {
        if (sym->isReference) {
            // Reference to array: variable already holds base address
            instructions.emplace_back(TACOp::ASSIGN, baseAddr, baseName);
            if (kIrDebug) cout << "DEBUG: Array access on array REFERENCE IDENTIFIER, using address value" << endl;
        } else {
            // Array variable: address of array object
            instructions.emplace_back(TACOp::ADDRESS, baseAddr, baseName);
            if (kIrDebug) cout << "DEBUG: Array access on array IDENTIFIER, using ADDRESS" << endl;
        }
    } else if (sym && sym->pointerDepth > 0) {
        // Pointer variable: use its value
    instructions.emplace_back(TACOp::ASSIGN, baseAddr, baseName);
    if (kIrDebug) cout << "DEBUG: Array access on pointer IDENTIFIER, using pointer value" << endl;
    } else {
        // Fallback
    instructions.emplace_back(TACOp::ADDRESS, baseAddr, baseName);
    if (kIrDebug) cout << "DEBUG: Array access on IDENTIFIER, default ADDRESS" << endl;
    }
}
else if (baseExpr->name == "ARRAY_ACCESS") {
    // This case is already handled above in multi-dimensional check
    // Should not reach here
    cout << "ERROR: Unexpected nested ARRAY_ACCESS" << endl;
}
else if (baseExpr->name == "UNARY_OP" && baseExpr->children.size() >= 2 && 
         baseExpr->children[0]->name == "*") {
    // ✅ FIX: Handle (*p)[i] where p is pointer-to-array
    // For pointer-to-array like int (*p)[4], dereferencing just gives us the pointer value
    // We should NOT generate a LOAD instruction
    Node* operand = baseExpr->children[1];
    if (operand && operand->name == "IDENTIFIER") {
        Symbol* sym = operand->symbol;
        // Check if this is a pointer-to-array (pointerDepth > 0 and has pointeeArrayDimensions)
        if (sym && sym->pointerDepth > 0 && !sym->pointeeArrayDimensions.empty()) {
            // This is pointer-to-array: use pointer value directly, don't LOAD
            string ptrName = getUniqueNameFor(operand, operand->lexeme);
            instructions.emplace_back(TACOp::ASSIGN, baseAddr, ptrName);
            if (kIrDebug) cout << "DEBUG: (*pointer-to-array)[i] - using pointer value directly" << endl;
        } else {
            // Regular pointer dereference: generate the expression (will create LOAD)
            string exprTemp = generateExpression(baseExpr);
            instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
            if (kIrDebug) cout << "DEBUG: Dereference expression for array base" << endl;
        }
    } else {
        // Complex dereference: generate the expression
        string exprTemp = generateExpression(baseExpr);
        instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
        if (kIrDebug) cout << "DEBUG: Complex dereference expression for array base" << endl;
    }
}
else if (expressionReturnsAddress(baseExpr)) {
    // Expression yields an address (pointer, &x, func returning pointer)
    string exprTemp = generateExpression(baseExpr);
    instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
    cout << "DEBUG: Expression returns address, using directly" << endl;
}
else {
    // Expression yields a value that should itself be a pointer value
    // Use it directly as the base (do not take address of a temporary)
    string exprTemp = generateExpression(baseExpr);
    instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
    cout << "DEBUG: Expression returns value, using as base" << endl;
}


    // Get element size
    int elemSize = 4; // default
    if (baseExpr->name == "IDENTIFIER" && baseExpr->symbol) {
        Symbol* sym = baseExpr->symbol;
        if (sym->isArray) {
            // For multi-dimensional arrays, calculate size of sub-array
            if (sym->arrayDimensions.size() > 1) {
                // Calculate: size of inner array = product of remaining dims * base type size
                int innerSize = getBaseTypeSize(sym->type);
                for (size_t i = 1; i < sym->arrayDimensions.size(); i++) {
                    innerSize *= sym->arrayDimensions[i];
                }
                elemSize = innerSize;
            } else {
                // Single dimension array
                // Check if elements are pointers (array of pointers)
                if (sym->pointerDepth > 0) {
                    // Array of pointers: elements are pointer-sized (4 bytes on MIPS32)
                    elemSize = 4;
                } else {
                    // Regular array: element is base type size
                    elemSize = getBaseTypeSize(sym->type);
                }
            }
        } else if (sym->pointerDepth > 0) {
            // ✅ FIX: Pointer-to-array: int (*p)[4] has pointeeArrayDimensions=[4]
            // Element size should be sizeof(int[4]) = 16, not sizeof(int) = 4
            if (!sym->pointeeArrayDimensions.empty()) {
                // Calculate size of pointee array: product of all dimensions * base type size
                int pointeeSize = getBaseTypeSize(sym->type);
                for (int dim : sym->pointeeArrayDimensions) {
                    pointeeSize *= dim;
                }
                elemSize = pointeeSize;
            } else {
                // Regular pointer: element is base type
                elemSize = getBaseTypeSize(sym->type);
            }
        } else {
            elemSize = getBaseTypeSize(sym->type);
        }
    } else if (baseExpr->name == "UNARY_OP" && baseExpr->children.size() >= 2 && 
               baseExpr->children[0]->name == "*") {
        // ✅ FIX: Handle (*p)[i] where p is pointer-to-array
        // Element size should be based on the pointee array dimensions
        Node* operand = baseExpr->children[1];
        if (operand && operand->name == "IDENTIFIER") {
            Symbol* sym = operand->symbol;
            if (sym && sym->pointerDepth > 0 && !sym->pointeeArrayDimensions.empty()) {
                // Pointer-to-array: element size is the size of the pointee array elements
                // For int (*p)[4], dereferencing gives int[4], so element size for [i] is sizeof(int) = 4
                elemSize = getBaseTypeSize(sym->type);
            } else {
                elemSize = getPointedToSize(baseExpr);
            }
        } else {
            elemSize = getPointedToSize(baseExpr);
        }
    } else {
        // Non-identifier base expression: use pointed-to size
        elemSize = getPointedToSize(baseExpr);
    }
    
    string elemSizeTemp = createTemp();
    string offsetTemp = createTemp();
    string elemAddr = createTemp();
    string resultTemp = createTemp();
    
    // offset = index * elemSize
    instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
    instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, elemSizeTemp);
    
    // elemAddr = baseAddr + offset
    instructions.emplace_back(TACOp::ADD, elemAddr, baseAddr, offsetTemp);
    
    // Load value
    instructions.emplace_back(TACOp::LOAD, resultTemp, elemAddr);
    
    if (kIrDebug) cout << "DEBUG: Array access complete - LOAD from address " << elemAddr << endl;
    
    return resultTemp;
}

// Generate address of array element WITHOUT loading the value
// Used for pointer initialization: int *ptr = array[i];
string IRGenerator::generateArrayAddressOnly(Node* node) {
    if (!node || node->children.size() < 2) return "";
    
    Node* arrayBase = node->children[0];
    Node* baseExpr = arrayBase;
    if (baseExpr && baseExpr->name == "EXPR_LIST" && !baseExpr->children.empty()) {
        baseExpr = baseExpr->children[0];
    }
    Node* indexExpr = node->children[1];
    
    // Handle multi-dimensional array access
    if (baseExpr->name == "ARRAY_ACCESS") {
        string subArrayAddr = generateMultiDimArrayAddress(baseExpr);
        
        Node* actualIndexExpr = indexExpr;
        if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
            actualIndexExpr = indexExpr->children[0];
        }
        
        string indexTemp = generateExpression(actualIndexExpr);
        int elemSize = getInnerElementSize(arrayBase);
        
        string elemSizeTemp = createTemp();
        string offsetTemp = createTemp();
        string elemAddr = createTemp();
        
        instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
        instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, elemSizeTemp);
        instructions.emplace_back(TACOp::ADD, elemAddr, subArrayAddr, offsetTemp);
        
        return elemAddr;  // Return address without LOAD
    }
    
    // Single dimension array
    Node* actualIndexExpr = indexExpr;
    if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
        actualIndexExpr = indexExpr->children[0];
    }
    
    string indexTemp = generateExpression(actualIndexExpr);
    string baseAddr = createTemp();
    
    if (baseExpr->name == "IDENTIFIER") {
        Symbol* sym = baseExpr->symbol;
        string baseName = getUniqueNameFor(baseExpr, baseExpr->lexeme);
        
        if (sym && sym->isArray) {
            instructions.emplace_back(TACOp::ADDRESS, baseAddr, baseName);
        } else {
            instructions.emplace_back(TACOp::ASSIGN, baseAddr, baseName);
        }
    } else {
        string exprTemp = generateExpression(baseExpr);
        instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
    }
    
    // Calculate element size
    int elemSize = 4;
    if (baseExpr->name == "IDENTIFIER" && baseExpr->symbol) {
        Symbol* sym = baseExpr->symbol;
        if (sym->isArray && sym->arrayDimensions.size() > 1) {
            int innerSize = getBaseTypeSize(sym->type);
            for (size_t i = 1; i < sym->arrayDimensions.size(); i++) {
                innerSize *= sym->arrayDimensions[i];
            }
            elemSize = innerSize;
        } else {
            elemSize = getBaseTypeSize(sym->type);
        }
    }
    
    string elemSizeTemp = createTemp();
    string offsetTemp = createTemp();
    string elemAddr = createTemp();
    
    instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
    instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, elemSizeTemp);
    instructions.emplace_back(TACOp::ADD, elemAddr, baseAddr, offsetTemp);
    
    return elemAddr;  // Return address without LOAD
}



string IRGenerator::generateMemberAccess(Node* node, bool isPointer) {
    if (!node || node->children.size() < 2) return "";
    
    cout << "DEBUG: Generating " << (isPointer ? "pointer" : "direct") 
         << " member access" << endl;
    
    Node* structNode = node->children[0];
    Node* memberNode = node->children[1];
    
    // For member access, we need the base address of the struct object
    // - For ptr->member: base is already an address (pointer value)
    // - For obj.member: compute address of the lvalue (handles identifiers, array elements, nested members)
    string structTemp = isPointer ? generateExpression(structNode)
                                  : generateLValueAddress(structNode);
    string memberName = "";
    
    if (memberNode->name == "IDENTIFIER") {
        memberName = memberNode->lexeme;
    }
    
    if (memberName.empty()) return "";
    
    // Calculate member offset
      int offset = getMemberOffset(structNode, memberName); 
 
    string baseAddr = createTemp();
    string offsetTemp = createTemp();
    string memberAddr = createTemp();
    string resultTemp = createTemp();
    
    if (isPointer) {
        // ptr->member: ptr is already an address, just use it
        instructions.emplace_back(TACOp::ASSIGN, baseAddr, structTemp);
    } else {
        // struct.member: we already computed the lvalue address
        instructions.emplace_back(TACOp::ASSIGN, baseAddr, structTemp);
    }
    
    // Calculate member address: baseAddr + offset
    instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
    instructions.emplace_back(TACOp::ADD, memberAddr, baseAddr, offsetTemp);
    
    // CRITICAL FIX: Determine member type for proper code generation
    // This is especially important for unions where all members share the same offset
    string memberType = "int";  // default
    if (structNode->name == "IDENTIFIER" && structNode->symbol) {
        Symbol* structSym = structNode->symbol;
        string structTypeName = structSym->type;
        
        // Strip "struct " or "union " prefix if present
        if (structTypeName.find("struct ") == 0) {
            structTypeName = structTypeName.substr(7);
        } else if (structTypeName.find("union ") == 0) {
            structTypeName = structTypeName.substr(6);
        }
        
        // Look up the member type from the struct/union definition
        Symbol* typeSym = symbolTable.lookuph(structTypeName);
        if (typeSym && (typeSym->isStruct || typeSym->isUnion)) {
            if (typeSym->structMembers.find(memberName) != typeSym->structMembers.end()) {
                memberType = typeSym->structMembers[memberName];
            }
        }
    }
    
    // Load value from member address
    instructions.emplace_back(TACOp::LOAD, resultTemp, memberAddr);
    
    // Track the type of the loaded value by marking the address temp
    // The code generator will use this to determine load instruction type
    if (memberType == "float") {
        tempTypes[memberAddr] = "float*";
    }
    
    return resultTemp;
}
string IRGenerator::generateMemberStore(Node* memberNode, Node* valueNode, bool isPointer) {
    if (!memberNode || memberNode->children.size() < 2) return "";
    
    Node* structNode = memberNode->children[0];
    Node* memberIdNode = memberNode->children[1];
    
    // Compute base address similarly to access case
    string structTemp = isPointer ? generateExpression(structNode)
                                  : generateLValueAddress(structNode);
    string valueTemp = generateExpression(valueNode);
    string memberName = "";
    
    if (memberIdNode->name == "IDENTIFIER") {
        memberName = memberIdNode->lexeme;
    }
    
    if (memberName.empty()) return "";
    
    // Calculate member offset
    int offset = getMemberOffset(structNode, memberName);
    
    string baseAddr = createTemp();
    string offsetTemp = createTemp();
    string memberAddr = createTemp();
    
    if (isPointer) {
        // ptr->member: ptr is already an address
        instructions.emplace_back(TACOp::ASSIGN, baseAddr, structTemp);
    } else {
        // struct.member: we already computed the lvalue address
        instructions.emplace_back(TACOp::ASSIGN, baseAddr, structTemp);
    }
    
    // Calculate member address: baseAddr + offset
    instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
    instructions.emplace_back(TACOp::ADD, memberAddr, baseAddr, offsetTemp);
    
    // Store value to member address
    instructions.emplace_back(TACOp::STORE, memberAddr, valueTemp);
    
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
      string condType = getExpressionResultType(node->children[0]);
    if (condType != "bool") {
        string boolCond = createTemp();
        instructions.emplace_back(TACOp::CAST, boolCond, conditionTemp, "bool");
        conditionTemp = boolCond;
    }
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
        // sizeof(expr): child may be EXPR_LIST -> expr OR a direct expression node
        Node* child = node->children[0];
        Node* expr = child;
        if (child && child->name == "EXPR_LIST" && !child->children.empty()) {
            expr = child->children[0];
        }
        if (expr) {
            size = getSizeofExpression(expr);
            // Fallback: if for some reason size stayed 0, try a direct symbol-based computation
            if (size == 0 && expr->name == "IDENTIFIER") {
                Symbol* sym = expr->symbol ? expr->symbol : symbolTable.lookuph(expr->lexeme);
                if (sym) {
                    if (sym->isArray && !sym->arrayDimensions.empty()) {
                        // Compute total array size: product(dims) * sizeof(element)
                        int elem = (sym->pointerDepth > 0) ? 4 : getBaseTypeSize(sym->type);  // FIXED: MIPS32 pointer size
                        long long total = elem;
                        for (int dim : sym->arrayDimensions) {
                            if (dim > 0) total *= dim;
                        }
                        size = static_cast<int>(total);
                    } else if (sym->pointerDepth > 0) {
                        size = 4;  // FIXED: MIPS32 pointer size
                    } else {
                        size = getBaseTypeSize(sym->type);
                    }
                }
            }
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
    // If there's an explicit pointer, sizeof(type*) is pointer size (MIPS32 = 4 bytes)
    for (auto child : typeNode->children) {
        if (child->name == "POINTER") {
            return 4;  // FIXED: MIPS32 pointer size
        }
    }
    
    // Extract type from SPEC_QUAL_LIST or handle struct/union specifiers
    for (auto child : typeNode->children) {
        if (child->name == "TYPE_SPECIFIER") {
            typeName = child->lexeme;
        }
        else if (child->name == "TYPE_NAME") {
            // e.g., typedef names like 'S' or 'Mode'
            typeName = child->lexeme;
        }
        else if (child->name == "SPEC_QUAL_LIST") {
            return getSizeofType(child);  // Recurse
        }
        else if (child->name == "STRUCT_OR_UNION_SPECIFIER") {
            // sizeof(struct/union T)
            string structName;
            for (auto sChild : child->children) {
                if (sChild->name == "IDENTIFIER") structName = sChild->lexeme;
            }
            if (!structName.empty()) {
                Symbol* structSym = symbolTable.lookuph(structName);
                if (structSym && (structSym->isStruct || structSym->isUnion)) {
                    return getStructSize(structSym);
                }
            }
            // Unknown struct name; fallback
        }
        else if (child->name == "POINTER") {
            pointerDepth++;
        }
    }
    
    // Any pointer is 8 bytes
    if (pointerDepth > 0) return 4;
    
    // Base type sizes
    if (typeName == "char") return 1;
    if (typeName == "short") return 2;
    if (typeName == "int") return 4;
    if (typeName == "long") return 8;
    if (typeName == "float") return 4;
    if (typeName == "double") return 8;
    if (typeName == "bool") return 1;
    
    // sizeof(struct/union/typedef Name) when represented as TYPE_NAME
    if (!typeName.empty()) {
        string base = typeName;
        // Direct struct/union spellings
        if (base.rfind("struct ", 0) == 0) {
            string tag = base.substr(7);
            if (Symbol* tSym = symbolTable.lookuph(tag)) {
                if (tSym->isStruct || tSym->isUnion) return getStructSize(tSym);
            }
        } else if (base.rfind("union ", 0) == 0) {
            string tag = base.substr(6);
            if (Symbol* tSym = symbolTable.lookuph(tag)) {
                if (tSym->isStruct || tSym->isUnion) return getStructSize(tSym);
            }
        } else {
            // Possibly a typedef or tag name without prefix
            if (Symbol* tSym = symbolTable.lookuph(base)) {
                if (tSym->isTypedef) {
                    if (tSym->pointerDepth > 0) return 4;
                    string aliased = tSym->aliasedType.empty() ? tSym->type : tSym->aliasedType;
                    return getBaseTypeSize(aliased);
                }
                if (tSym->isStruct || tSym->isUnion) {
                    return getStructSize(tSym);
                }
            }
        }
    }
    
    return 4;  // default
}

int IRGenerator::getSizeofExpression(Node* exprNode) {
    if (!exprNode) return 4;

    // Unwrap trivial EXPR_LIST wrappers (e.g., SIZEOF -> EXPR_LIST -> expr)
    while (exprNode && exprNode->name == "EXPR_LIST" && !exprNode->children.empty()) {
        exprNode = exprNode->children[0];
    }
    
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
                    return 4;  // MIPS32 pointer size
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
    
    // ✅ Handle array access (arr[i]) and nested accesses (arr[i][j]...)
    if (exprNode->name == "ARRAY_ACCESS") {
        if (exprNode->children.empty()) return 4;
        
        // Count how many ARRAY_ACCESS levels are applied and find the base
        int levels = 1; // current ARRAY_ACCESS is one level
        Node* base = exprNode->children[0];
        while (base && base->name == "ARRAY_ACCESS") {
            levels++;
            if (base->children.empty()) break;
            base = base->children[0];
        }
        
        if (!base) return 4;
        
        // Identifier-backed array variable
        if (base->name == "IDENTIFIER") {
            Symbol* sym = base->symbol ? base->symbol : symbolTable.lookuph(base->lexeme);
            if (!sym) return 4;
            if (!sym->isArray) {
                // Base is not an array; treat as pointer indexing
                // sizeof(*(ptr + ...)) semantics: size of pointed-to type
                int pDepth = sym->pointerDepth;
                if (pDepth > 1) return 4; // still pointer after deref(s)
                if (pDepth == 1) return getBaseTypeSize(sym->type);
                return getBaseTypeSize(sym->type);
            }
            const vector<int>& dims = sym->arrayDimensions;
            int elemBaseSize = getBaseTypeSize(sym->type);
            int elemPtrDepth = sym->pointerDepth; // pointer depth on element type
            
            if (levels < (int)dims.size()) {
                // Still pointing to a subarray of remaining dimensions
                long long size = elemBaseSize;
                for (size_t i = levels; i < dims.size(); ++i) {
                    int dim = dims[i];
                    if (dim > 0) size *= dim;
                }
                return (int)size;
            } else if (levels == (int)dims.size()) {
                // Exactly at the array element
                if (elemPtrDepth > 0) return 4; // element is a pointer object
                return elemBaseSize;
            } else {
                // Indexed beyond array dims: indexing into the pointer element
                int extra = levels - (int)dims.size();
                int remainingPtr = max(0, elemPtrDepth - extra);
                if (remainingPtr > 0) return 8; // still a pointer
                return elemBaseSize; // fully dereferenced to base type
            }
        }
        
        // For other base expressions (e.g., member producing array/pointer), fallback to pointed-to size
        int elemSize = getPointedToSize(base);
        return elemSize <= 0 ? 4 : elemSize;
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
                        return 4;  // MIPS32 pointer size
                    }
                }
            } else if (operand->name == "UNARY_OP" && operand->children.size() >= 2) {
                // Handle sizeof(*(&X)) -> sizeof(X)
                Node* innerOp = operand->children[0];
                Node* innerExpr = operand->children[1];
                string innerOpStr = innerOp->name.empty() ? innerOp->lexeme : innerOp->name;
                if (innerOpStr == "&") {
                    return getSizeofExpression(innerExpr);
                }
            } else {
                // General-case: sizeof(*<expr>) where <expr> yields a pointer (e.g., array access result)
                int ptSize = getPointedToSize(operand);
                if (ptSize > 0) return ptSize;
            }
        }
         else if (opStr == "&") {
        // Address-of operator - always returns pointer (4 bytes on MIPS32)
        return 4;
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
       Symbol* funcSym = exprNode->children[0]->symbol;   
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
        // Prefer the symbol attached by the analyzer; if missing (e.g., in some
        // sizeof(expr) contexts), fall back to a direct lookup in the current symbol table.
        Symbol* sym = exprNode->symbol ? exprNode->symbol : symbolTable.lookuph(varName);
        
        if (sym != NULL) {
            cout << "DEBUG sizeof: Found symbol " << varName 
                 << " isArray=" << sym->isArray 
                 << " pointerDepth=" << sym->pointerDepth << endl;
            
            // If it's an array, return total size
            if (sym->isArray && !sym->arrayDimensions.empty()) {
                int elemSize = (sym->pointerDepth > 0) ? 8 : getBaseTypeSize(sym->type);
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
            
            // If it's a pointer, return 4 (MIPS32 pointer size)
            if (sym->pointerDepth > 0) {
                cout << "DEBUG sizeof: Returning pointer size 4" << endl;
                return 4;
            }
            // In getSizeofExpression, after checking sym->type:
if (sym->isStruct || sym->isUnion) {
    // Use proper struct size calculation with alignment
    return getStructSize(sym);
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
   
    if (exprNode->name == "STRING_LITERAL") {
        string raw = exprNode->lexeme;
        // Remove quotes
        if (raw.size() >= 2 && raw.front() == '"' && raw.back() == '"') {
            raw = raw.substr(1, raw.size() - 2);
        }
        // Decode a subset of common escapes consistent with initializer handling
        string decoded;
        for (size_t i = 0; i < raw.size(); ++i) {
            char c = raw[i];
            if (c == '\\' && i + 1 < raw.size()) {
                char n = raw[i+1];
                if (n == 'n') { decoded.push_back('\n'); ++i; continue; }
                if (n == 't') { decoded.push_back('\t'); ++i; continue; }
                if (n == 'r') { decoded.push_back('\r'); ++i; continue; }
                if (n == '0') { decoded.push_back('\0'); ++i; continue; }
                if (n == '"') { decoded.push_back('"'); ++i; continue; }
                if (n == '\\') { decoded.push_back('\\'); ++i; continue; }
                // Fallback: treat as literal following char
            }
            decoded.push_back(c);
        }
        return static_cast<int>(decoded.size()) + 1; // +1 for null terminator
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
    // Fast-path for obvious pointer spellings (MIPS32 = 4 bytes)
    if (typeName.find("*") != string::npos) return 4;  // FIXED: MIPS32 pointer size

    // Arrays encoded in type string, e.g., "int[10]" or "struct B[2][3]"
    auto lb = typeName.find('[');
    if (lb != string::npos) {
        string base = typeName.substr(0, lb);
        int elemSize = getBaseTypeSize(base);
        long long total = elemSize;
        size_t pos = lb;
        while (pos < typeName.size()) {
            size_t l = typeName.find('[', pos);
            if (l == string::npos) break;
            size_t r = typeName.find(']', l + 1);
            if (r == string::npos) break;
            string inside = typeName.substr(l + 1, r - l - 1);
            if (inside.empty()) {
                // Unknown size (e.g., []), treat as 0 for sizeof at compile time
                // but don't crash; leave total as-is
            } else {
                int dim = stoi(inside);
                if (dim > 0) total *= dim;
            }
            pos = r + 1;
        }
        return static_cast<int>(total);
    }

    // Built-in scalars
    if (typeName == "char") return 1;
    if (typeName == "short") return 2;
    if (typeName == "int") return 4;
    if (typeName == "long") return 8;
    if (typeName == "float") return 4;
    if (typeName == "double") return 8;
    if (typeName == "bool") return 1;

    // Handle struct/union spelled inline: "struct T" / "union U"
    if (typeName.rfind("struct ", 0) == 0) {
        string tag = typeName.substr(7);
        if (Symbol* s = symbolTable.lookuph(tag)) {
            if (s->isStruct) return getStructSize(s);
        }
        return 0; // unknown tag → size 0 (caller usually pads/alines after)
    }
    if (typeName.rfind("union ", 0) == 0) {
        string tag = typeName.substr(6);
        if (Symbol* s = symbolTable.lookuph(tag)) {
            if (s->isUnion) return getStructSize(s);
        }
        return 0;
    }

    // Handle typedef names or tag names without the "struct/union" prefix
    if (!typeName.empty()) {
        Symbol* tSym = symbolTable.lookuph(typeName);
        if (tSym) {
            // Typedef to something
            if (tSym->isTypedef) {
                // Pointer typedef
                if (tSym->pointerDepth > 0) return 4;
                // Recurse into aliased type (could itself be struct/union or another typedef)
                if (!tSym->aliasedType.empty()) {
                    return getBaseTypeSize(tSym->aliasedType);
                }
                // Fallback to its declared type
                if (!tSym->type.empty()) {
                    return getBaseTypeSize(tSym->type);
                }
            }
            // Direct struct/union tags by plain name
            if (tSym->isStruct || tSym->isUnion) {
                return getStructSize(tSym);
            }
        }
    }

    return 4;  // default to int-size when unknown
}

int IRGenerator::getMemberOffset(Node* structNode, const string& memberName) {
    if (!structNode) return 0;

    // First try to resolve via attached symbol
    Symbol* varSym = structNode->symbol;
    string resolvedStructType = "";
    
    if (!varSym) {
        // Handle array element base: arr[i].member
        if (structNode->name == "ARRAY_ACCESS") {
            // Find the base identifier of the array
            Node* base = structNode;
            while (base && base->name == "ARRAY_ACCESS") {
                if (base->children.empty()) break;
                base = base->children[0];
            }
            if (base && base->name == "IDENTIFIER") {
                varSym = base->symbol ? base->symbol : symbolTable.lookuph(base->lexeme);
            }
        }
    
      if (structNode->name == "FUNC_CALL") {
            if (!structNode->children.empty() && 
                structNode->children[0]->name == "IDENTIFIER") {
                
                string funcName = structNode->children[0]->lexeme;
                Symbol* funcSym = symbolTable.lookuph(funcName);
                
                if (funcSym && funcSym->isFunction) {
                    // Use the function's return type
                    string returnType = funcSym->type;
                    cout << "DEBUG: Function " << funcName 
                         << " returns type: " << returnType << endl;
                    
                    // Remove "struct " prefix
                    if (returnType.find("struct ") == 0) {
                        returnType = returnType.substr(7);
                    }
                    
                    // Look up the struct definition
                    Symbol* structSym = symbolTable.lookuph(returnType);
                    if (structSym && structSym->isStruct) {
                        // Calculate offset with alignment (reuse existing code below)
                        int offset = 0;
                        for (const auto& member : structSym->structMembers) {
                            string memType = member.second;
                            int memberSize = getBaseTypeSize(memType);
                            int memberAlign = getMemberAlignment(memType);
                            
                            if (offset % memberAlign != 0) {
                                offset += memberAlign - (offset % memberAlign);
                            }
                            
                            if (member.first == memberName) {
                                cout << "DEBUG: Member " << memberName 
                                     << " found at offset " << offset << endl;
                                return offset;
                            }
                            
                            offset += memberSize;
                        }
                    }
                }
            }
        }
        // Fallback: try name-based lookup
        if (structNode->name == "IDENTIFIER") {
            varSym = symbolTable.lookuph(structNode->lexeme);
        }

        // As an additional fallback, attempt to resolve the struct type from the lvalue expression itself
        if (!varSym) {
            resolvedStructType = resolveStructTypeFromLValue(structNode);
        }
    }
    
    // Determine the struct/union type string to use
    string structType;
    if (varSym) {
        structType = varSym->type;
    } else if (!resolvedStructType.empty()) {
        structType = resolvedStructType;
    } else {
        cout << "WARNING: Cannot determine struct type" << endl;
        return 0;
    }
    
    // ✅ FIX: Remove pointer suffix first, then struct/union prefix
    // This handles cases like "struct Node*" or "Node*"
    string structTypeName = structType;
    
    // Strip trailing * (and spaces before it) for pointer types
    size_t lastStar = structTypeName.find_last_of('*');
    if (lastStar != string::npos) {
        structTypeName = structTypeName.substr(0, lastStar);
        // Trim trailing spaces
        while (!structTypeName.empty() && structTypeName.back() == ' ') {
            structTypeName.pop_back();
        }
    }
    
    // Remove "struct " or "union " prefix
    if (structTypeName.find("struct ") == 0) {
        structTypeName = structTypeName.substr(7);
    } else if (structTypeName.find("union ") == 0) {
        structTypeName = structTypeName.substr(6);
        // For unions, all members are at offset 0
        return 0;
    }
    
    // Look up the struct definition (type name lookup is OK)
    Symbol* structSym = symbolTable.lookuph(structTypeName);
    if (!structSym || !structSym->isStruct) {
        cout << "WARNING: Struct type " << structTypeName << " not found (original: " << structType << ")" << endl;
        return 0;
    }
    
    // Calculate offset with alignment, preserving declaration order
    int offset = 0;

    std::vector<std::pair<std::string, std::string>> orderedMembers;
    if (!structSym->structMemberOrder.empty()) {
        std::set<std::string> seen;
        for (const auto& name : structSym->structMemberOrder) {
            if (seen.count(name)) continue;
            auto it = structSym->structMembers.find(name);
            if (it != structSym->structMembers.end()) {
                orderedMembers.emplace_back(it->first, it->second);
                seen.insert(name);
            }
        }
    } else {
        for (const auto& kv : structSym->structMembers) orderedMembers.emplace_back(kv.first, kv.second);
    }

    for (const auto& kv : orderedMembers) {
        const std::string& name = kv.first;
        const std::string& memType = kv.second;
        int memberSize = getBaseTypeSize(memType);
        int memberAlign = getMemberAlignment(memType);
        
        // Add padding before this member if needed
        if (offset % memberAlign != 0) {
            offset += memberAlign - (offset % memberAlign);
        }
        
        // Found the member we're looking for
        if (name == memberName) {
            cout << "DEBUG: Member " << memberName << " found at offset " << offset << endl;
            return offset;
        }
        
        // Move to next member
        offset += memberSize;
    }
    
    cout << "WARNING: Member " << memberName << " not found in struct " << structType << endl;
    return 0;
}

// Resolve the struct/union type of an lvalue expression, handling nested member access and array elements
std::string IRGenerator::resolveStructTypeFromLValue(Node* node) {
    if (!node) return "";
    
    // ✅ Handle EXPR_LIST wrapper - unwrap it
    if (node->name == "EXPR_LIST" && !node->children.empty()) {
        return resolveStructTypeFromLValue(node->children[0]);
    }
    
    // Simple identifier: use its symbol type
    if (node->name == "IDENTIFIER") {
        Symbol* sym = node->symbol ? node->symbol : symbolTable.lookuph(node->lexeme);
        if (sym) {
            return sym->type; // May be "struct X", "union Y", or base type
        }
        return "";
    }
    
    // ✅ Handle dereference: (*ptr) strips one pointer level
    if (node->name == "UNARY_OP" && node->children.size() >= 2) {
        Node* opNode = node->children[0];
        Node* operandNode = node->children[1];
        string op = opNode->name.empty() ? opNode->lexeme : opNode->name;
        
        if (op == "*") {
            // Dereference: get operand type and strip one pointer level
            string operandType = resolveStructTypeFromLValue(operandNode);
            if (operandType.empty()) return "";
            
            // Strip one level of pointer (remove trailing *)
            size_t lastStar = operandType.find_last_of('*');
            if (lastStar != string::npos) {
                operandType = operandType.substr(0, lastStar);
                // Trim trailing spaces
                while (!operandType.empty() && operandType.back() == ' ') {
                    operandType.pop_back();
                }
            }
            return operandType;
        }
    }
    
    // Direct member access: base.member
    if (node->name == "MEMBER_ACCESS" || node->name == "DIRECT_MEMBER" || node->name == "DOT") {
        if (node->children.size() < 2) return "";
        Node* base = node->children[0];
        Node* mem = node->children[1];
        if (!mem || mem->name != "IDENTIFIER") return "";
        string baseType = resolveStructTypeFromLValue(base);
        if (baseType.empty()) return "";
        // Strip prefixes
        string tag = baseType;
        if (tag.rfind("struct ", 0) == 0) tag = tag.substr(7);
        else if (tag.rfind("union ", 0) == 0) tag = tag.substr(6);
        Symbol* structSym = symbolTable.lookuph(tag);
        if (!structSym) return "";
        auto it = structSym->structMembers.find(mem->lexeme);
        if (it == structSym->structMembers.end()) return "";
        return it->second; // Return the member's declared type (could be struct/union)
    }
    
    // Pointer member access: base->member
    if (node->name == "PTR_MEMBER_ACCESS" || node->name == "ARROW") {
        if (node->children.size() < 2) return "";
        Node* base = node->children[0];
        Node* mem = node->children[1];
        if (!mem || mem->name != "IDENTIFIER") return "";
        // Base is a pointer to a struct/union; get its pointed-to type
        string baseType = resolveStructTypeFromLValue(base);
        if (baseType.empty()) return "";
        
        // ✅ FIX: Strip pointer from baseType for chained access
        // When we have ptr->member where member is itself a pointer,
        // we need to strip one level of pointer indirection
        string tag = baseType;
        
        // Remove trailing * for pointer types
        size_t lastStar = tag.find_last_of('*');
        if (lastStar != string::npos) {
            tag = tag.substr(0, lastStar);
            // Trim trailing spaces
            while (!tag.empty() && tag.back() == ' ') tag.pop_back();
        }
        
        // Now strip struct/union prefix
        if (tag.rfind("struct ", 0) == 0) tag = tag.substr(7);
        else if (tag.rfind("union ", 0) == 0) tag = tag.substr(6);
        
        Symbol* structSym = symbolTable.lookuph(tag);
        if (!structSym) return "";
        auto it = structSym->structMembers.find(mem->lexeme);
        if (it == structSym->structMembers.end()) return "";
        return it->second;
    }
    
    // Array element of struct type: arr[i]
    if (node->name == "ARRAY_ACCESS") {
        // Find base expression
        Node* base = node;
        while (base && base->name == "ARRAY_ACCESS") {
            if (base->children.empty()) break;
            base = base->children[0];
        }
        if (base) {
            if (base->name == "IDENTIFIER") {
                Symbol* sym = base->symbol ? base->symbol : symbolTable.lookuph(base->lexeme);
                if (sym) return sym->type; // Element type
            } else if (base->name == "MEMBER_ACCESS" || base->name == "PTR_MEMBER_ACCESS") {
                // Resolve member type, then strip one array dimension if present
                if (base->children.size() >= 2 && base->children[1]->name == "IDENTIFIER") {
                    string baseStructType = resolveStructTypeFromLValue(base->children[0]);
                    if (!baseStructType.empty()) {
                        string tag = baseStructType;
                        if (tag.rfind("struct ", 0) == 0) tag = tag.substr(7);
                        else if (tag.rfind("union ", 0) == 0) tag = tag.substr(6);
                        if (Symbol* s = symbolTable.lookuph(tag)) {
                            auto it = s->structMembers.find(base->children[1]->lexeme);
                            if (it != s->structMembers.end()) {
                                string mType = it->second;
                                // Strip one array dimension
                                size_t lb = mType.find('[');
                                if (lb != string::npos) {
                                    return mType.substr(0, lb);
                                }
                                return mType;
                            }
                        }
                    }
                }
            }
        }
    }
    
    return "";
}

int IRGenerator::getMemberAlignment(const string& typeName) {
    static const bool kIrDebug = false;
    if (kIrDebug) cout << "DEBUG align: type='" << typeName << "'" << endl;
    // Pointers align to 8 (spellings with * or typedef pointers)
    if (typeName.find("*") != string::npos) return 4;
    // Arrays: alignment is alignment of element type
    auto lb = typeName.find('[');
    if (lb != string::npos) {
        string base = typeName.substr(0, lb);
    int a = getMemberAlignment(base);
    if (kIrDebug) cout << " -> align array base '" << base << "' = " << a << endl;
        return a;
    }
    if (!typeName.empty()) {
        Symbol* tSym = symbolTable.lookuph(typeName);
        if (tSym && tSym->isTypedef && tSym->pointerDepth > 0) return 8;
    }

    // Base type alignments (simplified - match size for basic types)
    if (typeName == "char" || typeName == "bool") { if (kIrDebug) cout << " -> align 1" << endl; return 1; }
    if (typeName == "short") { if (kIrDebug) cout << " -> align 2" << endl; return 2; }
    if (typeName == "int" || typeName == "float") { if (kIrDebug) cout << " -> align 4" << endl; return 4; }
    if (typeName == "long" || typeName == "double") { if (kIrDebug) cout << " -> align 8" << endl; return 8; }

    // For struct/union or typedefs to them, use max member alignment
    string baseType = typeName;
    if (baseType.rfind("struct ", 0) == 0) baseType = baseType.substr(7);
    else if (baseType.rfind("union ", 0) == 0) baseType = baseType.substr(6);

    Symbol* typeSym = symbolTable.lookuph(baseType);
    // If it's a typedef, resolve to underlying aliased type/tag
    if (typeSym && typeSym->isTypedef) {
        if (typeSym->pointerDepth > 0) return 4;
        baseType = typeSym->aliasedType.empty() ? typeSym->type : typeSym->aliasedType;
        if (baseType.rfind("struct ", 0) == 0) baseType = baseType.substr(7);
        else if (baseType.rfind("union ", 0) == 0) baseType = baseType.substr(6);
        typeSym = symbolTable.lookuph(baseType);
    }

    if (typeSym && (typeSym->isStruct || typeSym->isUnion)) {
        int maxAlign = 1;
        for (const auto& member : typeSym->structMembers) {
            int memberAlign = getMemberAlignment(member.second);
            maxAlign = max(maxAlign, memberAlign);
        }
        if (kIrDebug) cout << " -> align struct/union tag '" << baseType << "' = " << maxAlign << endl;
        return maxAlign;
    }
    if (kIrDebug) cout << " -> default align 4" << endl;
    return 4;  // Default alignment
}

int IRGenerator::getStructSize(Symbol* structSym) {
    if (!structSym) return 0;
    
    // For unions, size is max of all members
    if (structSym->isUnion) {
        int maxSize = 0;
        for (const auto& member : structSym->structMembers) {
            int memberSize = getBaseTypeSize(member.second);
            maxSize = max(maxSize, memberSize);
        }
        // Align to largest member's alignment
        int maxAlign = 1;
        for (const auto& member : structSym->structMembers) {
            int memberAlign = getMemberAlignment(member.second);
            maxAlign = max(maxAlign, memberAlign);
        }
        if (maxSize % maxAlign != 0) {
            maxSize += maxAlign - (maxSize % maxAlign);
        }
        return maxSize;
    }
    
    // For structs, sum with alignment padding (preserve declaration order)
    int totalSize = 0;
    int maxAlign = 1;
    
    std::vector<std::pair<std::string, std::string>> orderedMembers;
    if (!structSym->structMemberOrder.empty()) {
        std::set<std::string> seen;
        for (const auto& name : structSym->structMemberOrder) {
            if (seen.count(name)) continue;
            auto it = structSym->structMembers.find(name);
            if (it != structSym->structMembers.end()) {
                orderedMembers.emplace_back(it->first, it->second);
                seen.insert(name);
            }
        }
    } else {
        for (const auto& kv : structSym->structMembers) {
            orderedMembers.emplace_back(kv.first, kv.second);
        }
    }

    for (const auto& kv : orderedMembers) {
        const std::string& memType = kv.second;
        int memberSize = getBaseTypeSize(memType);
        int memberAlign = getMemberAlignment(memType);
        
        maxAlign = max(maxAlign, memberAlign);
        
        // Add padding before this member
        if (totalSize % memberAlign != 0) {
            totalSize += memberAlign - (totalSize % memberAlign);
        }
        
        totalSize += memberSize;
    }
    
    // Add padding at end to align whole struct
    if (totalSize % maxAlign != 0) {
        totalSize += maxAlign - (totalSize % maxAlign);
    }
    
    return totalSize;
}


string IRGenerator::generateMultiDimArrayAddress(Node* arrayNode) {
    if (!arrayNode || arrayNode->children.size() < 2) return "";
    if (kIrDebug) cout << "DEBUG: generateMultiDimArrayAddress" << endl;
    
    Node* arrayBase = arrayNode->children[0];
    Node* indexExprNode = arrayNode->children[1];

    // ✅ Unwrap EXPR_LIST if present
    Node* indexExpr = indexExprNode;
    if (indexExprNode->name == "EXPR_LIST" && !indexExprNode->children.empty()) {
        indexExpr = indexExprNode->children[0];
    }
    
    string baseAddr;
    
    // Recursively handle nested array accesses: arr[i][j][k]...
    if (arrayBase->name == "ARRAY_ACCESS") {
        // Recurse to get address of sub-array
        baseAddr = generateMultiDimArrayAddress(arrayBase);
    } else if (arrayBase->name == "IDENTIFIER") {
        // Base case: get address of the array variable
        string arrayName = getUniqueNameFor(arrayBase, arrayBase->lexeme);
        Symbol* sym = arrayBase->symbol;
        
        // ✅ FIX: Decayed array parameters have both isArray=true and pointerDepth>0
        // They are already pointers, so use their value, not address
        if (sym && sym->isArray && sym->pointerDepth > 0 && sym->isReference) {
            // Decayed array parameter: use the pointer value directly
            baseAddr = arrayName;
            if (kIrDebug) cout << "DEBUG: Multidim access on decayed array parameter, using pointer value" << endl;
        } else if (sym && sym->isReference) {
            // Reference-to-array parameter: already holds the base address
            baseAddr = arrayName;
            if (kIrDebug) cout << "DEBUG: Multidim access on reference array, using address value" << endl;
        } else if (sym && sym->pointerDepth > 0 && !sym->isArray) {
            // ✅ FIX: Pointer-to-array: int (*p)[4]
            // The pointer variable holds the address value, use it directly
            baseAddr = arrayName;
            if (kIrDebug) cout << "DEBUG: Multidim access on pointer-to-array, using pointer value" << endl;
        } else {
            // Regular array variable: take its address
            baseAddr = createTemp();
            instructions.emplace_back(TACOp::ADDRESS, baseAddr, arrayName);
            if (kIrDebug) cout << "DEBUG: Multidim access on regular array, taking ADDRESS of " << arrayName << endl;
        }
    } else {
        return "";
    }
    
    // Calculate size to multiply by for THIS dimension
    // We need to know which dimension we're at
    int dimIndex = getDimensionIndex(arrayNode);
    int subArraySize = getSubArraySizeForDimension(arrayBase, dimIndex);
    
    if (kIrDebug) cout << "DEBUG: Dimension index = " << dimIndex << ", sub-array size = " << subArraySize << endl;
    
    // Generate: offset = index * subArraySize
    string indexTemp = generateExpression(indexExpr);
    string sizeTemp = createTemp();
    string offsetTemp = createTemp();
    string resultAddr = createTemp();
    
    instructions.emplace_back(TACOp::CONST, sizeTemp, to_string(subArraySize));
    instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, sizeTemp);
    instructions.emplace_back(TACOp::ADD, resultAddr, baseAddr, offsetTemp);
    
    return resultAddr;
}

// Helper to determine which dimension index we're at
int IRGenerator::getDimensionIndex(Node* arrayAccessNode) {
    // Count how many ARRAY_ACCESS nodes are below this one
    int depth = 0;
    Node* temp = arrayAccessNode;
    
    while (temp && temp->name == "ARRAY_ACCESS") {
        if (temp->children.empty()) break;
        temp = temp->children[0];
        if (temp && temp->name == "ARRAY_ACCESS") {
            depth++;
        }
    }
    
    // depth tells us how many more array accesses are below
    // For matrix[1][2]:
    //   - Outer [2]: depth = 1 (one ARRAY_ACCESS below) → dimension index 1
    //   - Inner [1]: depth = 0 (no ARRAY_ACCESS below) → dimension index 0
    
    return depth;
}

int IRGenerator::getSubArraySizeForDimension(Node* arrayNode, int dimIndex) {
    if (kIrDebug) cout << "=== DEBUG getSubArraySizeForDimension, dimIndex = " << dimIndex << " ===" << endl;
    
    // Find base identifier
    Node* base = arrayNode;
    while (base && base->name == "ARRAY_ACCESS") {
        if (base->children.empty()) break;
        base = base->children[0];
    }
    
    if (!base || base->name != "IDENTIFIER") return 4;
    
    Symbol* sym = base->symbol;
    
    // ✅ FIX: Handle pointer-to-array: int (*p)[4]
    // For pointer-to-array, use pointeeArrayDimensions instead of arrayDimensions
    if (sym && !sym->isArray && sym->pointerDepth > 0 && !sym->pointeeArrayDimensions.empty()) {
        vector<int> dimensions = sym->pointeeArrayDimensions;
        int elemSize = getBaseTypeSize(sym->type);
        
        if (kIrDebug) {
            cout << "DEBUG: Pointer-to-array " << base->lexeme << " has " << dimensions.size() << " pointee dimensions: ";
            for (int d : dimensions) cout << d << " ";
            cout << endl;
            cout << "DEBUG: Base element size = " << elemSize << endl;
            cout << "DEBUG: Indexing dimension " << dimIndex << endl;
        }
        
        // For pointer-to-array, dimIndex 0 means the first dereference
        // int (*p)[4][5] with p[0][1][2]:
        //   - dimIndex 0 (p[0]): size = 4*5*elemSize = 80
        //   - dimIndex 1 ([1]): size = 5*elemSize = 20
        //   - dimIndex 2 ([2]): size = elemSize = 4
        int subArraySize = elemSize;
        for (size_t i = dimIndex; i < dimensions.size(); i++) {
            if (kIrDebug) cout << "DEBUG: Multiplying by pointee dimensions[" << i << "] = " << dimensions[i] << endl;
            subArraySize *= dimensions[i];
        }
        
        if (kIrDebug) cout << "DEBUG: Final sub-array size (pointer-to-array) = " << subArraySize << endl;
        return subArraySize;
    }
    
    if (!sym || !sym->isArray) return 4;
    
    vector<int> dimensions = sym->arrayDimensions;
    // ✅ FIX: For decayed array parameters (isArray=true, pointerDepth>0),
    // the pointerDepth refers to the pointer-to-array itself, NOT the element type.
    // Example: int a[3][3] decays to int(*)[3] with pointerDepth=1, arrayDims=[3]
    // The element is still int (4 bytes), not a pointer.
    int elemSize;
    if (sym->isArray && sym->pointerDepth > 0) {
        // Decayed array parameter: element type is still the base type
        elemSize = getBaseTypeSize(sym->type);
    } else if (sym->pointerDepth > 0) {
        // True pointer array: elements are pointers (4 bytes on MIPS32)
        elemSize = 4;
    } else {
        elemSize = getBaseTypeSize(sym->type);
    }
    
    if (kIrDebug) {
        cout << "DEBUG: Array " << base->lexeme << " has " << dimensions.size() << " dimensions: ";
        for (int d : dimensions) cout << d << " ";
        cout << endl;
        cout << "DEBUG: Base element size = " << elemSize << endl;
        cout << "DEBUG: Indexing dimension " << dimIndex << endl;
        cout << "DEBUG: Is decayed parameter: " << (sym->pointerDepth > 0 ? "yes" : "no") << endl;
    }
    
    // ✅ FIX: For decayed array parameters (e.g., int a[3][3] -> int(*)[3]),
    // arrayDims=[3] represents the INNER dimensions that should be multiplied
    // For the FIRST access (dimIndex=0), we should multiply by ALL remaining dims
    // For a regular array int arr[3][4]:
    //   - dimIndex 0: multiply by dimensions[1..end] = [4] → size = 4 * 4 = 16
    //   - dimIndex 1: multiply by dimensions[2..end] = [] → size = 4
    // For a decayed parameter int(*)[3] with arrayDims=[3]:
    //   - dimIndex 0: multiply by dimensions[0..end] = [3] → size = 4 * 3 = 12
    //   - dimIndex 1: multiply by dimensions[1..end] = [] → size = 4
    
    int subArraySize = elemSize;
    size_t startDim = (sym->pointerDepth > 0 && sym->isArray) ? dimIndex : (dimIndex + 1);
    
    for (size_t i = startDim; i < dimensions.size(); i++) {
        if (kIrDebug) cout << "DEBUG: Multiplying by dimensions[" << i << "] = " << dimensions[i] << endl;
        subArraySize *= dimensions[i];
    }
    
    if (kIrDebug) cout << "DEBUG: Final sub-array size = " << subArraySize << endl;
    return subArraySize;
}

int IRGenerator::getInnerElementSize(Node* arrayNode) {
    // Find the base array identifier
    Node* base = arrayNode;
    while (base && base->name == "ARRAY_ACCESS") {
        if (base->children.empty()) break;
        base = base->children[0];
    }
    
    if (!base || base->name != "IDENTIFIER") return 4;
    
    Symbol* sym = base->symbol;
    if (!sym) return 4;
    // We are computing the size for the NEXT index after having already indexed once
    // into the array in generateMultiDimArrayAddress. Therefore, regardless of whether
    // the array's element type is a pointer (e.g., char* arr[]), the "inner element"
    // accessed by the subsequent index is the base type that the pointer points to
    // (char in the example) or simply the array's base scalar type for true multi-d arrays.
    // Hence, always return the base type size here.
    return getBaseTypeSize(sym->type);
}

vector<int> IRGenerator::getArrayDimensions(Symbol* arraySym) {
    if (!arraySym || !arraySym->isArray) {
        return vector<int>();
    }
    return arraySym->arrayDimensions;
}

// Helper to check if a node represents a pointer type
bool IRGenerator::isPointerType(Node* node) {
    if (!node) return false;
    
    if (node->name == "IDENTIFIER") {
        Symbol* sym = node->symbol;
        if (sym) {
            if (sym->isArray) return true;  // Array name decays to pointer
            return sym->pointerDepth > 0;
        }
    }
    // String literals decay to char* in expressions
    if (node->name == "STRING_LITERAL") return true;
    
    // If it's an expression, try to infer type
    if (node->name == "UNARY_OP" && node->children.size() >= 2) {
        Node* op = node->children[0];
        string opStr = op->name.empty() ? op->lexeme : op->name;
        
        if (opStr == "&") {
            return true;  // Address-of always returns pointer
        }
        if (opStr == "*") {
            // Dereference reduces pointer level
            return isPointerType(node->children[1]) && getPointerDepth(node->children[1]) > 1;
        }
    }
    
    return false;
}

// Overloaded version to check if a temp variable is a pointer
bool IRGenerator::isTempPointer(const string& temp) {
    if (tempPointerDepth.find(temp) != tempPointerDepth.end()) {
        return tempPointerDepth[temp] > 0;
    }
    return false;
}

int IRGenerator::getPointerDepth(Node* node) {
    if (!node) return 0;
    
    if (node->name == "IDENTIFIER") {
        Symbol* sym = node->symbol;
        if (sym) {
            if (sym->isArray) return 1;  // Array name is like pointer
            return sym->pointerDepth;
        }
    }
    if (node->name == "STRING_LITERAL") return 1;
    
    return 0;
}
int IRGenerator::getPointedToSize(Node* ptrNode) {
    if (!ptrNode) return 4;
    // String literals behave like pointer to char
    if (ptrNode->name == "STRING_LITERAL") return 1;
    
    // Handle array access expressions: result may be a pointer-valued element (e.g., arr[i] where arr is array of pointers)
    if (ptrNode->name == "ARRAY_ACCESS" && !ptrNode->children.empty()) {
        // Count access levels and find base
        int levels = 1;
        Node* base = ptrNode->children[0];
        while (base && base->name == "ARRAY_ACCESS") {
            levels++;
            if (base->children.empty()) break;
            base = base->children[0];
        }
        if (base && base->name == "IDENTIFIER") {
            Symbol* sym = base->symbol ? base->symbol : symbolTable.lookuph(base->lexeme);
            if (sym) {
                const vector<int>& dims = sym->arrayDimensions;
                int elemPtrDepth = sym->pointerDepth;
                string elemTypeStr = sym->type;
                // If indexing exactly to the element and the element is a pointer, return size of what it points to
                if (levels == (int)dims.size()) {
                    if (elemPtrDepth > 1) return 4; // still pointer after one deref
                    if (elemPtrDepth == 1) {
                        // Compute base type by stripping one '*'
                        string baseType = elemTypeStr;
                        size_t star = baseType.find('*');
                        if (star != string::npos) {
                            baseType.erase(star, 1);
                        }
                        // Trim any residual spaces
                        while (!baseType.empty() && baseType.back() == ' ') baseType.pop_back();
                        while (!baseType.empty() && baseType.front() == ' ') baseType.erase(baseType.begin());
                        return getBaseTypeSize(baseType);
                    }
                    // Element is not a pointer; pointed-to size of a non-pointer is its own base size
                    return getBaseTypeSize(elemTypeStr);
                }
                // If indexing beyond the array dimensions, consume pointer levels from the element type
                if (levels > (int)dims.size()) {
                    int extra = levels - (int)dims.size();
                    int remainingPtr = max(0, elemPtrDepth - extra);
                    if (remainingPtr > 0) return 8; // still a pointer
                    // Fully dereferenced to base type
                    return getBaseTypeSize(elemTypeStr);
                }
                // If still within array (subarray result), the decay-to-pointer would point to its element
                // In that case, the pointed-to size is the size of one element of that subarray
                // which is either pointer-sized (if element is pointer) or the base element size.
                if (elemPtrDepth > 0) return 4; // subarray of pointers
                return getBaseTypeSize(elemTypeStr);
            }
        }
    }

    if (ptrNode->name == "IDENTIFIER") {
        // Prefer attached symbol; fall back to lookup by name if missing
        Symbol* sym = ptrNode->symbol ? ptrNode->symbol : symbolTable.lookuph(ptrNode->lexeme);
        if (sym) {
            cout << "DEBUG: getPointedToSize for " << sym->name 
                 << " pointerDepth=" << sym->pointerDepth << endl;
            
            // Arrays: element size; arrays-of-pointers are pointer-sized
            if (sym->isArray) {
                if (sym->pointerDepth > 0) {
                    cout << "DEBUG: Array of pointers, returning 4 (MIPS32)" << endl;
                    return 4;
                }
                // If multi-dimensional array, the element is the subarray of remaining dims
                int elemSize = getBaseTypeSize(sym->type);
                if (!sym->arrayDimensions.empty()) {
                    // Multiply by all dims except the first
                    for (size_t i = 1; i < sym->arrayDimensions.size(); ++i) {
                        int dim = sym->arrayDimensions[i];
                        if (dim > 0) elemSize *= dim;
                    }
                }
                cout << "DEBUG: Array element size (considering remaining dims) = " << elemSize << endl;
                return elemSize;
            }
            // Pointer-to-array: scale by full pointee aggregate size
            if (sym->pointerDepth >= 1 && !sym->pointeeArrayDimensions.empty()) {
                long long agg = getBaseTypeSize(sym->type);
                for (int d : sym->pointeeArrayDimensions) {
                    if (d > 0) agg *= d;
                }
                // For multi-level pointers, pointer arithmetic on the outer pointer uses pointer size
                if (sym->pointerDepth > 1) {
                    cout << "DEBUG: Multi-level pointer to array, returning pointer size 4 (MIPS32)" << endl;
                    return 4;
                }
                cout << "DEBUG: Pointer-to-array element size (full aggregate) = " << agg << endl;
                return static_cast<int>(agg);
            }
            // Pointers: if depth > 1, still a pointer (4); if depth == 1, base type size
            if (sym->pointerDepth > 1) {
                cout << "DEBUG: Multi-level pointer, returning 4 (MIPS32)" << endl;
                return 4;
            } else if (sym->pointerDepth == 1) {
                int size = getBaseTypeSize(sym->type);
                cout << "DEBUG: Single-level pointer, base type size = " << size << endl;
                return size;
            }
        }
    }
    // Member access: if the member is an array, return its element size
    if ((ptrNode->name == "MEMBER_ACCESS" || ptrNode->name == "PTR_MEMBER_ACCESS") && ptrNode->children.size() >= 2) {
        Node* base = ptrNode->children[0];
        Node* memId = ptrNode->children[1];
        if (memId && memId->name == "IDENTIFIER") {
            // Resolve base struct type
            string baseType = resolveStructTypeFromLValue(base);
            if (!baseType.empty()) {
                string tag = baseType;
                if (tag.rfind("struct ", 0) == 0) tag = tag.substr(7);
                else if (tag.rfind("union ", 0) == 0) tag = tag.substr(6);
                if (Symbol* s = symbolTable.lookuph(tag)) {
                    auto it = s->structMembers.find(memId->lexeme);
                    if (it != s->structMembers.end()) {
                        const string& mType = it->second;
                        size_t lb2 = mType.find('[');
                        if (lb2 != string::npos) {
                            string elemBase = mType.substr(0, lb2);
                            return getBaseTypeSize(elemBase);
                        }
                        // Pointer member: pointed-to base
                        if (mType.find('*') != string::npos) {
                            return getBaseTypeSize(mType);
                        }
                        // Otherwise, treat as scalar
                        return getBaseTypeSize(mType);
                    }
                }
            }
        }
    }
    
    // For dereference operations, reduce pointer level
    if (ptrNode->name == "UNARY_OP" && ptrNode->children.size() >= 2) {
        Node* op = ptrNode->children[0];
        string opStr = op->name.empty() ? op->lexeme : op->name;
        
        if (opStr == "*") {
            // After one dereference, check the reduced pointer depth
            Node* operand = ptrNode->children[1];
            
            // Recursively get the size of what we're pointing to after dereference
            if (operand->name == "IDENTIFIER") {
                Symbol* sym = operand->symbol;
                if (sym) {
                    cout << "DEBUG: Dereferencing " << sym->name 
                         << " (depth=" << sym->pointerDepth << ")" << endl;
                    
                    // After dereferencing, pointer depth reduces by 1
                    // int*** -> int** (still pointer, size 8)
                    // int** -> int* (still pointer, size 8)
                    // int* -> int (base type, size 4)
                    if (sym->pointerDepth > 2) {
                        cout << "DEBUG: After deref, still multi-level pointer, returning 8" << endl;
                        return 4;  // Still a pointer
                    } else if (sym->pointerDepth == 2) {
                        cout << "DEBUG: After deref, becomes single pointer, returning 8" << endl;
                        return 4;  // Becomes int* which is still a pointer
                    } else if (sym->pointerDepth == 1) {
                        int size = getBaseTypeSize(sym->type);
                        cout << "DEBUG: After deref, becomes base type, returning " << size << endl;
                        return size;  // Becomes base type
                    }
                }
            }
            // Recursively handle nested dereferences like **pp
            return getPointedToSize(operand);
        } else if (opStr == "&") {
            // Address-of: pointer points to the operand's type
            Node* operand = ptrNode->children[1];
            int size = getSizeofExpression(operand);
            cout << "DEBUG: Address-of operand size = " << size << endl;
            return size;
        }
    }
    
    // Pointer arithmetic nodes: use pointed-to size of the pointer operand
    if (ptrNode->name == "ADD_EXPR" || ptrNode->name == "SUB_EXPR") {
        Node* left = (ptrNode->children.size() >= 1) ? ptrNode->children[0] : nullptr;
        Node* right = (ptrNode->children.size() >= 2) ? ptrNode->children[1] : nullptr;
        if (ptrNode->name == "SUB_EXPR") {
            if (left) return getPointedToSize(left);
        } else {
            if (left && isPointerType(left)) return getPointedToSize(left);
            if (right && isPointerType(right)) return getPointedToSize(right);
        }
    }
    
    return 4;  // Default to int size
}


string IRGenerator::generatePointerArithmetic(Node* ptrNode, const string& ptrTemp, 
                                               const string& offsetTemp, TACOp op) {
    int elemSize = getPointedToSize(ptrNode);
    
    cout << "DEBUG: Pointer arithmetic - element size = " << elemSize << endl;
    
    // Scale offset: scaledOffset = offset * elemSize
    string elemSizeTemp = createTemp();
    string scaledOffsetTemp = createTemp();
    string resultTemp = createTemp();
    
    instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
    instructions.emplace_back(TACOp::MUL, scaledOffsetTemp, offsetTemp, elemSizeTemp);
    
    // Add or subtract scaled offset
    instructions.emplace_back(op, resultTemp, ptrTemp, scaledOffsetTemp);
    
    // Mark result as a pointer with same depth as input
    int ptrDepth = 0;
    if (ptrNode && isPointerType(ptrNode)) {
        ptrDepth = getPointerDepth(ptrNode);
    } else if (isTempPointer(ptrTemp)) {
        ptrDepth = tempPointerDepth[ptrTemp];
    }
    if (ptrDepth > 0) {
        tempPointerDepth[resultTemp] = ptrDepth;
    }
    
    return resultTemp;
}

string IRGenerator::generatePointerDifference(Node* ptr1Node, Node* ptr2Node,
                                               const string& ptr1Temp, const string& ptr2Temp) {
    (void)ptr2Node; // suppress unused parameter warning
    int elemSize = getPointedToSize(ptr1Node);
    
    cout << "DEBUG: Pointer difference - element size = " << elemSize << endl;
    
    // byteDiff = ptr1 - ptr2
    string byteDiffTemp = createTemp();
    instructions.emplace_back(TACOp::SUB, byteDiffTemp, ptr1Temp, ptr2Temp);
    
    // elemDiff = byteDiff / elemSize
    string elemSizeTemp = createTemp();
    string resultTemp = createTemp();
    
    instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
    instructions.emplace_back(TACOp::DIV, resultTemp, byteDiffTemp, elemSizeTemp);
    
    return resultTemp;
}


// Add to ir_generator.cpp
string IRGenerator::generateArrayAddress(Node* arrayNode) {
    if (!arrayNode || arrayNode->children.size() < 2) return "";
    
    Node* arrayBase = arrayNode->children[0];
    // Unwrap EXPR_LIST for base if present
    Node* baseExpr = arrayBase;
    if (baseExpr && baseExpr->name == "EXPR_LIST" && !baseExpr->children.empty()) {
        baseExpr = baseExpr->children[0];
    }
    Node* indexExpr = arrayNode->children[1];
    
    // Unwrap EXPR_LIST
    Node* actualIndexExpr = indexExpr;
    if (indexExpr->name == "EXPR_LIST" && !indexExpr->children.empty()) {
        actualIndexExpr = indexExpr->children[0];
    }
    
    // ✅ CRITICAL FIX: Handle nested array access (2D/3D arrays)
    // For &array[i][j], we need to recursively get the address without loading
    if (baseExpr->name == "ARRAY_ACCESS") {
        // Use generateMultiDimArrayAddress which handles multi-dimensional properly
        string subArrayAddr = generateMultiDimArrayAddress(baseExpr);
        string indexTemp = generateExpression(actualIndexExpr);
        
        // Calculate element size for final dimension
        int elemSize = getInnerElementSize(baseExpr);
        
        string elemSizeTemp = createTemp();
        string offsetTemp = createTemp();
        string elemAddr = createTemp();
        
        instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
        instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, elemSizeTemp);
        instructions.emplace_back(TACOp::ADD, elemAddr, subArrayAddr, offsetTemp);
        
        return elemAddr;
    }
    
   
    
    string indexTemp = generateExpression(actualIndexExpr);

    // Get base address
string baseAddr = createTemp();

    if (baseExpr->name == "IDENTIFIER") {
        Symbol* sym = baseExpr->symbol;
        string baseName = getUniqueNameFor(baseExpr, baseExpr->lexeme);
    if (sym && sym->isArray) {
        // Array variable: &array
            instructions.emplace_back(TACOp::ADDRESS, baseAddr, baseName);
        cout << "DEBUG: Array address of array IDENTIFIER" << endl;
    } else if (sym && sym->pointerDepth > 0) {
        // Pointer variable: value is already an address
            instructions.emplace_back(TACOp::ASSIGN, baseAddr, baseName);
        cout << "DEBUG: Array address of pointer IDENTIFIER (using value)" << endl;
    } else {
            instructions.emplace_back(TACOp::ADDRESS, baseAddr, baseName);
        cout << "DEBUG: Array address of IDENTIFIER (default ADDRESS)" << endl;
    }
}
    else if (expressionReturnsAddress(baseExpr)) {
        string exprTemp = generateExpression(baseExpr);
    instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
    cout << "DEBUG: Expression already returns address" << endl;
}
else {
        string exprTemp = generateExpression(baseExpr);
    // Expression result should already be a pointer value; use it directly
    instructions.emplace_back(TACOp::ASSIGN, baseAddr, exprTemp);
    cout << "DEBUG: Using expression result as base address" << endl;
}


    // Calculate element size
    int elemSize = 4;
    if (baseExpr->name == "IDENTIFIER" && baseExpr->symbol) {
        Symbol* sym = baseExpr->symbol;
        if (sym->isArray) {
            elemSize = (sym->pointerDepth > 0) ? 8 : getBaseTypeSize(sym->type);
        } else if (sym->pointerDepth > 0) {
            elemSize = getBaseTypeSize(sym->type);
        } else {
            elemSize = getBaseTypeSize(sym->type);
        }
    } else {
        // Non-identifier base expression: use pointed-to size
        elemSize = getPointedToSize(baseExpr);
        if ((baseExpr->name == "ADD_EXPR" || baseExpr->name == "SUB_EXPR") && baseExpr->children.size() >= 1) {
            Node* left = baseExpr->children[0];
            Node* right = (baseExpr->children.size() >= 2) ? baseExpr->children[1] : nullptr;
            if (left && left->name == "IDENTIFIER" && left->symbol && left->symbol->isArray && left->symbol->pointerDepth > 0) {
                elemSize = 4;
            } else if (right && right->name == "IDENTIFIER" && right->symbol && right->symbol->isArray && right->symbol->pointerDepth > 0) {
                elemSize = 4;
            }
        }
    }
    
    // Calculate address: baseAddr + (index * elemSize)
    string elemSizeTemp = createTemp();
    string offsetTemp = createTemp();
    string elemAddr = createTemp();
    
    instructions.emplace_back(TACOp::CONST, elemSizeTemp, to_string(elemSize));
    instructions.emplace_back(TACOp::MUL, offsetTemp, indexTemp, elemSizeTemp);
    instructions.emplace_back(TACOp::ADD, elemAddr, baseAddr, offsetTemp);
    
    return elemAddr;  // Return address WITHOUT loading
}

void IRGenerator::generatePrintfStatement(Node* node) {
    if (!node) return;
    
    cout << "DEBUG: Generating printf statement (varargs function)" << endl;
    
    Node* formatString = nullptr;
    Node* argList = nullptr;
    
    for (auto child : node->children) {
        if (child->name == "STRING_LITERAL") {
            formatString = child;
        } else if (child->name == "ARG_LIST") {
            argList = child;
        }
    }
    
    // ✅ Step 1: Evaluate ALL arguments first (left-to-right)
    vector<string> evaluatedArgs;
    
    // Format string is always first
    if (formatString) {
        string formatTemp = createTemp();
        instructions.emplace_back(TACOp::CONST, formatTemp, formatString->lexeme);
        evaluatedArgs.push_back(formatTemp);
    }
    
    // Evaluate remaining arguments (left-to-right)
    if (argList) {
        for (size_t i = 0; i < argList->children.size(); i++) {
            Node* arg = argList->children[i];
            
            cout << "DEBUG: Evaluating printf argument " << i << endl;
            
            string argTemp = generateExpression(arg);
            
            if (!argTemp.empty()) {
                // Preserve variables in temps to avoid side-effect issues
                bool isTemp = (argTemp[0] == 't' && isdigit(argTemp[1]));
                bool isConst = (argTemp[0] == '"' || isdigit(argTemp[0]) || 
                               (argTemp[0] == '-' && isdigit(argTemp[1])));
                
                if (!isTemp && !isConst) {
                    string preservedTemp = createTemp();
                    instructions.emplace_back(TACOp::ASSIGN, preservedTemp, argTemp);
                    evaluatedArgs.push_back(preservedTemp);
                } else {
                    evaluatedArgs.push_back(argTemp);
                }
            }
        }
    }
    
    // ✅ Step 2: Emit PARAM instructions in order
    for (const auto& arg : evaluatedArgs) {
        instructions.emplace_back(TACOp::PARAM, "", arg);
    }
    
    // ✅ Step 3: Emit CALL
    string resultTemp = createTemp();
    int paramCount = evaluatedArgs.size();  // Total count: format string + varargs
    instructions.emplace_back(TACOp::CALL, resultTemp, "printf", "", paramCount);
    
    cout << "DEBUG: Generated printf with " << evaluatedArgs.size() 
         << " parameters" << endl;
}

void IRGenerator::generateScanfStatement(Node* node) {
    if (!node) return;
    
    cout << "DEBUG: Generating scanf statement (varargs function)" << endl;
    
    Node* formatString = nullptr;
    Node* argList = nullptr;
    
    for (auto child : node->children) {
        if (child->name == "STRING_LITERAL") {
            formatString = child;
        } else if (child->name == "ARG_LIST") {
            argList = child;
        }
    }
    
    // ✅ Step 1: Evaluate ALL arguments first (left-to-right)
    vector<string> evaluatedArgs;
    
    // Format string is always first
    if (formatString) {
        string formatTemp = createTemp();
        instructions.emplace_back(TACOp::CONST, formatTemp, formatString->lexeme);
        evaluatedArgs.push_back(formatTemp);
    }
    
    // Evaluate address arguments (left-to-right)
    // Note: scanf takes addresses, typically &var
    if (argList) {
        for (size_t i = 0; i < argList->children.size(); i++) {
            Node* arg = argList->children[i];
            
            cout << "DEBUG: Evaluating scanf argument " << i 
                 << " (should be address)" << endl;
            
            // Generate expression - for scanf, these should be address-of expressions
            string argTemp = generateExpression(arg);
            
            if (!argTemp.empty()) {
                // For scanf, arguments are already addresses from &x
                // No need to preserve since addresses don't have side effects
                evaluatedArgs.push_back(argTemp);
            }
        }
    }
    
    // ✅ Step 2: Emit PARAM instructions in order
    for (const auto& arg : evaluatedArgs) {
        instructions.emplace_back(TACOp::PARAM, "", arg);
    }
    
    // ✅ Step 3: Emit CALL
    string resultTemp = createTemp();
    int paramCount = evaluatedArgs.size();  // Total count: format string + varargs
    instructions.emplace_back(TACOp::CALL, resultTemp, "scanf", "", paramCount);
    
    cout << "DEBUG: Generated scanf with " << evaluatedArgs.size() 
         << " parameters" << endl;
}

bool IRGenerator::expressionReturnsAddress(Node* node) {
    if (!node) return false;
    
    // Function calls returning pointers
    if (node->name == "FUNC_CALL") {
        if (!node->children.empty() && node->children[0]->name == "IDENTIFIER") {
            Symbol* funcSym = node->children[0]->symbol;
            return funcSym && funcSym->pointerDepth > 0;
        }
    }
    
    // Address-of operator
    if (node->name == "UNARY_OP" && node->children.size() >= 2) {
        Node* op = node->children[0];
        string opStr = op->name.empty() ? op->lexeme : op->name;
        return opStr == "&";
    }
    
    // Pointer arithmetic (ADD/SUB on pointers)
    if (node->name == "ADD_EXPR" || node->name == "SUB_EXPR") {
        if (node->children.size() >= 2) {
            // Result is a pointer if either side is a pointer (for +),
            // or left is a pointer (for -); conservative: if any side is pointer
            if (isPointerType(node->children[0]) || isPointerType(node->children[1])) {
                return true;
            }
        } else if (node->children.size() == 1) {
            return isPointerType(node->children[0]);
        }
    }
    
    // Member access that returns pointer
    if (node->name == "MEMBER_ACCESS" || node->name == "PTR_MEMBER_ACCESS") {
        // Check if member is a pointer type
        // For now, conservatively return false
        return false;
    }
    
    return false;
}

// Helper: compute the address of an lvalue expression
string IRGenerator::generateLValueAddress(Node* lvalue) {
    if (!lvalue) return "";
    
    // ✅ Unwrap EXPR_LIST if present
    if (lvalue->name == "EXPR_LIST" && !lvalue->children.empty()) {
        return generateLValueAddress(lvalue->children[0]);
    }
    
    // Identifier -> &var
    if (lvalue->name == "IDENTIFIER") {
        string varName = getUniqueNameFor(lvalue, lvalue->lexeme);
        // If it's a reference, the variable already holds the address
        if (lvalue->symbol && lvalue->symbol->isReference) {
            return varName;
        }
        string addr = createTemp();
        instructions.emplace_back(TACOp::ADDRESS, addr, varName);
        return addr;
    }
    // Dereference -> pointer expression value is already an address
    if (lvalue->name == "UNARY_OP" && lvalue->children.size() >= 2) {
        Node* op = lvalue->children[0];
        string opStr = op->name.empty() ? op->lexeme : op->name;
        if (opStr == "*") {
            return generateExpression(lvalue->children[1]);
        }
    }
    // Array element -> compute address
    if (lvalue->name == "ARRAY_ACCESS") {
        return generateArrayAddress(lvalue);
    }
    // Struct/union member -> compute address via offset
    if (lvalue->name == "MEMBER_ACCESS" || lvalue->name == "DIRECT_MEMBER" || lvalue->name == "DOT") {
        if (lvalue->children.size() >= 2 && lvalue->children[1]->name == "IDENTIFIER") {
            Node* structNode = lvalue->children[0];
            string memberName = lvalue->children[1]->lexeme;
            int offset = getMemberOffset(structNode, memberName);
            // Compute address of the base struct lvalue (not its value)
            string baseAddr = generateLValueAddress(structNode);
            string offsetTemp = createTemp();
            string memberAddr = createTemp();
            instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
            instructions.emplace_back(TACOp::ADD, memberAddr, baseAddr, offsetTemp);
            return memberAddr;
        }
    }
    if (lvalue->name == "PTR_MEMBER_ACCESS" || lvalue->name == "ARROW") {
        if (lvalue->children.size() >= 2 && lvalue->children[1]->name == "IDENTIFIER") {
            Node* structPtrNode = lvalue->children[0];
            string memberName = lvalue->children[1]->lexeme;
            int offset = getMemberOffset(structPtrNode, memberName);
            string structPtrTemp = generateExpression(structPtrNode);
            string offsetTemp = createTemp();
            string memberAddr = createTemp();
            instructions.emplace_back(TACOp::CONST, offsetTemp, to_string(offset));
            instructions.emplace_back(TACOp::ADD, memberAddr, structPtrTemp, offsetTemp);
            return memberAddr;
        }
    }
    return "";
}

// Helper: determine increment step (1 for scalar, element-size for pointer values)
int IRGenerator::getIncrementStepForLValue(Node* lvalue) {
    if (!lvalue) return 1;
    // Identifier: if the variable itself is a pointer, step by pointed-to size; else 1
    if (lvalue->name == "IDENTIFIER") {
        Symbol* sym = lvalue->symbol;
        // References: treat as underlying scalar for step purposes (1)
        if (sym && sym->isReference) {
            return 1;
        }
        if (sym && sym->pointerDepth > 0) {
            return getBaseTypeSize(sym->type);  // size of what the pointer points to
        }
        return 1;
    }
    // *expr: after deref, value is pointer if expr pointer depth > 1
    if (lvalue->name == "UNARY_OP" && lvalue->children.size() >= 2) {
        Node* op = lvalue->children[0];
        string opStr = op->name.empty() ? op->lexeme : op->name;
        if (opStr == "*") {
            Node* ptrExpr = lvalue->children[1];
            // If after one deref, still a pointer => step is pointer-size (8); else scalar => 1
            int depth = getPointerDepth(ptrExpr);
            if (depth > 1) return 8;
            return 1;
        }
    }
    // arr[i]: if the element is a pointer type (e.g., char*), step by the size of the
    // pointed-to base type (e.g., sizeof(char) = 1) for pointer increments; otherwise 1.
    if (lvalue->name == "ARRAY_ACCESS") {
        // Find base array identifier
        Node* base = lvalue;
        while (base && base->name == "ARRAY_ACCESS") {
            if (base->children.empty()) break;
            base = base->children[0];
        }
        if (base && base->name == "IDENTIFIER" && base->symbol) {
            Symbol* sym = base->symbol; // describes the array
            if (sym->isArray && sym->pointerDepth > 0) {
                // Element is a pointer; step by the size of its base type
                return getBaseTypeSize(sym->type);
            }
        }
        return 1;
    }
    // Member values: conservatively treat as scalar (1). For pointer members, they will be handled via identifier path when accessed directly.
    return 1;
}

bool IRGenerator::isConstantValue(const string& val) {
    if (val.empty()) return false;
    
    // Check if it's directly a CONST instruction result
    for (auto it = instructions.rbegin(); it != instructions.rend(); ++it) {
        if (it->opcode == TACOp::CONST && it->result == val) {
            return true;
        }
    }
    
    // Check if it's a variable assigned from a constant (trace back one level)
    for (auto it = instructions.rbegin(); it != instructions.rend(); ++it) {
        if (it->opcode == TACOp::ASSIGN && it->result == val) {
            // Found assignment: result = operand1
            // Check if operand1 is a constant
            string source = it->operand1;
            
            // Look for CONST instruction that produced this source
            for (auto it2 = instructions.rbegin(); it2 != instructions.rend(); ++it2) {
                if (it2->opcode == TACOp::CONST && it2->result == source) {
                    return true;  // Variable holds a constant value
                }
            }
            
            break;  // Stop at first assignment to this variable
        }
    }
    
    return false;
}

string IRGenerator::getConstantValue(const string& tempName) {
    // Direct CONST lookup
    for (auto it = instructions.rbegin(); it != instructions.rend(); ++it) {
        if (it->opcode == TACOp::CONST && it->result == tempName) {
            return it->operand1;
        }
    }
    
    // Trace through assignment: tempName = source, where source is CONST
    for (auto it = instructions.rbegin(); it != instructions.rend(); ++it) {
        if (it->opcode == TACOp::ASSIGN && it->result == tempName) {
            string source = it->operand1;
            
            // Look up the source's constant value
            for (auto it2 = instructions.rbegin(); it2 != instructions.rend(); ++it2) {
                if (it2->opcode == TACOp::CONST && it2->result == source) {
                    return it2->operand1;  // Return the constant value
                }
            }
            
            break;
        }
    }
    
    return "";
}


// Normalize type name
string IRGenerator::normalizeTypeName(const string& typeName) {
    string normalized = typeName;
    
    size_t pos;
    while ((pos = normalized.find("const ")) != string::npos) {
        normalized.erase(pos, 6);
    }
    while ((pos = normalized.find("volatile ")) != string::npos) {
        normalized.erase(pos, 9);
    }
    
    while (!normalized.empty() && normalized.front() == ' ') {
        normalized.erase(normalized.begin());
    }
    while (!normalized.empty() && normalized.back() == ' ') {
        normalized.pop_back();
    }
    
    return normalized;
}

// Convert constant value from one type to another
// FIX: fromType parameter removed as it was unused
string IRGenerator::convertConstantValue(const string& value, const string& /*fromType*/, const string& toType) {
    if (toType == "int" || toType == "unsigned int") {
        if (value == "false") return "0";
        if (value == "true") return "1";
        if (value.find('.') != string::npos) {
            // Float to int - truncate
            double d = stod(value);
            return to_string((int)d);
        }
        // Already an integer
        return value;
    }
    else if (toType == "bool") {
        if (value == "0" || value == "0.0" || value == "false") {
            return "false";
        }
        return "true";
    }
    else if (toType == "float" || toType == "double") {
        if (value == "false") return "0.0";
        if (value == "true") return "1.0";
        if (value.find('.') == string::npos) {
            // Add decimal point
            return value + ".0";
        }
        // Already a float
        return value;
    }
    else if (toType == "char" || toType == "unsigned char") {
        if (value == "false") return "0";
        if (value == "true") return "1";
        if (value.find('.') != string::npos) {
            double d = stod(value);
            return to_string((int)d % 256);
        } else {
            int v = stoi(value);
            return to_string(v % 256);
        }
    }
    
    return value;
}
// Handle implicit type conversions in assignments
string IRGenerator::handleImplicitConversion(const string& valueTemp, Node* valueNode, Node* targetVarNode) {
    if (!targetVarNode || !targetVarNode->symbol) {
        return valueTemp;
    }
    
    string targetType = normalizeTypeName(targetVarNode->symbol->type);
    string sourceType = getExpressionResultType(valueNode);
    
    cout << "DEBUG: Conversion check - source: " << sourceType 
         << " -> target: " << targetType << endl;
    
    // ✅ FIX: For float literals, don't convert double->float (they're compatible)
    if ((sourceType == "double" && targetType == "float") ||
        (sourceType == "float" && targetType == "double")) {
        cout << "DEBUG: Float/double are compatible, no conversion needed" << endl;
        return valueTemp;
    }
    
    // No conversion needed
    if (targetType == sourceType) {
        return valueTemp;
    }
    
    // ✅ FIX: Only do conversions for actual type mismatches
    // Check if it's a meaningful conversion (not just representation difference)
    bool needsConversion = false;
    
    // Integer <-> Float conversions
    if ((sourceType == "int" || sourceType == "long" || sourceType == "short" || sourceType == "char") &&
        (targetType == "float" || targetType == "double")) {
        needsConversion = true;
    }
    else if ((sourceType == "float" || sourceType == "double") &&
             (targetType == "int" || targetType == "long" || targetType == "short" || targetType == "char")) {
        needsConversion = true;
    }
    // Bool conversions (but not for pointers!)
    else if (sourceType == "bool" && targetType != "bool" && targetType.find('*') == string::npos) {
        needsConversion = true;
    }
    else if (sourceType != "bool" && targetType == "bool" && sourceType.find('*') == string::npos) {
        needsConversion = true;
    }
    // Integer width conversions (optional - can skip for IR)
    else if ((sourceType == "char" || sourceType == "short") && 
             (targetType == "int" || targetType == "long")) {
        needsConversion = false;  // Promotion happens implicitly
    }
    
    if (!needsConversion) {
        cout << "DEBUG: No meaningful conversion needed" << endl;
        return valueTemp;
    }
    
    cout << "DEBUG: Conversion needed: " << sourceType << " -> " << targetType << endl;
    
    // Generate runtime cast
    string resultTemp = createTemp();
    instructions.emplace_back(TACOp::CAST, resultTemp, valueTemp, targetType);
    return resultTemp;
}
    
// Determine the result type of an expression
string IRGenerator::getExpressionResultType(Node* exprNode) {
    if (!exprNode) return "int";
       if (exprNode->name == "EXPR_LIST" && !exprNode->children.empty()) {
        return getExpressionResultType(exprNode->children[0]);
    }
    // Identifier - use its type
    if (exprNode->name == "IDENTIFIER" && exprNode->symbol) {
        return normalizeTypeName(exprNode->symbol->type);
    }
    
    // Literals
    if (exprNode->name == "BOOL_LITERAL") return "bool";
    if (exprNode->name == "CHAR_LITERAL") return "char";
    if (exprNode->name == "INTEGER_CONSTANT") return "int";
    if (exprNode->name == "FLOAT_CONSTANT") return "double";
    if (exprNode->name == "STRING_LITERAL") return "char*";
    
    // Binary operations - determine result type from operands
    if (exprNode->name == "ADD_EXPR" || exprNode->name == "SUB_EXPR" ||
        exprNode->name == "MUL_EXPR" || exprNode->name == "DIV_EXPR" ||
        exprNode->name == "MOD_EXPR") {
        
        if (exprNode->children.size() >= 2) {
            string leftType = getExpressionResultType(exprNode->children[0]);
            string rightType = getExpressionResultType(exprNode->children[1]);
            
            // Apply usual arithmetic conversions
            // If either is double, result is double
            if (leftType == "double" || rightType == "double") return "double";
            
            // If either is float, result is float
            if (leftType == "float" || rightType == "float") return "float";
            
            // If either is long, result is long
            if (leftType == "long" || rightType == "long") return "long";
            
            // Otherwise int (after promotion)
            return "int";
        }
    }
    
    // Comparison operations always return bool/int
    if (exprNode->name == "EQ_EXPR" || exprNode->name == "NEQ_EXPR" ||
        exprNode->name == "LT_EXPR" || exprNode->name == "GT_EXPR" ||
        exprNode->name == "LE_EXPR" || exprNode->name == "GE_EXPR" ||
        exprNode->name == "LOGICAL_AND" || exprNode->name == "LOGICAL_OR") {
        return "int";
    }
    
    // Unary operations
    if (exprNode->name == "UNARY_OP" && exprNode->children.size() >= 2) {
        Node* op = exprNode->children[0];
        string opStr = op->name.empty() ? op->lexeme : op->name;
        
        if (opStr == "&") {
            // Address-of adds one pointer level
            string operandType = getExpressionResultType(exprNode->children[1]);
            return operandType + "*";
        }
        if (opStr == "*") {
            // ✅ Dereference - strip one pointer level from operand type
            Node* operand = exprNode->children[1];
            string operandType = getExpressionResultType(operand);
            
            // If operand has symbol with type, use that for more accurate type info
            if (operand->name == "IDENTIFIER" && operand->symbol) {
                operandType = normalizeTypeName(operand->symbol->type);
            }
            
            // Strip one level of pointer (remove trailing *)
            size_t lastStar = operandType.find_last_of('*');
            if (lastStar != string::npos) {
                operandType = operandType.substr(0, lastStar);
                // Trim trailing spaces
                while (!operandType.empty() && operandType.back() == ' ') {
                    operandType.pop_back();
                }
                return operandType;
            }
            
            // If no pointer to strip, return as-is (may be an error case)
            return operandType;
        }
        if (opStr == "!") return "int";
        if (opStr == "~") return "int";
        if (opStr == "-" || opStr == "+") {
            return getExpressionResultType(exprNode->children[1]);
        }
    }
    
    // Cast expression
    if (exprNode->name == "CAST_EXPR") {
        // Get target type from cast
        for (auto child : exprNode->children) {
            if (child && child->name == "SPEC_QUAL_LIST") {
                return extractTypeFromSpecQualList(child);
            }
        }
    }
    
    // ✅ Array access - result is element type (strips one array dimension)
    if (exprNode->name == "ARRAY_ACCESS" && exprNode->children.size() >= 1) {
        Node* arrayNode = exprNode->children[0];
        string arrayType = getExpressionResultType(arrayNode);
        
        // If array type ends with *, strip one pointer level
        size_t lastStar = arrayType.find_last_of('*');
        if (lastStar != string::npos) {
            arrayType = arrayType.substr(0, lastStar);
            while (!arrayType.empty() && arrayType.back() == ' ') {
                arrayType.pop_back();
            }
            return arrayType;
        }
        return arrayType;
    }
    
    // ✅ Member access - return member type
    if (exprNode->name == "MEMBER_ACCESS" || exprNode->name == "PTR_MEMBER_ACCESS") {
        if (exprNode->children.size() >= 2) {
            Node* baseNode = exprNode->children[0];
            Node* memberNode = exprNode->children[1];
            
            if (memberNode && memberNode->name == "IDENTIFIER") {
                string memberName = memberNode->lexeme;
                string baseType = resolveStructTypeFromLValue(baseNode);
                
                // Strip "struct " or "union " prefix and pointer suffixes
                string structTypeName = baseType;
                
                // Strip trailing * for pointer types
                size_t lastStar = structTypeName.find_last_of('*');
                if (lastStar != string::npos) {
                    structTypeName = structTypeName.substr(0, lastStar);
                    while (!structTypeName.empty() && structTypeName.back() == ' ') {
                        structTypeName.pop_back();
                    }
                }
                
                // Remove "struct " or "union " prefix
                if (structTypeName.find("struct ") == 0) {
                    structTypeName = structTypeName.substr(7);
                } else if (structTypeName.find("union ") == 0) {
                    structTypeName = structTypeName.substr(6);
                }
                
                // Look up struct and find member type
                Symbol* structSym = symbolTable.lookuph(structTypeName);
                if (structSym && structSym->isStruct) {
                    auto it = structSym->structMembers.find(memberName);
                    if (it != structSym->structMembers.end()) {
                        return it->second;  // Return member type
                    }
                }
            }
        }
    }
    
    // ✅ Increment/decrement - return operand type
    if (exprNode->name == "PRE_INC" || exprNode->name == "PRE_DEC" ||
        exprNode->name == "POST_INC" || exprNode->name == "POST_DEC") {
        if (!exprNode->children.empty()) {
            return getExpressionResultType(exprNode->children[0]);
        }
    }
    
    // Default to int
    return "int";
}


// Apply integer promotion (bool/char -> int) for arithmetic operations
string IRGenerator::applyIntegerPromotion(const string& valueTemp, Node* valueNode) {
    if (!valueNode) return valueTemp;
    
    string sourceType = "";
    
    // Determine the type of the operand
    if (valueNode->name == "IDENTIFIER" && valueNode->symbol) {
        sourceType = normalizeTypeName(valueNode->symbol->type);
    } else if (valueNode->name == "BOOL_LITERAL") {
        sourceType = "bool";
    } else if (valueNode->name == "CHAR_LITERAL") {
        sourceType = "char";
    } else if (valueNode->name == "INTEGER_CONSTANT") {
        return valueTemp;  // Already int
    } else if (valueNode->name == "FLOAT_CONSTANT") {
        return valueTemp;  // Float doesn't need promotion to int
    } else {
        return valueTemp;
    }
    
    // Integer promotion: bool, char, short → int (C standard 6.3.1.1)
    if (sourceType != "bool" && sourceType != "char" && 
        sourceType != "unsigned char" && sourceType != "signed char" &&
        sourceType != "short" && sourceType != "unsigned short") {
        return valueTemp;
    }
    
    cout << "DEBUG: Integer promotion: " << sourceType << " -> int for " << valueTemp << endl;
    
    // Check if it's a compile-time constant
    if (isConstantValue(valueTemp)) {
        string constValue = getConstantValue(valueTemp);
        string convertedValue = convertConstantValue(constValue, sourceType, "int");
        
        cout << "DEBUG: Constant promotion: " << constValue << " -> " << convertedValue << endl;
        
        string resultTemp = createTemp();
        instructions.emplace_back(TACOp::CONST, resultTemp, convertedValue);
        return resultTemp;
    }
    
    // Runtime promotion
    cout << "DEBUG: Runtime promotion: " << valueTemp << " (" << sourceType << ") -> int" << endl;
    string resultTemp = createTemp();
    instructions.emplace_back(TACOp::CAST, resultTemp, valueTemp, "int");
    return resultTemp;
}

// Generate constant cast (compile-time conversion)
string IRGenerator::generateConstantCast(const string& valTemp, const string& targetType) {
    string constValue = getConstantValue(valTemp);
    string resultTemp = createTemp();
    
    if (constValue.empty()) {
        // Fallback to runtime cast
        instructions.emplace_back(TACOp::CAST, resultTemp, valTemp, targetType);
        return resultTemp;
    }
    
    // Convert the constant based on target type
    string convertedValue = convertConstantValue(constValue, "", targetType);
    
    // Emit the converted constant directly
    instructions.emplace_back(TACOp::CONST, resultTemp, convertedValue);
    return resultTemp;
}

// Extract type name from SPEC_QUAL_LIST
string IRGenerator::extractTypeFromSpecQualList(Node* typeNode) {
    if (!typeNode) return "int";
    
    string typeName = "";
    bool isSigned = true;
    
    function<void(Node*)> extractType = [&](Node* n) {
        if (!n) return;
        
        if (n->name == "TYPE_NAME") {
            // This is a typedef name - resolve it
            typeName = normalizeTypeName(n->lexeme);
        }
        else if (n->name == "TYPE_SPECIFIER") {
            if (n->lexeme == "int" || n->lexeme == "char" || 
                n->lexeme == "float" || n->lexeme == "double" ||
                n->lexeme == "bool" || n->lexeme == "short" || 
                n->lexeme == "long" || n->lexeme == "void") {
                typeName = n->lexeme;
            }
        }
        else if (n->name == "STRUCT_OR_UNION_SPECIFIER") {
            // Extract struct/union type
            for (auto child : n->children) {
                if (child->name == "STRUCT" || child->name == "struct") {
                    // Find the struct name
                    for (auto sibling : n->children) {
                        if (sibling->name == "IDENTIFIER") {
                            typeName = "struct " + sibling->lexeme;
                            return;
                        }
                    }
                } else if (child->name == "UNION" || child->name == "union") {
                    // Find the union name
                    for (auto sibling : n->children) {
                        if (sibling->name == "IDENTIFIER") {
                            typeName = "union " + sibling->lexeme;
                            return;
                        }
                    }
                }
            }
        }
        else if (n->name == "SIGNED") {
            isSigned = true;
        }
        else if (n->name == "UNSIGNED") {
            isSigned = false;
        }
        
        for (auto child : n->children) {
            extractType(child);
        }
    };
    
    extractType(typeNode);
    
    if (typeName.empty()) typeName = "int";
    
    if (!isSigned && (typeName == "int" || typeName == "char")) {
        typeName = "unsigned " + typeName;
    }
    
    return typeName;
}

// OPTIMIZATION: Peephole optimization to remove redundant instructions
void IRGenerator::optimizeTAC() {
    if (instructions.empty()) return;
    
    vector<TACInstruction> optimized;
    optimized.reserve(instructions.size());
    
    for (size_t i = 0; i < instructions.size(); ++i) {
        auto& instr = instructions[i];
        bool skip = false;
        
        // Optimization 1: Remove consecutive bool casts
        // Pattern: t1 = (bool)x; t2 = (bool)t1  ->  t1 = (bool)x
        if (instr.opcode == TACOp::CAST && instr.operand2 == "bool") {
            // Check if previous instruction was also a bool cast of the same operand
            if (!optimized.empty()) {
                auto& prev = optimized.back();
                if (prev.opcode == TACOp::CAST && prev.operand2 == "bool" && 
                    prev.result == instr.operand1) {
                    // This is a redundant cast: t2 = (bool)t1 where t1 = (bool)x
                    // Skip this instruction and update all future references
                    skip = true;
                    cout << "Optimization: Removed redundant bool cast " << instr.result 
                         << " = (bool)" << instr.operand1 << endl;
                    
                    // Update future instructions that use instr.result to use prev.result instead
                    for (size_t j = i + 1; j < instructions.size(); ++j) {
                        auto& future = instructions[j];
                        if (future.operand1 == instr.result) future.operand1 = prev.result;
                        if (future.operand2 == instr.result) future.operand2 = prev.result;
                    }
                }
            }
        }
        
        // Optimization 2: Remove unused temporary assignments
        // Pattern: t10 = i#3 where t10 is never used
        if (!skip && instr.opcode == TACOp::ASSIGN && instr.result[0] == 't') {
            // Check if this temp is used anywhere later
            bool isUsed = false;
            for (size_t j = i + 1; j < instructions.size(); ++j) {
                auto& future = instructions[j];
                if (future.operand1 == instr.result || future.operand2 == instr.result) {
                    isUsed = true;
                    break;
                }
            }
            if (!isUsed) {
                skip = true;
                cout << "Optimization: Removed unused temp assignment " << instr.result 
                     << " = " << instr.operand1 << endl;
            }
        }
        
        if (!skip) {
            optimized.push_back(instr);
        }
    }
    
    cout << "Optimization: Reduced " << instructions.size() << " instructions to " 
         << optimized.size() << " (" << (instructions.size() - optimized.size()) 
         << " removed)" << endl;
    
    instructions = std::move(optimized);
}
