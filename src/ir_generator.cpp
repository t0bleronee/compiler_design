#include "ir_generator.h"
#include <sstream>
#include <iostream>
#include <functional>
#include <algorithm>

IRGenerator::IRGenerator(SymbolTable& symtab)
    : symtab(symtab), tempCounter(0), labelCounter(0) {
    // reference symtab to avoid unused-member warnings when not needed yet
    (void)this->symtab;
}

// Small typed value representation used temporarily in lowering
struct IRValue {
    std::string place; // temporary or variable name
    std::string type;  // textual type like "int", "float", "struct Point", "int*"
    IRValue() {}
    IRValue(const std::string &p, const std::string &t) : place(p), type(t) {}
};

// Helper: determine size (in bytes) of a textual type using symbol table info
// This is conservative and handles basic types, pointers, arrays (if expressed), and structs
static int getTypeSize(SymbolTable &symtab, const std::string &typeStr) {
    if (typeStr.empty()) return 4;

    // pointer types marked by trailing '*' characters
    int starCount = 0;
    for (int i = (int)typeStr.size()-1; i >= 0 && typeStr[i] == '*'; --i) starCount++;
    if (starCount > 0) {
        // assume 8-byte pointers on target (x86_64)
        return 8;
    }

    // simple known types
    if (typeStr.find("int") != std::string::npos) return 4;
    if (typeStr.find("char") != std::string::npos) return 1;
    if (typeStr.find("float") != std::string::npos) return 4;
    if (typeStr.find("double") != std::string::npos) return 8;

    // struct types like "struct Name" -> lookup symbol and sum member sizes
    if (typeStr.rfind("struct ", 0) == 0) {
        std::string name = typeStr.substr(7);
        Symbol* s = symtab.lookup(name);
        if (!s) return 4;
        int total = 0;
        for (const auto &m : s->structMembers) {
            total += getTypeSize(symtab, m.second);
        }
        return total > 0 ? total : 4;
    }

    // enum -> assume int
    if (typeStr.find("enum") != std::string::npos) return 4;

    // fallback conservative
    return 4;
}

// Compute member offset within a struct symbol by summing sizes of preceding members
static int computeMemberOffset(SymbolTable &symtab, Symbol* sym, const std::string &member) {
    if (!sym) return -1;
    int offset = 0;
    for (const auto &p : sym->structMembers) {
        if (p.first == member) return offset;
        offset += getTypeSize(symtab, p.second);
    }
    return -1;
}

std::string IRGenerator::newTemp() {
    return "t" + std::to_string(++tempCounter);
}

std::string IRGenerator::newLabel() {
    return "L" + std::to_string(++labelCounter);
}

int IRGenerator::emit(const std::string& instr) {
    code.push_back(instr);
    return (int)code.size() - 1;
}

void IRGenerator::pushLoop(const std::string& breakLbl, const std::string& continueLbl) {
    loopStack.push_back({breakLbl, continueLbl});
}

void IRGenerator::popLoop() {
    if (!loopStack.empty()) loopStack.pop_back();
}

IRGenerator::LoopContext* IRGenerator::currentLoop() {
    return loopStack.empty() ? nullptr : &loopStack.back();
}

void IRGenerator::generate(Node* root) {
    code.clear();
    tempCounter = 0;
    labelCounter = 0;
    loopStack.clear();

    if (!root) return;

    // First, emit globals (if any) as simple annotations in the IR
    for (auto child : root->children) {
        if (!child) continue;
        if (child->name == "GLOBAL_DECLARATION") {
            // Scan for declarations and emit GLOBAL lines
            for (auto gc : child->children) {
                if (!gc) continue;
                if (gc->name == "DECLARATION") {
                    // Look for INIT_DECL_LIST inside
                    for (auto d : gc->children) {
                        if (!d) continue;
                        if (d->name == "INIT_DECL_LIST") {
                            for (auto initDecl : d->children) {
                                if (!initDecl || initDecl->children.empty()) continue;
                                Node* first = initDecl->children[0];
                                if (!first) continue;
                                // Identifier or array declarator
                                std::function<std::string(Node*)> getName = [&](Node* n)->std::string {
                                    if (!n) return std::string();
                                    if (n->name == "IDENTIFIER") return n->lexeme;
                                    for (auto ch : n->children) {
                                        std::string nm = getName(ch);
                                        if (!nm.empty()) return nm;
                                    }
                                    return std::string();
                                };
                                std::string gname = getName(first);
                                if (!gname.empty()) {
                                    emit("GLOBAL " + gname);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Then emit functions
    for (auto child : root->children) {
        if (!child) continue;
        if (child->name == "FUNCTION_DEFINITION") {
            genFunction(child);
        }
    }
}

void IRGenerator::genFunction(Node* node) {
    if (!node) return;
    std::string funcName = "<anon>";
    
    // helper: find first node with given name in subtree
    std::function<Node*(Node*, const std::string&)> findFirst = [&](Node* n, const std::string& target)->Node* {
        if (!n) return nullptr;
        if (n->name == target) return n;
        for (auto ch : n->children) {
            Node* r = findFirst(ch, target);
            if (r) return r;
        }
        return nullptr;
    };

    // prefer the identifier inside a FUNCTION_DECL node
    Node* fdecl = findFirst(node, "FUNCTION_DECL");
    if (fdecl) {
        Node* id = findFirst(fdecl, "IDENTIFIER");
        if (id) funcName = id->lexeme;
    }
    // fallback: any identifier in subtree
    if (funcName == "<anon>") {
        Node* id2 = findFirst(node, "IDENTIFIER");
        if (id2) funcName = id2->lexeme;
    }

    emit("FUNC " + funcName + ":");

    // collect locals by scanning the compound statement subtree
    std::vector<std::string> locals;
    Node* body = findFirst(node, "COMPOUND_STMT");
    if (body) {
        // helper to extract identifier from declarator node
        std::function<std::string(Node*)> getName = [&](Node* n)->std::string {
            if (!n) return std::string();
            if (n->name == "IDENTIFIER") return n->lexeme;
            for (auto ch : n->children) {
                std::string r = getName(ch);
                if (!r.empty()) return r;
            }
            return std::string();
        };

        // scan subtree for DECLARATION -> INIT_DECL_LIST -> ASSIGN_EXPR (declarator)
        std::function<void(Node*)> scan = [&](Node* n) {
            if (!n) return;
            if (n->name == "DECLARATION") {
                for (auto dchild : n->children) {
                    if (!dchild) continue;
                    if (dchild->name == "INIT_DECL_LIST") {
                        for (auto initDecl : dchild->children) {
                            if (!initDecl) continue;
                            Node* decl = initDecl->children.size() ? initDecl->children[0] : nullptr;
                            std::string nm = getName(decl);
                            if (!nm.empty()) locals.push_back(nm);
                        }
                    }
                }
            }
            for (auto ch : n->children) scan(ch);
        };
        scan(body);
    }

    int nlocals = (int)locals.size();
    emit("alloc " + std::to_string(nlocals));
    if (nlocals > 0) {
        std::string list = "#locals: ";
        for (size_t i = 0; i < locals.size(); ++i) {
            if (i) list += ", ";
            list += locals[i];
        }
        emit(list);
    }

    // generate body
    Node* bodyNode = findFirst(node, "COMPOUND_STMT");
    if (bodyNode) {
        PatchList pending = genStmt(bodyNode);
        std::string Lend = newLabel();
        emit(Lend + ":");
        backpatch(pending, Lend);
    }

    emit("END_FUNC");
}

IRGenerator::ExprResult IRGenerator::genExpr(Node* node) {
    ExprResult res;
    if (!node) return res;

    // helper to build a target string for LHS (identifiers, array access)
    std::function<std::string(Node*)> getTargetPlace = [&](Node* n)->std::string {
        if (!n) return std::string();
        if (n->name == "IDENTIFIER") return n->lexeme;
        
        // Handle DECLARATOR nodes
        if (n->name == "DECLARATOR") {
            for (auto child : n->children) {
                if (child) {
                    if (child->name == "ARRAY") {
                        Node* baseNode = child->children[0];
                        if (baseNode && baseNode->name == "IDENTIFIER") {
                            return baseNode->lexeme;
                        }
                    } else if (child->name == "IDENTIFIER") {
                        return child->lexeme;
                    }
                }
            }
        }
        
        // Handle array declarations: int arr[2][3]
        if (n->name == "ARRAY") {
            // For nested arrays, the first child is another ARRAY node
            Node* firstChild = n->children[0];
            if (firstChild && firstChild->name == "ARRAY") {
                // Recursively get the base identifier
                return getTargetPlace(firstChild);
            } else if (firstChild && firstChild->name == "IDENTIFIER") {
                return firstChild->lexeme;
            }
        }
        // dereference as target: *p
        if (n->name == "UNARY_OP" && n->children.size() >= 2 && n->children[0] && n->children[0]->lexeme == "*") {
            Node* inner = n->children[1];
            // If inner is identifier or ptr member, return textual deref
            if (inner->name == "IDENTIFIER") return "*" + inner->lexeme;
            // if inner is member access or array access, compute its place
            ExprResult r = genExpr(inner);
            if (r.isBoolean) r.place = boolToTemp(r);
            if (!r.place.empty()) return "*" + r.place;
            return std::string();
        }
        // member access: base.member
        if (n->name == "MEMBER_ACCESS" && n->children.size() >= 2) {
                Node* base = n->children[0];
                Node* member = n->children[1];
                ExprResult b = genExpr(base);
                // If boolean, materialize to temp so we have a place
                if (b.isBoolean) b.place = boolToTemp(b);
                // fallback baseName: prefer b.place, else identifier lexeme
                std::string baseName;
                if (!b.place.empty()) baseName = b.place;
                else if (base->name == "IDENTIFIER") baseName = base->lexeme;
                else baseName = "";

                // Try to lookup struct symbol information for base's type
                Symbol *sym = nullptr;
                if (base->name == "IDENTIFIER") sym = symtab.lookup(base->lexeme);

                // Default: emit simple textual access if we can't compute offset
                if (!sym || sym->structMembers.empty()) {
                    if (member && member->name == "IDENTIFIER") return baseName + "." + member->lexeme;
                    return baseName;
                }

                // compute offset
                int offset = computeMemberOffset(symtab, sym, member->lexeme);
                if (offset < 0) {
                    if (member && member->name == "IDENTIFIER") return baseName + "." + member->lexeme;
                    return baseName;
                }

                return baseName + " + " + std::to_string(offset);
        }
        // pointer member access: base->member
        if (n->name == "PTR_MEMBER_ACCESS" && n->children.size() >= 2) {
            Node* base = n->children[0];
            Node* member = n->children[1];
            ExprResult br = genExpr(base);
            if (br.isBoolean) br.place = boolToTemp(br);
            std::string baseName = !br.place.empty() ? br.place : (base->name == "IDENTIFIER" ? base->lexeme : std::string());
            // If base is an identifier and points to a struct variable, try to compute offset
            Symbol* sym = nullptr;
            if (base->name == "IDENTIFIER") sym = symtab.lookup(base->lexeme);
            if (sym && !sym->structMembers.empty()) {
                int offset = computeMemberOffset(symtab, sym, member->lexeme);
                if (offset >= 0) return baseName + " + " + std::to_string(offset);
            }
            if (member && member->name == "IDENTIFIER") return baseName + "->" + member->lexeme;
            return baseName;
        }
        if (n->name == "ARRAY_ACCESS" && n->children.size() >= 2) {
            Node* base = n->children[0];
            Node* idxList = n->children[1];
            Node* idxExpr = nullptr;
            if (idxList && idxList->name == "EXPR_LIST" && !idxList->children.empty()) 
                idxExpr = idxList->children[0];
            else 
                idxExpr = idxList;
            
            ExprResult idxRes = genExpr(idxExpr);
            if (idxRes.isBoolean) idxRes.place = boolToTemp(idxRes);
            ExprResult br = genExpr(base);
            if (br.isBoolean) br.place = boolToTemp(br);
            std::string baseName = !br.place.empty() ? br.place : (base->name == "IDENTIFIER" ? base->lexeme : std::string());
            if (idxRes.place.empty()) return baseName;
            return baseName + "[" + idxRes.place + "]";
        }
        return std::string();
    };

    // constants and identifiers
    if (node->name == "INTEGER_CONSTANT" || node->name == "FLOAT_CONSTANT" || 
        node->name == "STRING_LITERAL" || node->name == "CHAR_CONSTANT") {
        res.place = node->lexeme;
        return res;
    }
    if (node->name == "IDENTIFIER") {
        res.place = node->lexeme;
        return res;
    }

    // member access as rvalue
    if ((node->name == "MEMBER_ACCESS" || node->name == "PTR_MEMBER_ACCESS") && node->children.size() >= 2) {
        Node* base = node->children[0];
        Node* member = node->children[1];
        ExprResult baseRes = genExpr(base);
        std::string baseName = baseRes.place.empty() && base->name == "IDENTIFIER" ? base->lexeme : baseRes.place;
        if (!member || member->name != "IDENTIFIER") {
            return res;
        }
        std::string mem = member->lexeme;

        // Try to lookup struct information from symbol table when base is identifier
        Symbol *sym = nullptr;
        if (base->name == "IDENTIFIER") sym = symtab.lookup(base->lexeme);

        if (sym && !sym->structMembers.empty()) {
            int offset = computeMemberOffset(symtab, sym, mem);
            if (offset >= 0) {
                std::string tmp = newTemp();
                emit(tmp + " = load " + baseName + " + " + std::to_string(offset));
                res.place = tmp;
                return res;
            }
        }

        // fallback: textually access
        std::string tmp = newTemp();
        if (node->name == "MEMBER_ACCESS") {
            emit(tmp + " = " + baseName + "." + mem);
        } else {
            emit(tmp + " = " + baseName + "->" + mem);
        }
        res.place = tmp;
        return res;
    }

    // array access as rvalue
    if (node->name == "ARRAY_ACCESS") {
        if (node->children.size() >= 2) {
            Node* base = node->children[0];
            Node* idxList = node->children[1];
            Node* idxExpr = nullptr;
            if (idxList && idxList->name == "EXPR_LIST" && !idxList->children.empty()) 
                idxExpr = idxList->children[0];
            else 
                idxExpr = idxList;
            
            ExprResult baseRes = genExpr(base);
            ExprResult idxRes = genExpr(idxExpr);
            std::string baseName = baseRes.place.empty() ? base->lexeme : baseRes.place;
            std::string tmp = newTemp();
            emit(tmp + " = " + baseName + "[" + idxRes.place + "]");
            res.place = tmp;
            return res;
        }
    }

    // function call
    if (node->name == "FUNC_CALL") {
        std::vector<std::string> args;
        if (node->children.size() > 1 && node->children[1] && 
            node->children[1]->name == "ARG_LIST") {
            for (auto a : node->children[1]->children) {
                ExprResult ar = genExpr(a);
                if (!ar.isBoolean) {
                    if (!ar.place.empty()) args.push_back(ar.place);
                } else {
                    // Convert boolean to 0/1
                    std::string t = boolToTemp(ar);
                    args.push_back(t);
                }
            }
        }

        // callee
        std::string callee;
        if (!node->children.empty()) {
            ExprResult c = genExpr(node->children[0]);
            callee = c.place;
        }
        
        // emit params in reverse order
        for (auto it = args.rbegin(); it != args.rend(); ++it) 
            emit("param " + *it);
        
        std::string ret = newTemp();
        emit(ret + " = call " + callee + ", " + std::to_string(args.size()));
        res.place = ret;
        return res;
    }

    // binary arithmetic
    if (node->name == "ADD_EXPR" || node->name == "SUB_EXPR" || 
        node->name == "MUL_EXPR" || node->name == "DIV_EXPR" || 
        node->name == "MOD_EXPR") {
        if (node->children.size() < 2) return res;
        
        ExprResult L = genExpr(node->children[0]);
        ExprResult R = genExpr(node->children[1]);
        
        // Convert booleans to temps
        std::string left = L.isBoolean ? boolToTemp(L) : L.place;
        std::string right = R.isBoolean ? boolToTemp(R) : R.place;
        
        std::string tmp = newTemp();
        std::string op = "+";
        if (node->name == "SUB_EXPR") op = "-";
        else if (node->name == "MUL_EXPR") op = "*";
        else if (node->name == "DIV_EXPR") op = "/";
        else if (node->name == "MOD_EXPR") op = "%";
        
        emit(tmp + " = " + left + " " + op + " " + right);
        res.place = tmp;
        return res;
    }

    // unary operators
    if (node->name == "UNARY_OP" && node->children.size() >= 2) {
        std::string op = node->children[0]->lexeme.empty() ? node->children[0]->name : node->children[0]->lexeme;

        // address-of
        if (op == "&") {
            Node* targetNode = node->children[1];
            std::string target;
            
            if (targetNode->name == "IDENTIFIER") {
                // Simple case: &x -> &x
                target = "&" + targetNode->lexeme;
            } else {
                // Complex case: &(expr) -> &(expr)
                ExprResult r = genExpr(targetNode);
                if (!r.place.empty()) {
                    target = "&(" + r.place + ")";
                } else {
                    target = "&" + targetNode->lexeme;
                }
            }
            res.place = target;
            return res;
        }

        // dereference
        if (op == "*") {
            Node* innerNode = node->children[1];
            std::string pointerExpr;
            
            if (innerNode->name == "IDENTIFIER") {
                // Simple case: *p -> load p
                pointerExpr = innerNode->lexeme;
            } else {
                // Complex case: *(expr) -> load (expr)
                ExprResult inner = genExpr(innerNode);
                if (inner.isBoolean) inner.place = boolToTemp(inner);
                pointerExpr = inner.place;
            }
            
            if (!pointerExpr.empty()) {
                std::string tmp = newTemp();
                emit(tmp + " = load " + pointerExpr);
                res.place = tmp;
                return res;
            }
            return res;
        }

        // logical not
        if (op == "!") {
            ExprResult inner = genExpr(node->children[1]);
            if (inner.isBoolean) {
                res.isBoolean = true;
                res.truelist = inner.falselist;
                res.falselist = inner.truelist;
                return res;
            } else {
                // Convert to boolean comparison: !x -> x == 0
                int idxTrue = emit("if_true " + inner.place + " == 0 goto ");
                int idxFalse = emit("goto ");
                res.isBoolean = true;
                res.truelist = makelist(idxTrue);
                res.falselist = makelist(idxFalse);
                return res;
            }
        }
        
        // arithmetic unary: -, +, ~
        ExprResult inner = genExpr(node->children[1]);
        std::string operand = inner.isBoolean ? boolToTemp(inner) : inner.place;
        std::string tmp = newTemp();
        emit(tmp + " = " + op + operand);
        res.place = tmp;
        return res;
    }

    // assignment
    if (node->name == "ASSIGN_EXPR") {
        if (node->children.size() >= 3) {
            Node* lhsNode = node->children[0];
            Node* rhsNode = node->children[2];
            
            // handle empty initializer
            if (rhsNode && rhsNode->name == "EMPTY") {
                std::string targ = getTargetPlace(lhsNode);
                res.place = targ;
                return res;
            }
            
            // handle array initialization
            if (rhsNode && rhsNode->name == "INIT_LIST") {
                std::string arrayName = getTargetPlace(lhsNode);
                if (!arrayName.empty()) {
                    // Generate array initialization code
                    genArrayInitialization(arrayName, rhsNode, 0);
                    res.place = arrayName;
                    return res;
                }
            }
            
            // compute RHS
            ExprResult R = genExpr(rhsNode);
            if (R.place.empty() && !R.isBoolean) {
                return res;
            }
            
            // Convert boolean RHS to temp
            std::string rhsVal = R.isBoolean ? boolToTemp(R) : R.place;
            
            // determine LHS target - handle pointer assignments specially
            std::string target;
            if (lhsNode->name == "IDENTIFIER") {
                // Simple variable assignment
                target = lhsNode->lexeme;
            } else if (lhsNode->name == "DECLARATOR") {
                // Handle declarator nodes (for declarations with initializers)
                // Structure: DECLARATOR -> [POINTER, IDENTIFIER] (siblings)
                for (auto child : lhsNode->children) {
                    if (child && child->name == "IDENTIFIER") {
                        target = child->lexeme;
                        break;
                    }
                }
            } else if (lhsNode->name == "UNARY_OP" && lhsNode->children.size() >= 2 && 
                      lhsNode->children[0]->lexeme == "*") {
                // Pointer dereference assignment: *p = value
                Node* ptrNode = lhsNode->children[1];
                std::string ptrExpr;
                if (ptrNode->name == "IDENTIFIER") {
                    ptrExpr = ptrNode->lexeme;
                } else {
                    ExprResult ptrRes = genExpr(ptrNode);
                    ptrExpr = ptrRes.place;
                }
                if (!ptrExpr.empty()) {
                    emit("store " + ptrExpr + " = " + rhsVal);
                    res.place = ptrExpr;
                    return res;
                }
            } else {
                // Use getTargetPlace for complex cases
                target = getTargetPlace(lhsNode);
                if (target.empty()) {
                    ExprResult L = genExpr(lhsNode);
                    target = L.place;
                }
            }
            
            if (!target.empty()) {
                emit(target + " = " + rhsVal);
                res.place = target;
            }
            return res;
        }
    }

    // compound assignments: +=, -=, *=, /=, %=
    if (node->name == "ADD_ASSIGN" || node->name == "SUB_ASSIGN" || 
        node->name == "MUL_ASSIGN" || node->name == "DIV_ASSIGN" || 
        node->name == "MOD_ASSIGN") {
        if (node->children.size() >= 2) {
            Node* lhsNode = node->children[0];
            Node* rhsNode = node->children[1];
            
            std::string target = getTargetPlace(lhsNode);
            ExprResult R = genExpr(rhsNode);
            std::string rhsVal = R.isBoolean ? boolToTemp(R) : R.place;
            
            std::string op = "+";
            if (node->name == "SUB_ASSIGN") op = "-";
            else if (node->name == "MUL_ASSIGN") op = "*";
            else if (node->name == "DIV_ASSIGN") op = "/";
            else if (node->name == "MOD_ASSIGN") op = "%";
            
            std::string tmp = newTemp();
            emit(tmp + " = " + target + " " + op + " " + rhsVal);
            emit(target + " = " + tmp);
            res.place = target;
            return res;
        }
    }

    // comparisons -> boolean lists
    if (node->name == "EQ_EXPR" || node->name == "NEQ_EXPR" || 
        node->name == "LT_EXPR" || node->name == "GT_EXPR" || 
        node->name == "LE_EXPR" || node->name == "GE_EXPR") {
        if (node->children.size() < 2) return res;
        
        ExprResult L = genExpr(node->children[0]);
        ExprResult R = genExpr(node->children[1]);
        
        std::string left = L.isBoolean ? boolToTemp(L) : L.place;
        std::string right = R.isBoolean ? boolToTemp(R) : R.place;
        
        std::string op;
        if (node->name == "EQ_EXPR") op = "==";
        else if (node->name == "NEQ_EXPR") op = "!=";
        else if (node->name == "LT_EXPR") op = "<";
        else if (node->name == "GT_EXPR") op = ">";
        else if (node->name == "LE_EXPR") op = "<=";
        else if (node->name == "GE_EXPR") op = ">=";

        int idxTrue = emit("if_true " + left + " " + op + " " + right + " goto ");
        int idxFalse = emit("goto ");
        res.isBoolean = true;
        res.truelist = makelist(idxTrue);
        res.falselist = makelist(idxFalse);
        return res;
    }

    // logical && with short-circuit
    if (node->name == "LOGICAL_AND") {
        if (node->children.size() < 2) return res;
        
        ExprResult left = genExpr(node->children[0]);
        
        // Convert non-boolean left to boolean
        if (!left.isBoolean) {
            int idxTrue = emit("if_true " + left.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            left.isBoolean = true;
            left.truelist = makelist(idxTrue);
            left.falselist = makelist(idxFalse);
        }
        
        std::string rightLabel = newLabel();
        backpatch(left.truelist, rightLabel);
        emit(rightLabel + ":");
        
        ExprResult right = genExpr(node->children[1]);
        
        // Convert non-boolean right to boolean
        if (!right.isBoolean) {
            int idxTrue = emit("if_true " + right.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            right.isBoolean = true;
            right.truelist = makelist(idxTrue);
            right.falselist = makelist(idxFalse);
        }
        
        res.isBoolean = true;
        res.truelist = right.truelist;
        res.falselist = merge(left.falselist, right.falselist);
        return res;
    }

    // logical || with short-circuit
    if (node->name == "LOGICAL_OR") {
        if (node->children.size() < 2) return res;
        
        ExprResult left = genExpr(node->children[0]);
        
        // Convert non-boolean left to boolean
        if (!left.isBoolean) {
            int idxTrue = emit("if_true " + left.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            left.isBoolean = true;
            left.truelist = makelist(idxTrue);
            left.falselist = makelist(idxFalse);
        }
        
        std::string rightLabel = newLabel();
        backpatch(left.falselist, rightLabel);
        emit(rightLabel + ":");
        
        ExprResult right = genExpr(node->children[1]);
        
        // Convert non-boolean right to boolean
        if (!right.isBoolean) {
            int idxTrue = emit("if_true " + right.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            right.isBoolean = true;
            right.truelist = makelist(idxTrue);
            right.falselist = makelist(idxFalse);
        }
        
        res.isBoolean = true;
        res.truelist = merge(left.truelist, right.truelist);
        res.falselist = right.falselist;
        return res;
    }

    // ternary operator: cond ? true_expr : false_expr
    if (node->name == "TERNARY_EXPR" && node->children.size() >= 3) {
        ExprResult cond = genExpr(node->children[0]);
        
        if (!cond.isBoolean) {
            int idxTrue = emit("if_true " + cond.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            cond.isBoolean = true;
            cond.truelist = makelist(idxTrue);
            cond.falselist = makelist(idxFalse);
        }
        
        std::string Ltrue = newLabel();
        std::string Lfalse = newLabel();
        std::string Lend = newLabel();
        
        backpatch(cond.truelist, Ltrue);
        emit(Ltrue + ":");
        ExprResult trueRes = genExpr(node->children[1]);
        std::string trueVal = trueRes.isBoolean ? boolToTemp(trueRes) : trueRes.place;
        std::string result = newTemp();
        emit(result + " = " + trueVal);
        emit("goto " + Lend);
        
        backpatch(cond.falselist, Lfalse);
        emit(Lfalse + ":");
        ExprResult falseRes = genExpr(node->children[2]);
        std::string falseVal = falseRes.isBoolean ? boolToTemp(falseRes) : falseRes.place;
        emit(result + " = " + falseVal);
        
        emit(Lend + ":");
        res.place = result;
        return res;
    }

    // increment/decrement: ++x, --x, x++, x--
    if (node->name == "PRE_INC" || node->name == "PRE_DEC" || 
        node->name == "POST_INC" || node->name == "POST_DEC") {
        if (node->children.empty()) return res;
        
        std::string target = getTargetPlace(node->children[0]);
        if (target.empty()) {
            ExprResult e = genExpr(node->children[0]);
            target = e.place;
        }
        
        bool isPost = (node->name == "POST_INC" || node->name == "POST_DEC");
        bool isInc = (node->name == "PRE_INC" || node->name == "POST_INC");
        
        if (isPost) {
            // Save old value
            std::string oldVal = newTemp();
            emit(oldVal + " = " + target);
            
            std::string tmp = newTemp();
            emit(tmp + " = " + target + (isInc ? " + 1" : " - 1"));
            emit(target + " = " + tmp);
            
            res.place = oldVal;
        } else {
            std::string tmp = newTemp();
            emit(tmp + " = " + target + (isInc ? " + 1" : " - 1"));
            emit(target + " = " + tmp);
            res.place = target;
        }
        return res;
    }

    // fallback: evaluate first child
    if (!node->children.empty()) return genExpr(node->children[0]);

    return res;
}

std::string IRGenerator::boolToTemp(ExprResult &e) {
    if (!e.isBoolean) return e.place;
    
    std::string t = newTemp();
    std::string Ltrue = newLabel();
    std::string Lfalse = newLabel();
    std::string Lend = newLabel();
    
    backpatch(e.truelist, Ltrue);
    backpatch(e.falselist, Lfalse);
    
    emit(Ltrue + ":");
    emit(t + " = 1");
    emit("goto " + Lend);
    
    emit(Lfalse + ":");
    emit(t + " = 0");
    
    emit(Lend + ":");
    return t;
}

IRGenerator::PatchList IRGenerator::genStmt(Node* node) {
    PatchList nextlist;
    if (!node) return nextlist;

    // compound statements
    if (node->name == "COMPOUND_STMT" || node->name == "BLOCK_ITEM_LIST" || 
        node->name == "STATEMENT_LIST") {
        for (auto c : node->children) {
            PatchList nl = genStmt(c);
            nextlist = merge(nextlist, nl);
        }
        return nextlist;
    }

    // expression list
    if (node->name == "EXPR_LIST") {
        for (auto c : node->children) genExpr(c);
        return nextlist;
    }

    // return statement
    if (node->name == "RETURN_STMT") {
        if (!node->children.empty()) {
            ExprResult v = genExpr(node->children[0]);
            std::string val = v.isBoolean ? boolToTemp(v) : v.place;
            if (val.empty()) emit("ret");
            else emit("ret " + val);
        } else {
            emit("ret");
        }
        return nextlist;
    }

    // break statement
    if (node->name == "BREAK_STMT") {
        LoopContext* loop = currentLoop();
        if (loop && !loop->breakLabel.empty()) {
            emit("goto " + loop->breakLabel);
        }
        return nextlist;
    }

    // continue statement
    if (node->name == "CONTINUE_STMT") {
        LoopContext* loop = currentLoop();
        if (loop && !loop->continueLabel.empty()) {
            emit("goto " + loop->continueLabel);
        }
        return nextlist;
    }

    // if statement
    if (node->name == "IF_STMT") {
        if (node->children.size() < 2) return nextlist;
        
        ExprResult cond = genExpr(node->children[0]);
        
        if (!cond.isBoolean) {
            int idxTrue = emit("if_true " + cond.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            cond.isBoolean = true;
            cond.truelist = makelist(idxTrue);
            cond.falselist = makelist(idxFalse);
        }
        
        std::string Lthen = newLabel();
        std::string Lnext = newLabel();
        
        backpatch(cond.truelist, Lthen);
        emit(Lthen + ":");
        PatchList thenList = genStmt(node->children[1]);
        
        backpatch(cond.falselist, Lnext);
        backpatch(thenList, Lnext);
        emit(Lnext + ":");
        
        return nextlist;
    }

    // if-else statement
    if (node->name == "IF_ELSE_STMT") {
        if (node->children.size() < 3) return nextlist;
        
        ExprResult cond = genExpr(node->children[0]);
        
        if (!cond.isBoolean) {
            int idxTrue = emit("if_true " + cond.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            cond.isBoolean = true;
            cond.truelist = makelist(idxTrue);
            cond.falselist = makelist(idxFalse);
        }
        
        std::string Lthen = newLabel();
        std::string Lelse = newLabel();
        std::string Lnext = newLabel();
        
        backpatch(cond.truelist, Lthen);
        emit(Lthen + ":");
        PatchList thenList = genStmt(node->children[1]);
        int idxGotoAfterThen = emit("goto ");
        
        backpatch(cond.falselist, Lelse);
        emit(Lelse + ":");
        PatchList elseList = genStmt(node->children[2]);
        
        backpatch(makelist(idxGotoAfterThen), Lnext);
        backpatch(thenList, Lnext);
        backpatch(elseList, Lnext);
        emit(Lnext + ":");
        
        return nextlist;
    }

    // while loop
    if (node->name == "WHILE_STMT") {
        if (node->children.size() < 2) return nextlist;
        
        std::string Lstart = newLabel();
        std::string Lbody = newLabel();
        std::string Lend = newLabel();
        
        pushLoop(Lend, Lstart);
        
        emit(Lstart + ":");
        ExprResult cond = genExpr(node->children[0]);
        
        if (!cond.isBoolean) {
            int idxTrue = emit("if_true " + cond.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            cond.isBoolean = true;
            cond.truelist = makelist(idxTrue);
            cond.falselist = makelist(idxFalse);
        }
        
        backpatch(cond.truelist, Lbody);
        emit(Lbody + ":");
        PatchList bodyList = genStmt(node->children[1]);
        backpatch(bodyList, Lstart);
        emit("goto " + Lstart);
        
        backpatch(cond.falselist, Lend);
        emit(Lend + ":");
        
        popLoop();
        return nextlist;
    }

    // do-while loop
    if (node->name == "DO_WHILE_STMT") {
        if (node->children.size() < 2) return nextlist;
        
        std::string Lstart = newLabel();
        std::string Lcond = newLabel();
        std::string Lend = newLabel();
        
        pushLoop(Lend, Lcond);
        
        emit(Lstart + ":");
        PatchList bodyList = genStmt(node->children[0]);
        backpatch(bodyList, Lcond);
        
        emit(Lcond + ":");
        ExprResult cond = genExpr(node->children[1]);
        
        if (!cond.isBoolean) {
            int idxTrue = emit("if_true " + cond.place + " != 0 goto ");
            int idxFalse = emit("goto ");
            cond.isBoolean = true;
            cond.truelist = makelist(idxTrue);
            cond.falselist = makelist(idxFalse);
        }
        
        backpatch(cond.truelist, Lstart);
        backpatch(cond.falselist, Lend);
        emit(Lend + ":");
        
        popLoop();
        return nextlist;
    }

    // for loop: for(init; cond; update) body
    if (node->name == "FOR_STMT_2" || node->name == "FOR_STMT_3") {
        std::string Lstart = newLabel();
        std::string Lbody = newLabel();
        std::string Lupdate = newLabel();
        std::string Lend = newLabel();
        
        pushLoop(Lend, Lupdate);
        
        // init
        if (!node->children.empty() && node->children[0]) {
            genStmt(node->children[0]);
        }
        
        emit(Lstart + ":");
        
        // condition (if exists)
        if (node->children.size() > 1 && node->children[1]) {
            ExprResult cond = genExpr(node->children[1]);
            
            if (!cond.isBoolean) {
                int idxTrue = emit("if_true " + cond.place + " != 0 goto ");
                int idxFalse = emit("goto ");
                cond.isBoolean = true;
                cond.truelist = makelist(idxTrue);
                cond.falselist = makelist(idxFalse);
            }
            
            backpatch(cond.truelist, Lbody);
            emit(Lbody + ":");
            
            // body
            Node* bodyNode = node->children.back();
            PatchList bodyList = genStmt(bodyNode);
            backpatch(bodyList, Lupdate);
            
            emit(Lupdate + ":");
            
            // update (if exists for FOR_STMT_3)
            if (node->name == "FOR_STMT_3" && node->children.size() > 3 && node->children[2]) {
                genExpr(node->children[2]);
            }
            
            emit("goto " + Lstart);
            
            backpatch(cond.falselist, Lend);
            emit(Lend + ":");
        } else {
            // infinite loop (no condition)
            emit(Lbody + ":");
            Node* bodyNode = node->children.back();
            PatchList bodyList = genStmt(bodyNode);
            backpatch(bodyList, Lupdate);
            
            emit(Lupdate + ":");
            emit("goto " + Lstart);
            emit(Lend + ":");
        }
        
        popLoop();
        return nextlist;
    }

    // switch statement
    if (node->name == "SWITCH_STMT") {
        if (node->children.size() < 2) return nextlist;
        
        ExprResult switchExpr = genExpr(node->children[0]);
        std::string switchVal = switchExpr.isBoolean ? boolToTemp(switchExpr) : switchExpr.place;
        
        std::string Lend = newLabel();
        pushLoop(Lend, ""); // break works, continue doesn't in switch
        
        // Process switch body
        Node* body = node->children[1];
        
        std::vector<std::pair<std::string, std::string>> cases; // (value, label)
        std::string defaultLabel;
        
        // Helper to find case/default statements
        std::function<void(Node*)> findCases = [&](Node* n) {
            if (!n) return;
            
            if (n->name == "CASE_STMT" && !n->children.empty()) {
                ExprResult caseVal = genExpr(n->children[0]);
                std::string label = newLabel();
                cases.push_back({caseVal.place, label});
            } else if (n->name == "DEFAULT_STMT") {
                defaultLabel = newLabel();
            }
            
            for (auto ch : n->children) findCases(ch);
        };
        
        findCases(body);
        
        // Generate comparison code for each case
        for (size_t i = 0; i < cases.size(); ++i) {
            const std::string &val = cases[i].first;
            const std::string &label = cases[i].second;
            emit("if_true " + switchVal + " == " + val + " goto " + label);
        }
        
        if (!defaultLabel.empty()) {
            emit("goto " + defaultLabel);
        } else {
            emit("goto " + Lend);
        }
        
        // Generate body with labels
        std::function<PatchList(Node*)> genSwitchBody = [&](Node* n) -> PatchList {
            PatchList list;
            if (!n) return list;
            
            if (n->name == "CASE_STMT") {
                // Find matching label
                if (!n->children.empty()) {
                    ExprResult caseVal = genExpr(n->children[0]);
                    for (size_t i = 0; i < cases.size(); ++i) {
                        const std::string &val = cases[i].first;
                        const std::string &label = cases[i].second;
                        if (val == caseVal.place) {
                            emit(label + ":");
                            break;
                        }
                    }
                }
                // Generate statements after case label
                if (n->children.size() > 1) {
                    return genStmt(n->children[1]);
                }
            } else if (n->name == "DEFAULT_STMT") {
                emit(defaultLabel + ":");
                if (!n->children.empty()) {
                    return genStmt(n->children[0]);
                }
            } else {
                for (auto ch : n->children) {
                    PatchList nl = genSwitchBody(ch);
                    list = merge(list, nl);
                }
            }
            return list;
        };
        
        PatchList bodyList = genSwitchBody(body);
        backpatch(bodyList, Lend);
        emit(Lend + ":");
        
        popLoop();
        return nextlist;
    }

    // expression statements
    if (node->name == "ASSIGN_EXPR" || node->name == "FUNC_CALL" || 
        node->name == "ADD_EXPR" || node->name == "SUB_EXPR" ||
        node->name == "MUL_EXPR" || node->name == "DIV_EXPR") {
        genExpr(node);
        return nextlist;
    }

    // declarations (process initializers)
    if (node->name == "DECLARATION" || node->name == "INIT_DECL_LIST") {
        for (auto c : node->children) {
            genStmt(c);
        }
        return nextlist;
    }
    
    // Handle INIT_DECL_LIST with ASSIGN_EXPR (declarations with initializers)
    if (node->name == "INIT_DECL_LIST") {
        for (auto c : node->children) {
            if (c && c->name == "ASSIGN_EXPR") {
                genStmt(c); // This will handle the assignment
            }
        }
        return nextlist;
    }

    // generic: process all children
    for (auto c : node->children) {
        PatchList nl = genStmt(c);
        nextlist = merge(nextlist, nl);
    }
    
    return nextlist;
}

void IRGenerator::printToStdout() const {
    for (auto &s : code) std::cout << s << std::endl;
}

bool IRGenerator::writeToFile(const std::string& path) const {
    std::ofstream out(path);
    if (!out) return false;
    for (auto &s : code) out << s << std::endl;
    out.close();
    return true;
}

IRGenerator::PatchList IRGenerator::makelist(int instrIndex) {
    return PatchList(1, instrIndex);
}

IRGenerator::PatchList IRGenerator::merge(const PatchList &a, const PatchList &b) {
    PatchList r = a; 
    r.insert(r.end(), b.begin(), b.end()); 
    return r;
}

void IRGenerator::backpatch(const PatchList &list, const std::string &label) {
    for (int idx : list) {
        if (idx >= 0 && idx < (int)code.size()) {
            code[idx] += label;
        }
    }
}

void IRGenerator::genArrayInitialization(const std::string& arrayName, Node* initList, int baseOffset) {
    if (!initList || initList->name != "INIT_LIST") return;
    
    int elementIndex = 0;
    for (auto child : initList->children) {
        if (!child) continue;
        
        if (child->name == "INIT_LIST") {
            // Nested initialization list (multi-dimensional array)
            // For 2D arrays, calculate proper offset: row * cols + col
            int rowSize = 3; // This should be determined from array dimensions
            genArrayInitialization(arrayName, child, baseOffset + elementIndex * rowSize);
            elementIndex++;
        } else if (child->name == "INTEGER_CONSTANT" || child->name == "FLOAT_CONSTANT" || 
                   child->name == "CHAR_CONSTANT" || child->name == "STRING_LITERAL") {
            // Simple constant initialization
            std::string target = arrayName + "[" + std::to_string(baseOffset + elementIndex) + "]";
            emit(target + " = " + child->lexeme);
            elementIndex++;
        } else {
            // Complex expression initialization
            ExprResult expr = genExpr(child);
            if (!expr.place.empty()) {
                std::string target = arrayName + "[" + std::to_string(baseOffset + elementIndex) + "]";
                std::string value = expr.isBoolean ? boolToTemp(expr) : expr.place;
                emit(target + " = " + value);
                elementIndex++;
            }
        }
    }
}