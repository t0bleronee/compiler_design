#include "symbol_table.h"

using namespace std;

// Constructor: start with a global scope
SymbolTable::SymbolTable() {

enterScope();
}


void SymbolTable::enterScope() {
    scopes.push_back(std::map<std::string, Symbol*>());
}

void SymbolTable::exitScope() {
    if (!scopes.empty()) {
        for (const auto& pair : scopes.back()) {
            cout << pair.first << " " ;
        }
        cout << "\n";
        Scopeh s;
        s.level = scopes.size();
        s.symbols = scopes.back();
        scopeHistory.push_back(s);
        scopes.pop_back();
        // Debug: print scope levels
        // cout << "Exited scope, now at level " << scopes.size() << ", history = " << scopeHistory.size() << "\n";
    }
}
bool SymbolTable::addSymbol(const string& name, const string& type, Node* node, 
                            bool isFunction, const vector<string>& params,
                            bool isArray, const vector<int>& arrayDims, 
                            int pointerDepth, bool isStruct, bool isEnum, 
                            bool isUnion, bool isTypedef, string aliasedType ,bool isStatic, bool isExtern, bool isVariadic) {
    auto& current = scopes.back();
    
    // Check if already exists
    if (current.find(name) != current.end()) {
        if (isFunction) {
            Symbol* existing = current[name];
            if (existing->paramTypes == params) {
                return false;
            }
            return false;
        }
        return false;
    }
    
  unique_ptr<Symbol> sym(new Symbol(name, type, node, isFunction));
    sym->paramTypes = params;
    sym->isArray = isArray;
    sym->arrayDimensions = arrayDims;
    sym->pointerDepth = pointerDepth;
    sym->isStruct = isStruct;
    sym->isEnum = isEnum;
    sym->isUnion = isUnion;
    sym->isTypedef = isTypedef;
    sym->aliasedType = aliasedType;
        sym->isStatic = isStatic;      // ✅ NEW
    sym->isExtern = isExtern;      // ✅ NEW
    sym->isVariadic = isVariadic;      // ✅ NEW: Set variadic flag
        // Reference flags default (can be set by semantic analyzer after add)
        sym->isReference = false;
    
    // ✅ Get the pointer BEFORE moving
    Symbol* symPtr = sym.get();
    
    // ✅ Store in permanent storage
    allSymbols.push_back(move(sym));
    
    // ✅ Store pointer in current scope
    current[name] = symPtr;
    
    return true;
}

// Lookup symbol in all scopes (from inner to outer)

Symbol* SymbolTable::lookup(const std::string& name) {
    // Traverse from innermost (back) to outermost (front)
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return found->second; // ✅ return actual symbol pointer
        }
    }
    return nullptr;
}

Symbol* SymbolTable::lookuph(const std::string& name) {
    // Traverse from innermost (back) to outermost (front)
    for (auto it = scopeHistory.rbegin(); it != scopeHistory.rend(); ++it) {
        auto found = it->symbols.find(name);  // ✅ access the map inside Scopeh
        if (found != it->symbols.end()) {     // ✅ compare with map's end()
            return found->second;            // return pointer to Symbol
        }
    }
    return nullptr; // not found
}


Symbol* SymbolTable::lookupCurrentScope(const std::string& name) {
    if (scopes.empty()) return nullptr;
    auto& current = scopes.back(); // was top()
    auto it = current.find(name);
    if (it != current.end()) return it->second;
    return nullptr;
}


// Add an error to the list
void SymbolTable::addError(const string& error) {
    errors.push_back(error);
    cout << "ERROR: " << error << endl;
}

// Print all collected errors
void SymbolTable::printErrors() const {
    cout << "\n=== Semantic Errors ===" << endl;
    for (const auto& e : errors) {
        cout << "  ❌ " << e << endl;
    }
}

void SymbolTable::printAllScopes() const {
    std::cout << "\n=== Symbol Table ===\n";
    
    // Print in reverse order (most nested scopes first)
    for (auto it = scopeHistory.rbegin(); it != scopeHistory.rend(); ++it) {
        const Scopeh& s = *it;
        std::cout << "Scope Level " << s.level << ":\n";
        for (const auto &p : s.symbols) {
        
        // Suppose p.second.type is something like "int***"
std::string type = p.second->type;

// 1️⃣ Count trailing '*'
size_t starCount = 0;
for (size_t i = type.size(); i > 0; --i) {
    if (type[i - 1] == '*')
        starCount++;
    else
        break;
}

// 2️⃣ Remove those stars to get the base type
std::string baseType = type.substr(0, type.size() - starCount);

            std::cout << "  " << p.second->name << " [" << p.second->type;
              // Print struct members if it's a struct type
            if (p.second->isStruct && !p.second->structMembers.empty()) {
                std::cout << " { ";
                for (const auto& member : p.second->structMembers) {
                    std::cout << member.first << ":" << member.second << " ";
                }
                std::cout << "}";
            }
            
            // Print enum values if it's an enum type
            if (p.second->isEnum && !p.second->enumValues.empty()) {
                std::cout << " { ";
                for (const auto& value : p.second->enumValues) {
                    std::cout << value.first << "=" << value.second << " ";
                }
                std::cout << "}";
            }
            // Print pointer depth
            for (int i = 0; i < p.second->pointerDepth; i++) {
                std::cout << "*";
            }
            
            // Print array dimensions
            if (p.second->isArray) {
                for (int dim : p.second->arrayDimensions) {
                    std::cout << "[";
                    if (dim != -1) {
                        std::cout << dim;
                    }
                    std::cout << "]";
                }
            }
            
            std::cout << "]";
            
            // Print function info
            if (p.second->isFunction) {
                std::cout << " (function)";
                if (!p.second->paramTypes.empty()) {
                    std::cout << " params:(";
                    for (size_t i = 0; i < p.second->paramTypes.size(); i++) {
                        std::cout << p.second->paramTypes[i];
                        if (i < p.second->paramIsReference.size() && p.second->paramIsReference[i]) {
                            std::cout << "&"; // annotate reference params for debug
                        }
                        if (i < p.second->paramTypes.size() - 1) std::cout << ", ";
                    }
                    std::cout << ")";
                }
                if (p.second->isVariadic) {
                    std::cout << " variadic";
                }
            }
            // Print function pointer info (debug)
            if (p.second->isFunctionPointer) {
                std::cout << " (funcptr -> " << p.second->funcPtrReturnType << "(";
                for (size_t i = 0; i < p.second->funcPtrParamTypes.size(); ++i) {
                    std::cout << p.second->funcPtrParamTypes[i];
                    if (i < p.second->funcPtrParamIsReference.size() && p.second->funcPtrParamIsReference[i]) {
                        std::cout << "&";
                    }
                    if (i + 1 < p.second->funcPtrParamTypes.size()) std::cout << ", ";
                }
                if (p.second->funcPtrIsVariadic) std::cout << ", ...";
                std::cout << "))";
            }
            
            std::cout << "\n";
        }
    }
}

void SymbolTable::printSymbol(const Symbol& sym) const {
    std::cout << "  " << sym.name << " [" << sym.type;
    
    // Print pointer depth
    for (int i = 0; i < sym.pointerDepth; i++) {
        std::cout << "*";
    }
    
    // Print array dimensions
    if (sym.isArray) {
        for (int dim : sym.arrayDimensions) {
            std::cout << "[";
            if (dim != -1) {
                std::cout << dim;
            }
            std::cout << "]";
        }
    }
    
    // Print enum values
    if (sym.isEnum && !sym.enumValues.empty()) {
        std::cout << " { ";
        for (const auto& value : sym.enumValues) {
            std::cout << value.first << "=" << value.second << " ";
        }
        std::cout << "}";
    }
    
    // Print struct members
    if (sym.isStruct && !sym.structMembers.empty()) {
        std::cout << " { ";
        for (const auto& member : sym.structMembers) {
            std::cout << member.first << ":" << member.second << " ";
        }
        std::cout << "}";
    }
    
    std::cout << "]\n";
}
