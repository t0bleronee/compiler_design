#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <string>
#include <vector>
#include <map>
#include <stack>
#include <iostream>
#include "ast.h"   // Your Node definition
#include <memory>



class Symbol {
public:
    std::string name;
    std::string type;       // Base type: "int", "float", etc.
    Node* node;
    bool isFunction;
    std::vector<std::string> paramTypes;
    bool isVariadic = false; // Function accepts variable arguments (has ...)
    // Reference support
    bool isReference = false;               // This symbol denotes a reference (holds an address)
    std::vector<bool> paramIsReference;     // For function symbols: per-parameter reference flags
    
    // Function-pointer support
    bool isFunctionPointer = false;                     // This symbol is a pointer to function
    std::string funcPtrReturnType;                      // Return type of the function this pointer points to
    std::vector<std::string> funcPtrParamTypes;         // Parameter types of the function pointer
    std::vector<bool> funcPtrParamIsReference;          // Reference flags per parameter for function pointer
    bool funcPtrIsVariadic = false;                     // Variadic flag for function pointer
    
    // ADD THESE:
    bool isArray;           // Is this an array?
   std::vector<int> arrayDimensions;         // Size if known, -1 if unknown/dynamic
    int pointerDepth;       // 0 = not pointer, 1 = *, 2 = **, etc.
    // If this symbol is a pointer and the pointee is an array (e.g., int (*p)[3][3]),
    // capture the array dimensions of the pointee here. This helps pointer arithmetic
    // scale correctly by the full aggregate size.
    std::vector<int> pointeeArrayDimensions;
    
    
    // After pointerDepth, add:
bool isStruct;
bool isEnum;
bool isUnion;  

bool isTypedef = false;        
   std:: string aliasedType ; 
    
    
std::map<std::string, std::string> structMembers;  // member name -> type
std::vector<std::string> structMemberOrder;        // declaration order of members
std::map<std::string, int> enumValues;             // enumerator name -> value
 bool isStatic = false;           // ✅ NEW: Track if symbol has static storage
    bool isExtern = false;           // ✅ NEW: For extern declarations
    bool isLocal = false;            // ✅ NEW: Track if symbol is local to current function
 
    
    
     Symbol() : name(""), type(""), node(nullptr), isFunction(false), 
         isArray(false), pointerDepth(0), isStruct(false), isEnum(false),isUnion(false) ,isTypedef (false), aliasedType(""), isReference(false),
         isFunctionPointer(false), funcPtrReturnType(""), funcPtrIsVariadic(false), isLocal(false) {}
           
Symbol(std::string n, std::string t, Node* nd, bool func=false)
    : name(n), type(t), node(nd), isFunction(func), 
    isArray(false), pointerDepth(0), isStruct(false), isEnum(false),isUnion(false) , isTypedef (false), aliasedType(""), isReference(false),
    isFunctionPointer(false), funcPtrReturnType(""), funcPtrIsVariadic(false), isLocal(false) {}
};

struct Scopeh {
    int level;
    std::map<std::string, Symbol*> symbols;
};
// Symbol table class
class SymbolTable {
private:
    std::vector<std::map<std::string, Symbol*>> scopes;  // Stack of scopes
    std::vector<std::string> errors;                  // Semantic errors
     std::vector<Scopeh> scopeHistory;
     std::vector<std::unique_ptr<Symbol>> allSymbols;

public:
    SymbolTable();                       
    void enterScope();                      // Enter a new block/function scope
    void exitScope();                       // Exit current scope
    bool addSymbol(const std::string& name, const std::string& type, Node* node, 
               bool isFunction=false, const std::vector<std::string>& params = {},
               bool isArray=false, const std::vector<int>& arrayDims = {}, 
               int pointerDepth=0, bool isStruct=false, bool isEnum=false, bool isUnion=false , bool isTypedef = false, std::string aliasedType = "",bool isStatic=false,bool isExtern=false, bool isVariadic=false);
    Symbol* lookup(const std::string& name);   
     Symbol* lookuph(const std::string& name);   
    Symbol* lookupCurrentScope(const std::string& name); 
    void addError(const std::string& error);
    void printErrors() const;
    bool hasErrors() const { return !errors.empty(); }
    void printAllScopes() const;
    void printSymbol(const Symbol& sym) const ;
int getCurrentScopeLevel() const {
    return scopes.size();
}
bool isEmptyScopes() const {
    return scopes.empty();
}
};

#endif

