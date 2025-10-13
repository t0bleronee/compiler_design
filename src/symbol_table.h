#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <string>
#include <vector>
#include <map>
#include <stack>
#include <iostream>
#include "ast.h"   // Your Node definition



class Symbol {
public:
    std::string name;
    std::string type;       // Base type: "int", "float", etc.
    Node* node;
    bool isFunction;
    std::vector<std::string> paramTypes;
    
    // ADD THESE:
    bool isArray;           // Is this an array?
   std::vector<int> arrayDimensions;         // Size if known, -1 if unknown/dynamic
    int pointerDepth;       // 0 = not pointer, 1 = *, 2 = **, etc.
    
    
    // After pointerDepth, add:
bool isStruct;
bool isEnum;


bool isTypedef = false;        // NEW: Is this a typedef alias?
   std:: string aliasedType ; 
    
    
std::map<std::string, std::string> structMembers;  // member name -> type
std::map<std::string, int> enumValues;             // enumerator name -> value

    
   Symbol() : name(""), type(""), node(nullptr), isFunction(false), 
           isArray(false), pointerDepth(0), isStruct(false), isEnum(false) ,isTypedef (false), aliasedType("") {}
           
Symbol(std::string n, std::string t, Node* nd, bool func=false)
    : name(n), type(t), node(nd), isFunction(func), 
      isArray(false), pointerDepth(0), isStruct(false), isEnum(false),isTypedef (false), aliasedType("") {}
};

struct Scopeh {
    int level;
    std::map<std::string, Symbol> symbols;
};
// Symbol table class
class SymbolTable {
private:
    std::vector<std::map<std::string, Symbol>> scopes;  // Stack of scopes
    std::vector<std::string> errors;                  // Semantic errors
     std::vector<Scopeh> scopeHistory;
   

public:
    SymbolTable();                       
    void enterScope();                      // Enter a new block/function scope
    void exitScope();                       // Exit current scope
    bool addSymbol(const std::string& name, const std::string& type, Node* node, 
               bool isFunction=false, const std::vector<std::string>& params = {},
               bool isArray=false, const std::vector<int>& arrayDims = {}, 
               int pointerDepth=0, bool isStruct=false, bool isEnum=false, bool isTypedef = false, std::string aliasedType = "");
    Symbol* lookup(const std::string& name);        // Lookup in all scopes
    Symbol* lookupCurrentScope(const std::string& name); // Lookup only current scope
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

