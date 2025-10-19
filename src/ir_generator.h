#ifndef IR_GENERATOR_H
#define IR_GENERATOR_H

#include <string>
#include <vector>
#include <map>
#include <fstream>
#include "ast.h"
#include "symbol_table.h"

// Simple 3-address code IR generator with backpatching support
class IRGenerator {
public:
    IRGenerator(SymbolTable& symtab);
    void generate(Node* root);
    void printToStdout() const;
    bool writeToFile(const std::string& path) const;

private:
    SymbolTable& symtab;
    std::vector<std::string> code;
    int tempCounter;
    int labelCounter;

    std::string newTemp();
    std::string newLabel();
    int emit(const std::string& instr);

    // Traversal / generation
    struct ExprResult {
        std::string place;            // temporary or variable name
        bool isBoolean = false;       // true if expression yields true/false lists
        std::vector<int> truelist;    // instruction indices to patch when true
        std::vector<int> falselist;   // instruction indices to patch when false
    };

    // genExpr returns ExprResult (with place for non-boolean exprs,
    // and truelist/falselist for boolean ones).
    ExprResult genExpr(Node* node);

    // genStmt returns a nextlist: list of instruction indices that should be
    // patched to the next instruction after this statement.
    using PatchList = std::vector<int>;
    PatchList genStmt(Node* node);
    void genFunction(Node* node);

    // Backpatching helpers
    PatchList makelist(int instrIndex);
    PatchList merge(const PatchList &a, const PatchList &b);
    void backpatch(const PatchList &list, const std::string &label);
    
    // Convert a boolean ExprResult (truelist/falselist) into a temporary with 0/1
    std::string boolToTemp(ExprResult &e);
    
    // Stack for break/continue labels
    struct LoopContext {
        std::string breakLabel;
        std::string continueLabel;
    };
    std::vector<LoopContext> loopStack;
    
    void pushLoop(const std::string& breakLbl, const std::string& continueLbl);
    void popLoop();
    LoopContext* currentLoop();
    
    // Array initialization helper
    void genArrayInitialization(const std::string& arrayName, Node* initList, int baseOffset);
};

#endif // IR_GENERATOR_H