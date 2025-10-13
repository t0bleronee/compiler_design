#ifndef AST_H
#define AST_H

#include <string>
#include <vector>
#include <fstream>
#include <iostream>

class Node {
public:
    std::string name;
    std::string lexeme = "default";
    std::string type = "default";
    std::vector<Node*> children;
    Node* parent;
    bool processed = false;
    // Constructors
    //Node(const std::string &name) : name(name) {}
    Node(const std::string &name_, const std::string &lexeme_ = "")
        : name(name_), lexeme(lexeme_) , parent(nullptr) {}
   
    void addChild(Node* child);
    void addChild(const std::string &childName);
    void addChild(const std::string &childName, const std::string &childLexeme);

    // Print / debug
    void printTree(int depth = 0, std::string prefix = "");
    void generateDOT(std::ofstream &out, int &nodeId);

    // Type helpers
    void setType(const std::string &type) { this->type = type; }
    std::string getType() const { return type; }
    std::string getLexeme() const { return lexeme; }
};

extern Node* root;

// Type helper stubs
void setDeclarationType(Node* node, const std::string &type);
std::string setDeclaratorType(Node* node, const std::string &type, bool isFunctionDefinition, int isVirtual = 0);
std::string setDirectDeclaratorType(Node* node, const std::string &type, bool isFunctionDefinition, int isVirtual = 0);
std::string setAbstractDirectDeclaratorType(Node* node, const std::string &type);
std::string setAbstractDeclaratorType(Node* node, const std::string &type);

#endif // AST_H

