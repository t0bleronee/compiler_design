#include "ast.h"
#include <iostream>
#include <fstream>
using namespace std;


 void Node::addChild(Node* child) {
        if (child) {
          
            child->parent = this;  
            children.push_back(child);
          
        }
    }
// Add a child by name only (no lexeme)
/*void Node::addChild(const std::string &childName) {
    Node* child = new Node(childName);
    children.push_back(child);
}*/

// Add a child by name + lexeme (for operators / identifiers)
void Node::addChild(const std::string &childName, const std::string &childLexeme) {
    Node* child = new Node(childName, childLexeme);
    children.push_back(child);
}

// Recursively print the AST
void Node::printTree(int depth, string prefix) {
    for (int i = 0; i < depth; i++) cout << "  ";
    cout << prefix << name;
    if (lexeme != "default") cout << " : " << lexeme;
    if (type != "default") cout << " [" << type << "]";
    cout << endl;

    for (auto child : children) {
        child->printTree(depth + 1);
    }
}

// Generate DOT format for Graphviz
void Node::generateDOT(ofstream& out, int& nodeId) {
    int currentId = nodeId++;
    out << "  node" << currentId << " [label=\"" << name;
    if (lexeme != "default") out << "\\n" << lexeme;
    if (type != "default") out << "\\n[" << type << "]";
    out << "\"];" << endl;

    for (auto child : children) {
        int childId = nodeId;
        child->generateDOT(out, nodeId);
        out << "  node" << currentId << " -> node" << childId << ";" << endl;
    }
}

// Type helper stubs
void setDeclarationType(Node* node, const std::string &type) {
    if (node) node->setType(type);
}

std::string setDeclaratorType(Node* node, const std::string &type, bool, int) {
    if (node) node->setType(type);
    return type;
}

std::string setDirectDeclaratorType(Node* node, const std::string &type, bool, int) {
    if (node) node->setType(type);
    return type;
}

std::string setAbstractDirectDeclaratorType(Node* node, const std::string &type) {
    if (node) node->setType(type);
    return type;
}

std::string setAbstractDeclaratorType(Node* node, const std::string &type) {
    if (node) node->setType(type);
    return type;
}

