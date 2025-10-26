#include "ast.h"
#include "symbol_table.h"
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


void Node::generateDOT(std::ofstream& out, int& counter) {
    int nodeId = counter++;
    
    // Escape the label content properly
    std::string label = name;
    if (!lexeme.empty()) {
        label += "\\n" + lexeme;
    }
    
    // Replace all problematic characters
    std::string escapedLabel;
    for (char c : label) {
        switch (c) {
            case '"': escapedLabel += "\\\""; break;
            case '\\': escapedLabel += "\\\\"; break;
            case '\n': escapedLabel += "\\n"; break;
            case '\r': escapedLabel += "\\r"; break;
            case '\t': escapedLabel += "\\t"; break;
            case '<': escapedLabel += "&lt;"; break;
            case '>': escapedLabel += "&gt;"; break;
            case '{': escapedLabel += "\\{"; break;
            case '}': escapedLabel += "\\}"; break;
            case '|': escapedLabel += "\\|"; break;
            case '[': escapedLabel += "\\["; break;
            case ']': escapedLabel += "\\]"; break;
            default: escapedLabel += c;
        }
    }
    
    out << "    " << nodeId << " [label=\"" << escapedLabel << "\"];" << std::endl;
    
    for (auto child : children) {
        if (child) {
            int childId = counter;
            child->generateDOT(out, counter);
            out << "    " << nodeId << " -> " << childId << ";" << std::endl;
        }
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

