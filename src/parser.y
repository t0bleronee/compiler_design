%{
#include <cstdio>
#include <cstdlib>
#include <string>
#include <cstring>
#include <set>
#include "symbol_table.h"
#include "ast.h"
#include "semantic_analyzer.h"
#include "ir_generator.h"
#include <iostream>
#include "errors.h"
#include <unordered_set>
#include <functional>
using namespace std;

extern int yylex();
extern int yyparse();
extern FILE* yyin;
extern int line_num;
extern int col_num;
void yyerror(const char* msg);


extern std::unordered_set<std::string> typedef_names;



Node* root = nullptr;


// Helper function to extract identifier name from declarator tree
std::string getDeclaratorName(Node* node) {
    if (!node) return "";
    
    // Check if this is an IDENTIFIER node
    if (node->name == "IDENTIFIER") {
        return node->lexeme;  // Use lexeme field, not value
    }
    
    // Recurse through children to find IDENTIFIER
    for (auto child : node->children) {
        std::string name = getDeclaratorName(child);
        if (!name.empty()) return name;
    }
    
    return "";
}

// Check if declaration_specifiers contains TYPEDEF (non-recursive, only direct children)
bool hasTypedefSpecifier(Node* declSpec) {
    if (!declSpec) {
        printf("DEBUG hasTypedefSpecifier: NULL node\n");
        return false;
    }
    
    
    // Check if this node itself is TYPEDEF
    if (declSpec->name == "TYPEDEF") {
        printf("DEBUG hasTypedefSpecifier: Found TYPEDEF at root\n");
        return true;
    }
    
    // Only check DIRECT children, don't recurse further
    for (size_t i = 0; i < declSpec->children.size(); i++) {
        Node* child = declSpec->children[i];
        
        if (child && child->name == "TYPEDEF") {
            printf("DEBUG hasTypedefSpecifier: Found TYPEDEF in child[%zu]\n", i);
            return true;
        }
        
        // Check if child points back to parent (circular reference detection)
        if (child == declSpec) {
            printf("ERROR: Circular reference detected! child[%zu] points to parent!\n", i);
        }
    }
    
    printf("DEBUG hasTypedefSpecifier: No TYPEDEF found\n");
    return false;
}

// Register typedef names from init_declarator_list
void registerTypedefNames(Node* declSpec, Node* initDeclList) {
    // Check if this is a typedef declaration
    if (!hasTypedefSpecifier(declSpec) || !initDeclList) {
        return;
    }
    
    // Safety check: ensure initDeclList has children
    if (initDeclList->children.empty()) {
        return;
    }
    
    // Process each declarator in the init_declarator_list
    for (auto assignExpr : initDeclList->children) {
        if (!assignExpr) continue;  // Safety check for null pointer
        
        if (assignExpr->name == "ASSIGN_EXPR" && !assignExpr->children.empty()) {
            // First child is the declarator (LHS of assignment)
            Node* declarator = assignExpr->children[0];
            
            if (!declarator) continue;  // Safety check
            
            std::string typeName = getDeclaratorName(declarator);
            
            if (!typeName.empty()) {
                typedef_names.insert(typeName);
                printf("TYPEDEF REGISTERED: '%s'\n", typeName.c_str());
            }
        }
    }
}

%}

%union {
    int intval;
    float floatval;
    char* strval;
   Node* node;
}
// Token declarations from lexer
%token <strval> IDENTIFIER STRING_LITERAL CHAR_LITERAL
%token <strval> INTEGER_CONSTANT FLOAT_CONSTANT
%token <strval> HEX_FLOAT_LITERAL HEX_INT_LITERAL OCTAL_INT_LITERAL BINARY_INT_LITERAL
%token <strval> BOOL_LITERAL CONST

// Keywords
%token<node> AUTO ARRAY BOOL BREAK CASE CATCH CHAR CLASS CONSTEXPR 
%token<node> COUT CIN ENDL CONTINUE DEFAULT DELETE DO DOUBLE ENUM EXPLICIT FLOAT
%token<node> FOR FRIEND GOTO INLINE INT LONG NAMESPACE NEW NULLPTR
%token<node> OVERRIDE PRIVATE PUBLIC PROTECTED RETURN SHORT SIGNED SIZEOF STATIC STRUCT SWITCH SCANF PRINTF
%token<node> TEMPLATE THIS THROW TRY TYPEDEF TYPEID TYPENAME UNION
%token<node> UNSIGNED USING VECTOR VIRTUAL VOID WHILE
%token<strval> TYPE_NAME

// Operators
%token<node> AND OR NOT XOR BITAND BITOR COMPL
%token<node> AND_EQ OR_EQ XOR_EQ NOT_EQ
%token<node> ARROW_OPERATOR SCOPE_OPERATOR INC_OP DEC_OP
%token<node> ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token<node> LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN
%token<node> EQ_OP NE_OP GE_OP LE_OP AND_OP OR_OP LEFT_OP RIGHT_OP
%token<node> PLUS MINUS ASTERISK_OPERATOR DIVIDE PERCENT GT LT ASSIGN
%token<node> AMP PIPE CARET TILDE BANG
%token<node> SEMICOLON QUESTIONMARK COLON COMMA DOT ELLIPSIS
%token<node> LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token<node> UNTIL DONE


%type <node> program
%type <node> translation_unit
%type <node> external_declaration
%type <node> using_directive
%type <node> function_definition

%type <node> declaration
%type <node> declaration_list
%type <node> declaration_specifiers
%type <node> init_declarator_list
%type <node> init_declarator

%type <node> type_specifier
%type <node> storage_class_specifier
%type <node> type_qualifier
%type <node> type_qualifier_list
%type <node> specifier_qualifier_list

%type <node> struct_or_union_specifier
%type <node> struct_or_union
%type <node> struct_declaration_list
%type <node> enum_specifier
%type <node> enumerator_list
%type <node> enumerator

%type <node> declarator
%type <node> direct_declarator
%type <node> pointer
%type <node> abstract_declarator
%type <node> direct_abstract_declarator

%type <node> parameter_type_list
%type <node> parameter_list
%type <node> parameter_declaration
%type <node> identifier_list

%type <node> initializer
%type <node> initializer_list

%type <node> compound_statement
%type <node> block_item_list
%type <node> block_item
%type <node> statement
%type <node> labeled_statement
%type <node> expression_statement
%type <node> selection_statement
%type <node> iteration_statement
%type <node> jump_statement

%type <node> switch_body
%type <node> case_list_opt
%type <node> case_list
%type <node> case_element
%type <node> statement_list

%type <node> for_init_statement
%type <node> range_based_for

%type <node> expression
%type <node> assignment_expression
%type <node> assignment_operator
%type <node> conditional_expression
%type <node> logical_or_expression
%type <node> logical_and_expression
%type <node> inclusive_or_expression
%type <node> exclusive_or_expression
%type <node> and_expression
%type <node> equality_expression
%type <node> relational_expression
%type <node> shift_expression
%type <node> additive_expression
%type <node> multiplicative_expression
%type <node> cast_expression
%type <node> unary_expression
%type <node> unary_operator
%type <node> postfix_expression
%type <node> primary_expression
%type <node> argument_expression_list
%type <node> constant_expression

%type <node> constant
%type <node> printf_stmt
%type <node> scanf_stmt
%type <node> variable_list
%type <node> type_name





// Fix precedence conflicts
%nonassoc IF
%nonassoc ELSE
%nonassoc RPAREN


// Operator precedence and associativity (corrected order)
%right ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN
%left COMMA
%right QUESTIONMARK COLON
%left OR_OP
%left AND_OP
%left PIPE
%left CARET
%left AMP
%left EQ_OP NE_OP NOT_EQ
%left LT LE_OP GT GE_OP
%left LEFT_OP RIGHT_OP
%left PLUS MINUS
%left ASTERISK_OPERATOR DIVIDE PERCENT
%right BANG TILDE INC_OP DEC_OP SIZEOF unary_minus unary_plus
%left ARROW_OPERATOR DOT LBRACKET LPAREN

%start program

%%

program
    : translation_unit
    {
    root = $1;
  
    }
    ;


translation_unit
    : external_declaration
        { $$ = $1; }   // Only one child
    | translation_unit external_declaration
        { 
            $1->addChild($2);  // Add new declaration as child
            $$ = $1;
        }
    ;
external_declaration
    : function_definition
        {
            $$ = new Node("FUNCTION_DEFINITION"); 
            $$->addChild($1);
            printf("PARSE: Found function definition\n");
        }
    | declaration
        {
            $$ = new Node("GLOBAL_DECLARATION"); 
            $$->addChild($1);
            printf("PARSE: Found global declaration\n");
        }
    | using_directive
        { 
            $$ = $1; 
        }
    | error 
        { 
            yyerrok; 
            $$ = new Node("ERROR"); 
        }
    ;

using_directive
    : USING NAMESPACE IDENTIFIER SEMICOLON
    {
        $$ = new Node("USING_DIRECTIVE");
        Node* nsNode = new Node("NAMESPACE", std::string($3));
        $$->addChild(nsNode);
        printf("PARSE: Using directive for namespace %s\n", $3);
        
    }
    ;



function_definition
    : declaration_specifiers declarator declaration_list compound_statement
        {
            $$ = new Node("FUNCTION_DEFINITION");
            Node* declSpecNode = new Node("DECL_SPECIFIERS");
            declSpecNode->addChild($1);        // return type
            $$->addChild(declSpecNode);
            $$->addChild($2);                  // declarator / function name
            $$->addChild($3);                  // declaration list (parameters/globals inside)
            $$->addChild($4);                  // function body
        }
    | declaration_specifiers declarator compound_statement
        {
            $$ = new Node("FUNCTION_DEFINITION");
            Node* declSpecNode = new Node("DECL_SPECIFIERS");
            declSpecNode->addChild($1);
            $$->addChild(declSpecNode);
            $$->addChild($2);
            $$->addChild($3);
        }
    | declarator declaration_list compound_statement
        {
            $$ = new Node("FUNCTION_DEFINITION");
            $$->addChild($1);
            $$->addChild($2);
            $$->addChild($3);
        }
    | declarator compound_statement
        {
            $$ = new Node("FUNCTION_DEFINITION");
            $$->addChild($1);
            $$->addChild($2);
        }
    ;


declaration_list
    : declaration
        { 
            $$ = new Node("DECLARATION_LIST"); 
            $$->addChild($1); 
        }
    | declaration_list declaration
        { 
            $1->addChild($2); 
            $$ = $1; 
        }
    ;


declaration
    : declaration_specifiers SEMICOLON
        { 
            $$ = new Node("DECLARATION");
            $$->addChild($1);
        }
    | declaration_specifiers init_declarator_list SEMICOLON
        {
            $$ = new Node("DECLARATION");
            $$->addChild($1);   // type specifiers
            $$->addChild($2);   // declarators
            registerTypedefNames($1, $2);
     
        
           
        }
    ;
    
declaration_specifiers
    : storage_class_specifier
        { 
            $$ = new Node("DECL_SPECIFIERS");
            $$->addChild($1);
        }
    | type_specifier
        { 
            $$ = new Node("DECL_SPECIFIERS");
            $$->addChild($1);
        }
    | type_qualifier
        { 
            $$ = new Node("DECL_SPECIFIERS");
            $$->addChild($1);
        }
    | storage_class_specifier declaration_specifiers
        {
          $$ = $2;
$$->addChild($1); 

        }
    | type_specifier declaration_specifiers
        {
            $$ = $2;
            $$->addChild($1);       
        }
    | type_qualifier declaration_specifiers
        {
            $$ = $2;
            $$->addChild($1);  
        }
    ;

init_declarator_list
    : init_declarator
        { 
            $$ = new Node("INIT_DECL_LIST"); 
            $$->addChild($1); 
        }
    | init_declarator_list COMMA init_declarator
        { 
            $1->addChild($3); 
            $$ = $1; 
        }
    ;



init_declarator
    : declarator
        {
            $$ = new Node("ASSIGN_EXPR");
            $$->addChild($1);                  // LHS
            $$->addChild(new Node("OP", "=")); // Operator
            $$->addChild(new Node("EMPTY"));   // RHS is empty
        }
    | declarator ASSIGN initializer
        {
            $$ = new Node("ASSIGN_EXPR");
            $$->addChild($1);                  // LHS
            $$->addChild(new Node("OP", "=")); // Operator
            $$->addChild($3);                  // RHS
        }
;


type_specifier
    : VOID      { $$ = new Node("TYPE_SPECIFIER", "void"); }
    | CHAR      { $$ = new Node("TYPE_SPECIFIER", "char"); }
    | SHORT     { $$ = new Node("TYPE_SPECIFIER", "short"); }
    | INT       { $$ = new Node("TYPE_SPECIFIER", "int"); }
    | LONG      { $$ = new Node("TYPE_SPECIFIER", "long"); }
    | FLOAT     { $$ = new Node("TYPE_SPECIFIER", "float"); }
    | DOUBLE    { $$ = new Node("TYPE_SPECIFIER", "double"); }
    | SIGNED    { $$ = new Node("TYPE_SPECIFIER", "signed"); }
    | UNSIGNED  { $$ = new Node("TYPE_SPECIFIER", "unsigned"); }
    | BOOL      { $$ = new Node("TYPE_SPECIFIER", "bool"); }
    | struct_or_union_specifier { $$ = $1; }
    | enum_specifier            { $$ = $1; }
    | TYPE_NAME                 {  $$ = new Node("TYPE_NAME", std::string($1)); 
        free($1);  }
    
    ;


storage_class_specifier
    : STATIC { $$ = new Node("STATIC"); }
    | TYPEDEF { $$ = new Node("TYPEDEF"); }
    ;
    
    
struct_or_union
    : STRUCT { $$ = new Node("STRUCT"); }
    | UNION { $$ = new Node("UNION"); }
    ;


struct_or_union_specifier
    : struct_or_union IDENTIFIER LBRACE struct_declaration_list RBRACE {
        $$ = new Node("STRUCT_OR_UNION_SPECIFIER");
        $$->addChild($1);  // struct/union
        Node* idNode = new Node("IDENTIFIER", std::string($2));
        $$->addChild(idNode);
        $$->addChild($4);  // member declarations
        
    }
    | struct_or_union LBRACE struct_declaration_list RBRACE {
        $$ = new Node("STRUCT_OR_UNION_SPECIFIER");
        $$->addChild($1);
        $$->addChild($3);
    }
    | struct_or_union IDENTIFIER {
        $$ = new Node("STRUCT_OR_UNION_SPECIFIER");
        $$->addChild($1);
        Node* idNode = new Node("IDENTIFIER", std::string($2));
        $$->addChild(idNode);
       
    }
    ;

struct_declaration_list
    : struct_declaration_list declaration {
        $$ = $1;
        $$->addChild($2);
    }
    | struct_declaration_list function_definition {
        $$ = $1;
        $$->addChild($2);
    }
    | declaration { $$ = $1; }
    ;


enum_specifier
    : ENUM LBRACE enumerator_list RBRACE {
        $$ = new Node("ENUM_SPECIFIER");
        $$->addChild($3);
    }
    | ENUM IDENTIFIER LBRACE enumerator_list RBRACE {
        $$ = new Node("ENUM_SPECIFIER");
        Node* idNode = new Node("IDENTIFIER", std::string($2));
        $$->addChild(idNode);
        $$->addChild($4);
       
    }
    | ENUM IDENTIFIER {
        $$ = new Node("ENUM_SPECIFIER");
        Node* idNode = new Node("IDENTIFIER", std::string($2));
        $$->addChild(idNode);
    
    }
    ;


enumerator_list
    : enumerator { 
        $$ = new Node("ENUMERATOR_LIST"); 
        $$->addChild($1); 
    }
    | enumerator_list COMMA enumerator {
        $$ = $1;
        $$->addChild($3);
    }
    ;

enumerator
    : IDENTIFIER {
        $$ = new Node("ENUMERATOR");
        Node* idNode = new Node("IDENTIFIER", std::string($1));
        $$->addChild(idNode);
    }
    | IDENTIFIER ASSIGN constant_expression {
        $$ = new Node("ENUMERATOR");
        Node* idNode = new Node("IDENTIFIER", std::string($1));
        $$->addChild(idNode);
        $$->addChild($3);
    }
    ;

type_qualifier
    : CONST { $$ = new Node("CONST"); }
    ;

type_qualifier_list
    : type_qualifier { $$ = $1; }
    | type_qualifier_list type_qualifier {
        $$ = $1;
        $$->addChild($2);
    }
    ;


pointer
    : ASTERISK_OPERATOR { $$ = new Node("POINTER"); }
    | ASTERISK_OPERATOR type_qualifier_list {
        $$ = new Node("POINTER");
        $$->addChild($2);
    }
    | ASTERISK_OPERATOR pointer {
        $$ = new Node("POINTER");
        $$->addChild($2);
    }
    | ASTERISK_OPERATOR type_qualifier_list pointer {
        $$ = new Node("POINTER");
        $$->addChild($2);
        $$->addChild($3);
    }
    ;

declarator
    : direct_declarator { $$ = $1; }
    | pointer direct_declarator {
        $$ = new Node("DECLARATOR");
        $$->addChild($1);
        $$->addChild($2);
    }
    ;

direct_declarator

    : IDENTIFIER {
    $$ = new Node("IDENTIFIER", std::string($1));
   
}

    | LPAREN declarator RPAREN { $$ = $2; }
    | direct_declarator LBRACKET RBRACKET {
        $$ = new Node("ARRAY");
        $$->addChild($1);
    }
    | direct_declarator LBRACKET constant_expression RBRACKET {
        $$ = new Node("ARRAY");
        $$->addChild($1);
        $$->addChild($3);
    }
    | direct_declarator LPAREN parameter_type_list RPAREN {
        $$ = new Node("FUNCTION_DECL");
        $$->addChild($1);
        $$->addChild($3);
    }
    | direct_declarator LPAREN RPAREN {
        $$ = new Node("FUNCTION_DECL");
        $$->addChild($1);
    }
    | direct_declarator LPAREN identifier_list RPAREN {
        $$ = new Node("FUNCTION_DECL");
        $$->addChild($1);
        $$->addChild($3);
    }
    ;


parameter_type_list
    : parameter_list { 
        $$ = new Node("PARAM_TYPE_LIST"); 
        $$->addChild($1); 
    }
    | parameter_list COMMA ELLIPSIS {
        $$ = new Node("PARAM_TYPE_LIST"); 
        $$->addChild($1); 
        $$->addChild(new Node("ELLIPSIS"));
    }

    ;

parameter_list
    : parameter_declaration { 
        $$ = new Node("PARAM_LIST"); 
        $$->addChild($1); 
    }
    | parameter_list COMMA parameter_declaration { 
        $$ = $1;
        $$->addChild($3);
    }
;
parameter_declaration
    : declaration_specifiers declarator {
        $$ = new Node("PARAM_DECL");
        $$->addChild($1);
        $$->addChild($2);
     }
    | declaration_specifiers AMP declarator {
        $$ = new Node("PARAM_DECL");
        $$->addChild($1);
        $$->addChild(new Node("&"));
        $$->addChild($3);
         }
    | declaration_specifiers abstract_declarator {
        $$ = new Node("PARAM_DECL");
        $$->addChild($1);
        $$->addChild($2);
    }
    | declaration_specifiers AMP abstract_declarator {
        $$ = new Node("PARAM_DECL");
        $$->addChild($1);
        $$->addChild(new Node("&"));
        $$->addChild($3);
    }
    | declaration_specifiers { 
        $$ = new Node("PARAM_DECL");
        $$->addChild($1);
    }
    ;


identifier_list
    : IDENTIFIER {
        $$ = new Node("IDENTIFIER", std::string($1));
        free($1);
    }
    | identifier_list COMMA IDENTIFIER {
        Node* idNode = new Node("IDENTIFIER", std::string($3));
        free($3);
        $$ = $1;
        $$->addChild(idNode);
    }
    ;

type_name
    : specifier_qualifier_list {
        $$ = $1;
    }
    | specifier_qualifier_list abstract_declarator {
        $$ = new Node("TYPE_NAME");
        $$->addChild($1);
        $$->addChild($2);
    }
    ;

abstract_declarator
    : pointer { $$ = $1; }
    | direct_abstract_declarator { $$ = $1; }
    | pointer direct_abstract_declarator {
        $$ = new Node("ABSTRACT_DECLARATOR");
        $$->addChild($1);
        $$->addChild($2);
    }
    ;

direct_abstract_declarator
    : LPAREN abstract_declarator RPAREN { $$ = $2; }
    | LBRACKET RBRACKET { $$ = new Node("ARRAY"); }
    | LBRACKET constant_expression RBRACKET { 
        $$ = new Node("ARRAY"); 
        $$->addChild($2); 
    }
    | direct_abstract_declarator LBRACKET RBRACKET { 
        $$ = new Node("ARRAY"); 
        $$->addChild($1); 
    }
    | direct_abstract_declarator LBRACKET constant_expression RBRACKET { 
        $$ = new Node("ARRAY"); 
        $$->addChild($1); 
        $$->addChild($3); 
    }
    | LPAREN RPAREN { $$ = new Node("FUNCTION"); }
    | LPAREN parameter_type_list RPAREN {
        $$ = new Node("FUNCTION");
        $$->addChild($2);
    }
    | direct_abstract_declarator LPAREN RPAREN {
        $$ = new Node("FUNCTION");
        $$->addChild($1);
    }
    | direct_abstract_declarator LPAREN parameter_type_list RPAREN {
        $$ = new Node("FUNCTION");
        $$->addChild($1);
        $$->addChild($3);
    }
    ;



initializer
    : assignment_expression { $$ = $1; }   // single assignment, no wrapper
    | LBRACE RBRACE { $$ = new Node("INIT_LIST_EMPTY"); }
    | LBRACE initializer_list RBRACE { 
    $$ = $2;  // Always return the INIT_LIST node (no flattening)
}
    | LBRACE initializer_list COMMA RBRACE { 
    $$ = $2;  // Same fix - no flattening
}
;


initializer_list
    : initializer { 
        $$ = new Node("INIT_LIST");
        $$->addChild($1);
    }
    | initializer_list COMMA initializer { $$->addChild($3); $$ = $1; }
	;
specifier_qualifier_list
    : type_qualifier { 
        $$ = new Node("SPEC_QUAL_LIST"); 
        $$->addChild($1); 
    }
    | type_specifier { 
        $$ = new Node("SPEC_QUAL_LIST"); 
        $$->addChild($1); 
    }
    | type_qualifier specifier_qualifier_list { 
        $$ = new Node("SPEC_QUAL_LIST");
        $$->addChild($1);
        $$->addChild($2); 
    }
    | type_specifier specifier_qualifier_list { 
        $$ = new Node("SPEC_QUAL_LIST");
        $$->addChild($1);
        $$->addChild($2);
    }
;
	


compound_statement
    : LBRACE RBRACE { $$ = new Node("COMPOUND_STMT"); }
    | LBRACE block_item_list RBRACE { 
        $$ = new Node("COMPOUND_STMT");
        $$->addChild($2);
    }
    ;

block_item_list
    : block_item { 
        $$ = new Node("BLOCK_ITEM_LIST"); 
        $$->addChild($1); 
    }
    | block_item_list block_item { 
        $1->addChild($2); 
        $$ = $1; 
    }
;


block_item
    : declaration { $$ = $1; }
    | statement { $$ = $1; }
    
    ;

statement
    : labeled_statement { $$ = $1; }
    | expression_statement { $$ = $1; }
    | compound_statement { $$ = $1; }
    | selection_statement { $$ = $1; }
    | iteration_statement { $$ = $1; }
    | jump_statement { $$ = $1; }
    ;


labeled_statement
    : IDENTIFIER COLON statement {
        Node* labelNode = new Node("IDENTIFIER", std::string($1));
        free($1);

        $$ = new Node("LABELED_STMT");
        $$->addChild(labelNode);
        $$->addChild($3);
    }
    ;

    

expression_statement
    : SEMICOLON { $$ = new Node("EMPTY_STMT"); }
    | expression SEMICOLON { $$ = $1; }
    | printf_stmt SEMICOLON { $$ = $1; }
    | scanf_stmt SEMICOLON { $$ = $1; }
    | error SEMICOLON { yyerrok; $$ = new Node("EXPR_ERROR"); }
    ;

selection_statement
    : IF LPAREN expression RPAREN statement %prec IF {
        $$ = new Node("IF_STMT");
        $$->addChild($3);
        $$->addChild($5);
    }
    | IF LPAREN expression RPAREN statement ELSE statement {
        $$ = new Node("IF_ELSE_STMT");
        $$->addChild($3);
        $$->addChild($5);
        $$->addChild($7);
    }
    | SWITCH LPAREN expression RPAREN switch_body {
        $$ = new Node("SWITCH_STMT");
        $$->addChild($3);
        $$->addChild($5);
    }
    ;

switch_body
    : LBRACE case_list_opt RBRACE { $$ = $2; }
    ;


case_list_opt
    : /* empty */ { 
        $$ = new Node("CASE_LIST");  // always a CASE_LIST node
    }
    | case_list { 
        $$ = $1; 
    }
    ;

case_list
    : case_element { 
        $$ = new Node("CASE_LIST"); 
        $$->addChild($1); 
    }
    | case_list case_element { 
        $$ = $1;
        $$->addChild($2);
    }
    ;

case_element
    : CASE constant_expression COLON statement_list {
        $$ = new Node("CASE_ELEMENT");
        $$->addChild($2);
        $$->addChild($4);
    }
    | DEFAULT COLON statement_list {
        $$ = new Node("DEFAULT_ELEMENT");
        $$->addChild($3);
    }
    ;


statement_list
    : statement { 
        $$ = new Node("STATEMENT_LIST"); 
        $$->addChild($1); 
    }
    | statement_list statement { 
        $$ = $1;
        $$->addChild($2);
    }
	;

iteration_statement
    : WHILE LPAREN expression RPAREN statement {
        $$ = new Node("WHILE_STMT");
        $$->addChild($3); // condition
        $$->addChild($5); // body
    }
    | DO statement WHILE LPAREN expression RPAREN SEMICOLON {
        $$ = new Node("DO_WHILE_STMT");
        $$->addChild($2); // body
        $$->addChild($5); // condition
    }
    | FOR LPAREN for_init_statement expression_statement RPAREN statement {
        $$ = new Node("FOR_STMT_2");
        $$->addChild($3); // init
        $$->addChild($4); // condition
        $$->addChild($6); // body
    }
    | FOR LPAREN for_init_statement expression_statement expression RPAREN statement {
        $$ = new Node("FOR_STMT_3");
        $$->addChild($3); // init
        $$->addChild($4); // condition
        $$->addChild($5); // update
        $$->addChild($7); // body
    }
    | FOR LPAREN range_based_for RPAREN statement {
        $$ = new Node("FOR_RANGE_STMT");
        $$->addChild($3); // range
        $$->addChild($5); // body
    }
    | UNTIL LPAREN expression RPAREN compound_statement {
        $$ = new Node("UNTIL_STMT");
        $$->addChild($3); // condition
        $$->addChild($5); // body
    }
    ;

for_init_statement
    : expression_statement { $$ = $1; }
    | declaration { $$ = $1; }
    ;

range_based_for
    : declaration_specifiers IDENTIFIER COLON expression {
        $$ = new Node("RANGE_FOR");
        $$->addChild($1); // type
       
          Node* labelNode = new Node("IDENTIFIER", std::string($2));
        free($2);
         $$->addChild(labelNode); // variable
        $$->addChild($4); // iterable
    }
    | declaration_specifiers AMP IDENTIFIER COLON expression {
        $$ = new Node("RANGE_FOR_REF");
        $$->addChild($1); // type
         Node* labelNode = new Node("IDENTIFIER", std::string($3));
        free($3);
         $$->addChild(labelNode); // variable
        $$->addChild($5); // iterable
    }
    ;
jump_statement
    : GOTO IDENTIFIER SEMICOLON {
        $$ = new Node("GOTO_STMT");
       Node* labelNode = new Node("IDENTIFIER", std::string($2));
        free($2);
         $$->addChild(labelNode); // variable
    }
    | CONTINUE SEMICOLON {
        $$ = new Node("CONTINUE_STMT");
    }
    | BREAK SEMICOLON {
        $$ = new Node("BREAK_STMT");
    }
    | RETURN SEMICOLON {
        $$ = new Node("RETURN_STMT");
    }
    | RETURN expression SEMICOLON {
        $$ = new Node("RETURN_STMT");
        $$->addChild($2);
    }
    ;

expression
    : assignment_expression { 
        $$ = new Node("EXPR_LIST"); 
        $$->addChild($1); 
    }
    | expression COMMA assignment_expression {
        if ($1->type == "EXPR_LIST") {
            $1->addChild($3);
            $$ = $1;
        } else {
            $$ = new Node("EXPR_LIST");
            $$->addChild($1);
            $$->addChild($3);
        }
    }
    ;


assignment_operator
    : ASSIGN       { $$ = new Node("OP", std::string("=")); }
    | MUL_ASSIGN   { $$ = new Node("OP", std::string("*=")); }
    | DIV_ASSIGN   { $$ = new Node("OP", std::string("/=")); }
    | MOD_ASSIGN   { $$ = new Node("OP", std::string("%=")); }
    | ADD_ASSIGN   { $$ = new Node("OP", std::string("+=")); }
    | SUB_ASSIGN   { $$ = new Node("OP", std::string("-=")); }
    | LEFT_ASSIGN  { $$ = new Node("OP", std::string("<<=")); }
    | RIGHT_ASSIGN { $$ = new Node("OP", std::string(">>=")); }
    | AND_ASSIGN   { $$ = new Node("OP", std::string("&=")); }
    | XOR_ASSIGN   { $$ = new Node("OP", std::string("^=")); }
    | OR_ASSIGN    { $$ = new Node("OP", std::string("|=")); }
    ;
    
assignment_expression
    : conditional_expression { $$ = $1; }
    | unary_expression assignment_operator assignment_expression {
        $$ = new Node("ASSIGN_EXPR");
        $$->addChild($1);  // left-hand side
        $$->addChild($2);  // operator node
        $$->addChild($3);  // right-hand side
    }
    ;

conditional_expression
    : logical_or_expression { $$ = $1; }
    | logical_or_expression QUESTIONMARK expression COLON conditional_expression {
        $$ = new Node("COND_EXPR");
        $$->addChild($1); // condition
        $$->addChild($3); // true branch
        $$->addChild($5); // false branch
    }
    ;

logical_or_expression
    : logical_and_expression { $$ = $1; }
    | logical_or_expression OR_OP logical_and_expression {
    $$ = new Node("LOGICAL_OR");
    $$->addChild($1);
    $$->addChild($3);
}
    ;

logical_and_expression
    : inclusive_or_expression { $$ = $1; }
    | logical_and_expression AND_OP inclusive_or_expression {
    $$ = new Node("LOGICAL_AND");
    $$->addChild($1);
    $$->addChild($3);
}
    ;

inclusive_or_expression
    : exclusive_or_expression { $$ = $1; }
    | inclusive_or_expression PIPE exclusive_or_expression {
    $$ = new Node("BIT_OR");
    $$->addChild($1);
    $$->addChild($3);
}
    ;

exclusive_or_expression
    : and_expression { $$ = $1; }
   | exclusive_or_expression CARET and_expression {
    $$ = new Node("BIT_XOR");
    $$->addChild($1);
    $$->addChild($3);
}
    ;

and_expression
    : equality_expression { $$ = $1; }
   | and_expression AMP equality_expression {
    $$ = new Node("BIT_AND");
    $$->addChild($1);
    $$->addChild($3);
}
    ;
equality_expression
    : relational_expression { $$ = $1; }
    | equality_expression EQ_OP relational_expression {
    if ($1->type == "EQ_EXPR") {
        $1->addChild($3);
        $$ = $1;
    } else {
        $$ = new Node("EQ_EXPR");
        $$->addChild($1);
        $$->addChild($3);
    }
}
| equality_expression NE_OP relational_expression {
    if ($1->type == "NEQ_EXPR") {
        $1->addChild($3);
        $$ = $1;
    } else {
        $$ = new Node("NEQ_EXPR");
        $$->addChild($1);
        $$->addChild($3);
    }
}
    ;

relational_expression
    : shift_expression { $$ = $1; }
  | relational_expression LT shift_expression {
    $$ = new Node("LT_EXPR");
    $$->addChild($1);
    $$->addChild($3);
}
| relational_expression GT shift_expression {
    $$ = new Node("GT_EXPR");
    $$->addChild($1);
    $$->addChild($3);
}
| relational_expression LE_OP shift_expression {
    $$ = new Node("LE_EXPR");
    $$->addChild($1);
    $$->addChild($3);
}
| relational_expression GE_OP shift_expression {
    $$ = new Node("GE_EXPR");
    $$->addChild($1);
    $$->addChild($3);
}
    ;

shift_expression
    : additive_expression { $$ = $1; }
   | shift_expression LEFT_OP additive_expression {
    $$ = new Node("LSHIFT_EXPR");
    $$->addChild($1);
    $$->addChild($3);
}
| shift_expression RIGHT_OP additive_expression {
    $$ = new Node("RSHIFT_EXPR");
    $$->addChild($1);
    $$->addChild($3);
}
    ;

additive_expression
    : multiplicative_expression { $$ = $1; }
    | additive_expression PLUS multiplicative_expression {
        $$ = new Node("ADD_EXPR");
        $$->addChild($1);
        $$->addChild($3);
    }
    | additive_expression MINUS multiplicative_expression {
        $$ = new Node("SUB_EXPR");
        $$->addChild($1);
        $$->addChild($3);
    }
    ;

multiplicative_expression
    : cast_expression { $$ = $1; }
    | multiplicative_expression ASTERISK_OPERATOR cast_expression {
        $$ = new Node("MUL_EXPR");
        $$->addChild($1);
        $$->addChild($3);
    }
    | multiplicative_expression DIVIDE cast_expression {
        $$ = new Node("DIV_EXPR");
        $$->addChild($1);
        $$->addChild($3);
    }
    | multiplicative_expression PERCENT cast_expression {
        $$ = new Node("MOD_EXPR");
        $$->addChild($1);
        $$->addChild($3);
    }
    ;

cast_expression
    : unary_expression { $$ = $1; }
    | LPAREN type_name RPAREN cast_expression {
        $$ = new Node("CAST_EXPR");
        $$->addChild($2); // type
        $$->addChild($4); // expression
    }
    ;

unary_expression
    : postfix_expression { $$ = $1; }
    | INC_OP unary_expression {
        $$ = new Node("PRE_INC");
        $$->addChild($2);
    }
    | DEC_OP unary_expression {
        $$ = new Node("PRE_DEC");
        $$->addChild($2);
    }
    | unary_operator cast_expression {
        $$ = new Node("UNARY_OP");
        $$->addChild($1);
        $$->addChild($2);
    }
    | SIZEOF unary_expression {
        $$ = new Node("SIZEOF");
        $$->addChild($2);
    }
    | SIZEOF LPAREN type_name RPAREN {
        $$ = new Node("SIZEOF_TYPE");
        $$->addChild($3);
    }
    ;

   
unary_operator
    : AMP { $$ = new Node(std::string("&")); }
    | ASTERISK_OPERATOR { $$ = new Node(std::string("*")); }
    | PLUS { $$ = new Node(std::string("+")); }
    | MINUS { $$ = new Node(std::string("-")); }
    | TILDE { $$ = new Node(std::string("~")); }
    | BANG { $$ = new Node(std::string("!")); }
    ;

postfix_expression
    : primary_expression { $$ = $1; }
    | postfix_expression LBRACKET expression RBRACKET {
        $$ = new Node("ARRAY_ACCESS");
        $$->addChild($1); // array
        $$->addChild($3); // index
    }
    | postfix_expression LPAREN RPAREN {
        $$ = new Node("FUNC_CALL");
        $$->addChild($1); // function
    }
    | postfix_expression LPAREN argument_expression_list RPAREN {
        $$ = new Node("FUNC_CALL");
        $$->addChild($1); // function
        $$->addChild($3); // arguments
    }
    | postfix_expression DOT IDENTIFIER {
        $$ = new Node("MEMBER_ACCESS");
        $$->addChild($1); // object
        Node* idNode = new Node("IDENTIFIER", std::string($3));
      
        $$->addChild(idNode); // member
    }
    | postfix_expression ARROW_OPERATOR IDENTIFIER {
        $$ = new Node("PTR_MEMBER_ACCESS");
        $$->addChild($1); // pointer
        Node* idNode = new Node("IDENTIFIER", std::string($3));
    
        $$->addChild(idNode); // member
    }
    | postfix_expression INC_OP {
        $$ = new Node("POST_INC");
        $$->addChild($1);
    }
    | postfix_expression DEC_OP {
        $$ = new Node("POST_DEC");
        $$->addChild($1);
    }
    ;

argument_expression_list
    : assignment_expression { $$ = new Node("ARG_LIST"); $$->addChild($1); }
    | argument_expression_list COMMA assignment_expression {
        $$ = $1;
        $$->addChild($3);
    }
    ;
primary_expression
    : IDENTIFIER { $$ = new Node("IDENTIFIER", std::string($1));  }
    | constant { $$ = $1; }
| STRING_LITERAL { $$ = new Node("STRING_LITERAL", std::string($1));  }
    | LPAREN expression RPAREN { $$ = $2; }
    ;

constant
    : INTEGER_CONSTANT { $$ = new Node("INTEGER_CONSTANT", std::string($1)); free($1); }
| FLOAT_CONSTANT { $$ = new Node("FLOAT_CONSTANT", std::string($1)); free($1); }
| CHAR_LITERAL { $$ = new Node("CHAR_LITERAL", std::string($1)); free($1); }
| BOOL_LITERAL { $$ = new Node("BOOL_LITERAL", std::string($1)); free($1); }
| HEX_INT_LITERAL { $$ = new Node("HEX_INT_LITERAL", std::string($1)); free($1); }
| OCTAL_INT_LITERAL { $$ = new Node("OCTAL_INT_LITERAL", std::string($1)); free($1); }
| BINARY_INT_LITERAL { $$ = new Node("BINARY_INT_LITERAL", std::string($1)); free($1); }
| HEX_FLOAT_LITERAL { $$ = new Node("HEX_FLOAT_LITERAL", std::string($1)); free($1); }

constant_expression
    : conditional_expression { $$ = $1; }
    ;


printf_stmt
    : PRINTF LPAREN STRING_LITERAL RPAREN {
        $$ = new Node("PRINTF");
        Node* strNode = new Node("STRING_LITERAL", std::string($3));
      
        $$->addChild(strNode); // format string
    }
    | PRINTF LPAREN STRING_LITERAL COMMA argument_expression_list RPAREN {
        $$ = new Node("PRINTF");
        Node* strNode = new Node("STRING_LITERAL", std::string($3));
       
        $$->addChild(strNode); // format string
        $$->addChild($5);      // arguments
    }
    ;

scanf_stmt
    : SCANF LPAREN STRING_LITERAL RPAREN {
        $$ = new Node("SCANF");
        Node* strNode = new Node("STRING_LITERAL", std::string($3));
        
        $$->addChild(strNode); // format string
    }
    | SCANF LPAREN STRING_LITERAL COMMA variable_list RPAREN {
        $$ = new Node("SCANF");
        Node* strNode = new Node("STRING_LITERAL", std::string($3));
        
        $$->addChild(strNode); // format string
        $$->addChild($5);      // variables
    }
    ;


variable_list
    : AMP IDENTIFIER {
        $$ = new Node("IDENTIFIER", std::string($2));
      
    }
    | variable_list COMMA AMP IDENTIFIER { 
    $$ = $1;  // Use existing list
    Node* idNode = new Node("IDENTIFIER", std::string($4));
    free($4);
    $$->addChild(idNode);
}
;


%%


void yyerror(const char* msg) {
    printf("<<< SYNTAX ERROR at line %d, column %d: %s >>>\n", line_num, col_num, msg);
    errors.addError(line_num, ("SYNTAX ERROR: " + std::string(msg)).c_str());
    yyclearin;  
}



int main(int argc, char** argv) {
    if (argc > 1) {
        FILE* file = fopen(argv[1], "r");
        if (!file) {
            perror("Error opening file");
            return 1;
        }
        yyin = file;
    }
    
    SymbolTable symTab;
    printf("Starting syntax analysis...\n");
    printf("%-40s | %s\n", "TOKEN", "LEXEME");
    printf("------------------------------------------|----------\n");
    
    int result = yyparse();
    
    if (root) {
        cout << "\nPrinting AST:\n";
        root->printTree();

        ofstream out("ast.dot");
        int nodeId = 0;
        out << "digraph AST {" << endl;
        root->generateDOT(out, nodeId);
        out << "}" << endl;
        out.close();

        cout << "DOT file generated: ast.dot" << endl;
        system("dot -Tpng ast.dot -o ast.png");
        cout << "AST image generated: ast.png" << endl;
        
        
        cout << "\n=== STARTING SEMANTIC ANALYSIS ===" << endl;
        
        // Semantic Analysis
        SemanticAnalyzer semanticAnalyzer(symTab);
        bool semanticSuccess = semanticAnalyzer.buildSymbolTable(root);
        
        cout << "=== SEMANTIC ANALYSIS COMPLETE ===\n" << endl;
        
        // ✅ NEW: IR Generation only if semantics are valid
        if (semanticSuccess && !semanticAnalyzer.hasErrors()) {
            cout << "\n=== GENERATING TAC IR ===" << endl;
            
            IRGenerator irGenerator(symTab);
            bool irSuccess = irGenerator.generateIR(root);
            
            if (irSuccess) {
                // Print to console
                irGenerator.printIR();
                
                // Write to file
                irGenerator.writeToFile("output.3ac");
                
                cout << "✅ TAC IR generation successful" << endl;
                cout << "📄 TAC output written to: output.3ac" << endl;
            } else {
                cout << "❌ TAC IR generation failed" << endl;
                result = 1;
            }
        } else {
            cout << "❌ Semantic errors detected - skipping IR generation" << endl;
            semanticAnalyzer.printErrors();
            result = 1;
        }
    }

    printf("------------------------------------------|----------\n");
    printf("\n\n");
    if (result == 0) {
        printf("✅ Compilation completed successfully\n");
    } else {
        printf("❌ Compilation failed\n");
    }
    
    
    if (argc > 1) {
        fclose(yyin);
    }
    return result;
}
