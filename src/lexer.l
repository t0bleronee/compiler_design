%{
#include <cstdio>
#include <string>
#include "ast.h"   
#include "semantic_analyzer.h"
#include "parser.tab.h"
#include "errors.h"
#include <unordered_set>
#include "symbol_table.h"
using namespace std;


std::unordered_set<std::string> typedef_names;



int lexical_error = 0;
int line_num = 1;    
int col_num = 1;  


bool inside_class = false;
int inside_for_loop = 0;
int after_equal = 0;


int open_braces = 0;

void printToken(const char* token, const char* lexeme) {
    printf("%-40s | %s\n", token, lexeme);
}
#define YY_USER_ACTION \
    { \
        int i; \
        for (i = 0; yytext[i] != '\0'; i++) { \
            if (yytext[i] == '\n') { \
                line_num++; \
                col_num = 1; \
            } else { \
                col_num++; \
            } \
        } \
    }

%}
%x COMMENT
%option nounput

%option noyywrap


WHITESPACE  [ \t\r\n]+

SINGLE_LINE_COMMENT   "//"[^\n]*

/* Common whitespace before and after # */
PP_PREFIX        ^[ \t]*#[ \t]*

/* Identifier for macro names */
PP_IDENTIFIER    [a-zA-Z_][a-zA-Z0-9_]*

/* Preprocessor categories */
PP_DEFINE        {PP_PREFIX}define[ \t]+{PP_IDENTIFIER}([ \t]+.*)?$
PP_UNDEF         {PP_PREFIX}undef[ \t]+{PP_IDENTIFIER}[ \t]*$
PP_IF            {PP_PREFIX}if[ \t]+.*$
PP_ELIF          {PP_PREFIX}elif[ \t]+.*$
PP_ELSE          {PP_PREFIX}else[ \t]*$
PP_ENDIF         {PP_PREFIX}endif[ \t]*$
PP_IFDEF         {PP_PREFIX}ifdef[ \t]+{PP_IDENTIFIER}[ \t]*$
PP_IFNDEF        {PP_PREFIX}ifndef[ \t]+{PP_IDENTIFIER}[ \t]*$
PP_ERROR         {PP_PREFIX}error[ \t]+.*$
PP_WARNING       {PP_PREFIX}warning[ \t]+.*$
PP_PRAGMA        {PP_PREFIX}pragma[ \t]+.*$
PP_INCLUDE       {PP_PREFIX}include[ \t]*([<"][^>"]+[>"])

STRING_LITERAL      \"([^\\\"\n]|(\\.))*\"
ESCAPE_HEX    \\x[0-9a-fA-F]+
ESCAPE_OCT    \\[0-7]{1,3}
ESCAPE_CHAR   \\[abfnrtv\'\"\\?] 
ESCAPE_UNICODE  \\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}
CHAR_PREFIX    (u|U|L)?
CHAR_CONTENT  ({ESCAPE_HEX}|{ESCAPE_OCT}|{ESCAPE_CHAR}|{ESCAPE_UNICODE}|[^\\\'\n])
CHAR_LITERAL   {CHAR_PREFIX}\'({CHAR_CONTENT})\'


KEYWORD    (int|float|double|char|bool|void|short|long|signed|unsigned|if|else|switch|case|default|for|while|do|break|continue|return|goto|auto|static|class|struct|union|enum|template|typename|this|namespace|friend|virtual|override|explicit|inline|try|catch|throw|const|sizeof|typedef|typeid|new|delete|nullptr|constexpr|array|vector|using|cout|cin)

ALTERNATIVE_OPERATOR  (and|or|not|xor|bitand|bitor|compl|and_eq|or_eq|xor_eq|not_eq)

BOOL_LITERAL (true|false|TRUE|FALSE)


IDENTIFIER  [a-zA-Z_][a-zA-Z0-9_]*




HEXADECIMAL_FLOAT_LITERAL  0[xX]([0-9a-fA-F]+\.[0-9a-fA-F]*|\.[0-9a-fA-F]+|[0-9a-fA-F]+)[pP][+-]?[0-9]+([fFlL])?


HEXADECIMAL_INT_LITERAL 0[xX][0-9a-fA-F]+((u|U)(ll|LL|l|L)?|(ll|LL|l|L)(u|U)?)?

OCTAL_INT_LITERAL 0[0-7]+((u|U)(ll|LL|l|L)?|(ll|LL|l|L)(u|U)?)?  

BINARY_INT_LITERAL 0[bB][01]+((u|U)(ll|LL|l|L)?|(ll|LL|l|L)(u|U)?)? 


/* Numbers: integers, floats (with optional decimal and exponent), and hex */
FLOAT_LITERAL          (((?:0|[1-9][0-9]*)\.[0-9]*([eE][+-]?[0-9]+)?)|((?:0|[1-9][0-9]*)?\.[0-9]+([eE][+-]?[0-9]+)?)|((0|[1-9][0-9]*)([eE][+-]?[0-9]+)))([lLfF])?
INTEGER_LITERAL        (0|([1-9][0-9]*))((u|U)(ll|LL|l|L)?|(ll|LL|l|L)(u|U)?)?

/* Operators: multi-character and single-character operators with proper alternation */
OPERATOR    (\+\+|--|\+=|-=|\/=|%=|<<=|>>=|==|!=|>=|<=|&&|\|\||<<|>>|&=|\|=|\^=|\+|-|\*|\/|%|>|<|=|&|\||\^|~|!|\?|:)

/* Punctuation: includes ..., ::, and . */
PUNCTUATION (\.\.\.|::|\.)

/* Delimiters: single characters like parentheses, brackets, braces, commas, semicolons */
DELIMITER   [()\[\]{},;]




%%
"/*"                { BEGIN(COMMENT); }

<COMMENT>{
    "*/"            { BEGIN(INITIAL); }   /* End of comment */
    .|\n            { /* consume inside comment */ }
    <<EOF>>         { 
                       errors.addError(line_num, "LEXICAL ERROR: Unterminated comment");
                       BEGIN(INITIAL); 
                    }
}
{WHITESPACE}    {  }
{SINGLE_LINE_COMMENT} {  }


{PP_DEFINE}      { printf("%s\tpreprocessor_define\n", yytext); }
{PP_UNDEF}       { printf("%s\tpreprocessor_undef\n", yytext); }
{PP_IF}          { printf("%s\tpreprocessor_if\n", yytext); }
{PP_ELIF}        { printf("%s\tpreprocessor_elif\n", yytext); }
{PP_ELSE}        { printf("%s\tpreprocessor_else\n", yytext); }
{PP_ENDIF}       { printf("%s\tpreprocessor_endif\n", yytext); }
{PP_IFDEF}       { printf("%s\tpreprocessor_ifdef\n", yytext); }
{PP_IFNDEF}      { printf("%s\tpreprocessor_ifndef\n", yytext); }
{PP_ERROR}       { printf("%s\tpreprocessor_error\n", yytext); }
{PP_WARNING}     { printf("%s\tpreprocessor_warning\n", yytext); }
{PP_PRAGMA}      { printf("%s\tpreprocessor_pragma\n", yytext); }
{PP_INCLUDE}     { printf("%s\tpreprocessor_include\n", yytext); }


{STRING_LITERAL}            { printToken("STRING LITERAL", yytext); 
                                           yylval.strval = strdup(yytext); 
                                            return STRING_LITERAL;
                                          }
{CHAR_LITERAL}      	{ printToken("CHAR LITERAL", yytext);
                                         yylval.strval = strdup(yytext); 
                                            return CHAR_LITERAL;
                                          }



"and"			{ printToken("OPERATOR", yytext); return AND; }
"or"			{ printToken("OPERATOR", yytext); return OR; }
"not"			{ printToken("OPERATOR", yytext); return NOT; }
"xor"			{ printToken("OPERATOR", yytext); return XOR; }
"bitand"		{ printToken("OPERATOR", yytext); return BITAND; }
"bitor"			{ printToken("OPERATOR", yytext); return BITOR; }
"compl"			{ printToken("OPERATOR", yytext); return COMPL; }
"and_eq"		{ printToken("OPERATOR", yytext); return AND_EQ; }
"or_eq"			{ printToken("OPERATOR", yytext); return OR_EQ; }
"xor_eq"		{ printToken("OPERATOR", yytext); return XOR_EQ; }
"not_eq"		{ printToken("OPERATOR", yytext); return NOT_EQ; }





"auto"			{ printToken("KEYWORD", yytext); return AUTO; }
"array"			{ printToken("KEYWORD", yytext); return ARRAY; }
"bool"			{ printToken("KEYWORD", yytext); return BOOL; }
"break"			{ printToken("KEYWORD", yytext); return BREAK; }
"case"			{ printToken("KEYWORD", yytext); return CASE; }
"catch"			{ printToken("KEYWORD", yytext); return CATCH; }
"char"			{ printToken("KEYWORD", yytext); return CHAR; }
"class"			{inside_class = true; printToken("KEYWORD", yytext); return CLASS; }

"const"			{ printToken("KEYWORD", yytext); return CONST; }
"constexpr"		{ printToken("KEYWORD", yytext); return CONSTEXPR; }
"continue"		{ printToken("KEYWORD", yytext); return CONTINUE; }
"cout"      		{ printToken("KEYWORD", yytext); return COUT; } 
"cin"       		{ printToken("KEYWORD", yytext); return CIN; } 
"default"		{ printToken("KEYWORD", yytext); return DEFAULT; }
"delete"		{ printToken("KEYWORD", yytext); return DELETE; }
"do"			{ printToken("KEYWORD", yytext); return DO; }
"double"		{ printToken("KEYWORD", yytext); return DOUBLE; }
"else"			{ printToken("KEYWORD", yytext); return ELSE; }
"enum"			{ printToken("KEYWORD", yytext); return ENUM; }
"endl"			{printToken("KEYWORD", yytext);return ENDL;}
"explicit"		{ printToken("KEYWORD", yytext); return EXPLICIT; }
"float"			{ printToken("KEYWORD", yytext); return FLOAT; }
"for"			{inside_for_loop=1; printToken("KEYWORD", yytext); return FOR; }  
"friend"		{ printToken("KEYWORD", yytext); return FRIEND; }
"goto"			{ printToken("KEYWORD", yytext); return GOTO; }
"if"			{ printToken("KEYWORD", yytext); return IF; }
"inline"		{ printToken("KEYWORD", yytext); return INLINE; }
"int"			{ printToken("KEYWORD", yytext); return INT; }
"long"			{  printToken("KEYWORD", yytext); return LONG; }
"namespace"		{ printToken("KEYWORD", yytext); return NAMESPACE; }
"new"			{ printToken("KEYWORD", yytext); return NEW; }
"nullptr"		{ printToken("KEYWORD", yytext); return NULLPTR; }
"override"		{ printToken("KEYWORD", yytext); return OVERRIDE; }
"private"		{ printToken("KEYWORD", yytext); return PRIVATE; }
"printf" 		{ printToken("KEYWORD", yytext); return PRINTF; }
"public"		{ printToken("KEYWORD", yytext); return PUBLIC; }
"protected"		{ printToken("KEYWORD", yytext); return PROTECTED; }
"return"		{ printToken("KEYWORD", yytext); return RETURN; }
"scanf"			{ printToken("KEYWORD", yytext); return SCANF; }
"short"			{ printToken("KEYWORD", yytext); return SHORT; }
"signed"		{ printToken("KEYWORD", yytext); return SIGNED; }
"sizeof"		{after_equal=0; printToken("KEYWORD", yytext); return SIZEOF; }
"static"		{ printToken("KEYWORD", yytext); return STATIC; }
"struct"		{ printToken("KEYWORD", yytext); return STRUCT; }
"switch"		{ printToken("KEYWORD", yytext); return SWITCH; }
"template"		{ printToken("KEYWORD", yytext); return TEMPLATE; }
"this"			{ printToken("KEYWORD", yytext); return THIS; }
"throw"			{ printToken("KEYWORD", yytext); return THROW; }
"try"			{  printToken("KEYWORD", yytext); return TRY; }
"typedef"		{ printToken("KEYWORD", yytext); return TYPEDEF; }
"typeid"		{ printToken("KEYWORD", yytext); return TYPEID; }
"typename"		{ printToken("KEYWORD", yytext); return TYPENAME; }
"union"			{ printToken("KEYWORD", yytext); return UNION; }
"unsigned"		{ printToken("KEYWORD", yytext); return UNSIGNED; }
"using"                 {printToken("KEYWORD", yytext); return USING; }
"vector"		{ printToken("KEYWORD", yytext); return VECTOR; }
"virtual"		{ printToken("KEYWORD", yytext); return VIRTUAL; }
"void"			{ printToken("KEYWORD", yytext); return VOID; }
"while"			{ printToken("KEYWORD", yytext); return WHILE; }

"until"     { printToken("KEYWORD", yytext); return UNTIL; }
"done"      { printToken("KEYWORD", yytext); return DONE; }







"true"          { printToken("BOOL_LITERAL", yytext); yylval.strval = strdup(yytext); return BOOL_LITERAL; }
"false"         { printToken("BOOL_LITERAL", yytext); yylval.strval = strdup(yytext); return BOOL_LITERAL; }
"TRUE"          { printToken("BOOL_LITERAL", yytext); yylval.strval = strdup(yytext); return BOOL_LITERAL; }
"FALSE"         { printToken("BOOL_LITERAL", yytext); yylval.strval = strdup(yytext); return BOOL_LITERAL; }




{HEXADECIMAL_FLOAT_LITERAL} { printToken("HEX_FLOAT_LITERAL", yytext); 
				  yylval.strval = strdup(yytext);
				return HEX_FLOAT_LITERAL; }
{HEXADECIMAL_INT_LITERAL}   { printToken("HEX_INT_LITERAL", yytext); 
				  yylval.strval = strdup(yytext);
				return HEX_INT_LITERAL; }
{OCTAL_INT_LITERAL}         { printToken("OCTAL_INT_LITERAL", yytext); 
				  yylval.strval = strdup(yytext);
				return OCTAL_INT_LITERAL; }
{BINARY_INT_LITERAL}        { printToken("BINARY_INT_LITERAL", yytext); 
 				  yylval.strval = strdup(yytext);
				return BINARY_INT_LITERAL; }
{FLOAT_LITERAL}             { printToken("FLOAT CONSTANT", yytext); 
                                             yylval.strval = strdup(yytext);
                                            return FLOAT_CONSTANT; 
                                          }
{INTEGER_LITERAL}           {  printToken("CONSTANT", yytext); 
                                            yylval.strval = strdup(yytext);
                                            return INTEGER_CONSTANT; 
                                          }


{IDENTIFIER}    { 
    yylval.strval = strdup(yytext);
    
    // Check if this is a typedef name
    if (typedef_names.find(std::string(yytext)) != typedef_names.end()) {
        printToken("TYPE_NAME", yytext);
        return TYPE_NAME;
    }
 
    printToken("IDENTIFIER", yytext); 
    return IDENTIFIER; 
}



"->"		    { printToken("OPERATOR", yytext); return ARROW_OPERATOR; }
"::"		    { printToken("OPERATOR", yytext); return SCOPE_OPERATOR; }
"++"		    { printToken("OPERATOR", yytext); return INC_OP; }
"--"		    { printToken("OPERATOR", yytext); return DEC_OP; }
"+="		    { printToken("OPERATOR", yytext); return ADD_ASSIGN; }
"-="		    { printToken("OPERATOR", yytext); return SUB_ASSIGN; }
"*="		    { printToken("OPERATOR", yytext); return MUL_ASSIGN; }
"/="		    { printToken("OPERATOR", yytext); return DIV_ASSIGN; }
"%="		    { printToken("OPERATOR", yytext); return MOD_ASSIGN; }
"<<="		    { printToken("OPERATOR", yytext); return LEFT_ASSIGN; }
">>="		    { printToken("OPERATOR", yytext); return RIGHT_ASSIGN; }
"=="		    { printToken("OPERATOR", yytext); return EQ_OP; }
"!="		    { printToken("OPERATOR", yytext); return NE_OP; }
">="		    { printToken("OPERATOR", yytext); return GE_OP; }
"<="		    { printToken("OPERATOR", yytext); return LE_OP; }
"&&"		    { printToken("OPERATOR", yytext); return AND_OP; }
"||"		    { printToken("OPERATOR", yytext); return OR_OP; }
"<<"		    { printToken("OPERATOR", yytext); return LEFT_OP; }
">>"		    { printToken("OPERATOR", yytext); return RIGHT_OP; }
"&="		    { printToken("OPERATOR", yytext); return AND_ASSIGN; }
"|="		    { printToken("OPERATOR", yytext); return OR_ASSIGN; }
"^="		    { printToken("OPERATOR", yytext); return XOR_ASSIGN; }
"+"		        { printToken("OPERATOR", yytext); return PLUS; }
"-"		        { printToken("OPERATOR", yytext); return MINUS; }
"*"		        { printToken("OPERATOR", yytext); return ASTERISK_OPERATOR; }
"/"		        { printToken("OPERATOR", yytext); return DIVIDE; }
"%"		        { printToken("OPERATOR", yytext); return PERCENT; }
">"		        { printToken("OPERATOR", yytext); return GT; }
"<"		        { printToken("OPERATOR", yytext); return LT; }
"="		        {after_equal=1; printToken("OPERATOR", yytext); return ASSIGN; }
"&"		        { printToken("OPERATOR", yytext); return AMP; }
"|"		        { printToken("OPERATOR", yytext); return PIPE; }
"^"		        { printToken("OPERATOR", yytext); return CARET; }
"~"		        { printToken("OPERATOR", yytext); return TILDE; }
"!"		        { printToken("OPERATOR", yytext); return BANG; }



";"			    {after_equal=0; printToken("PUNCTUATOR", yytext); return SEMICOLON; }
"?"			    { printToken("PUNCTUATOR", yytext); return QUESTIONMARK; }
":"			    { printToken("PUNCTUATOR", yytext); return COLON; }
","			    { printToken("PUNCTUATOR", yytext); return COMMA; }
"."			    { printToken("PUNCTUATOR", yytext); return DOT; }
"..."			{ printToken("PUNCTUATOR", yytext); return ELLIPSIS; }
"("                 { printToken("PUNCTUATOR", yytext); return LPAREN; }
")"                 {inside_for_loop=0; printToken("PUNCTUATOR", yytext); return RPAREN; }
"{"                 {if(inside_class) open_braces++; printToken("PUNCTUATOR", yytext); return LBRACE; }
"}"                 { printToken("PUNCTUATOR", yytext); return RBRACE; }
"["                 { printToken("PUNCTUATOR", yytext); return LBRACKET; }
"]"                 { printToken("PUNCTUATOR", yytext); return RBRACKET; }




.                   { errors.addError(line_num, (std::string("LEXICAL ERROR: ") + yytext).c_str()); }


%%


