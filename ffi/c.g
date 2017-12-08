// grammar for C types
int_type_base: CHAR | SHORT | INT | LONG | UNSIGNED | SIGNED;
int_type: int_type int_type_base | int_type_base;
float_type: FLOAT | DOUBLE | LONG FLOAT | LONG DOUBLE;
type
    : int_type
    | float_type
    | VOID
    | IDENT
    | struct_or_union_decl
    | type pointer
    ;
// XXX pointer attributes _Nullable, etc...
pointer: SPLAT;
typedef: TYPEDEF type_declarator SEMICOLON;
// XXX apparently the parens around the IDENT are optional.
type_declarator
    : type declaratorSpecifier
    | type LPAREN SPLAT IDENT RPAREN signature
    ;
declaratorSpecifier
    : declarator
    | declarator attribute_specifier
    ;

declarator
    : IDENT
    | declarator LBRACKET RBRACKET
    | declarator LBRACKET NUMBER RBRACKET
    | declarator LBRACKET expression RBRACKET
    ;
struct_or_union: STRUCT | UNION;
struct_or_union_decl
    : struct_or_union       LBRACE struct_elems RBRACE
    | struct_or_union IDENT LBRACE struct_elems RBRACE
    | struct_or_union IDENT
    ;
struct_elems: struct_elem | struct_elems struct_elem;
struct_elem: type_declarator SEMICOLON;
signature: LPAREN RPAREN | LPAREN arglist RPAREN;
arglist: arglist COMMA arg | arg;
arg: type | type IDENT;

// stripped down from the C11 expression grammar.
//
// Everything below here is needed *ONLY* because of types like this:
//   ...
//   int32_t fds_bits[VERY_COMPLEX_EXPRESSION];
//   ...

primaryExpression
    : IDENT
    | NUMBER
    | LPAREN expression RPAREN
    | SIZEOF LPAREN type RPAREN
    ;

castExpression
    : primaryExpression
    | LPAREN type RPAREN castExpression
    ;

multiplicativeExpression
    :   castExpression
    |   multiplicativeExpression SPLAT castExpression
    |   multiplicativeExpression SLASH castExpression
    |   multiplicativeExpression PERCENT castExpression
    ;

additiveExpression
    :   multiplicativeExpression
    |   additiveExpression PLUS multiplicativeExpression
    |   additiveExpression MINUS multiplicativeExpression
    ;

shiftExpression
    :   additiveExpression
    |   shiftExpression LSHIFT additiveExpression
    |   shiftExpression RSHIFT additiveExpression
    ;

comparisonExpression
    :   shiftExpression
    |   comparisonExpression COMP_OP shiftExpression
    ;

andExpression
    :   comparisonExpression
    |   andExpression AMPERSAND equalityExpression
    ;

exclusiveOrExpression
    :   andExpression
    |   exclusiveOrExpression CARET andExpression
    ;

inclusiveOrExpression
    :   exclusiveOrExpression
    |   inclusiveOrExpression VBAR exclusiveOrExpression
    ;

logicalAndExpression
    :   inclusiveOrExpression
    |   logicalAndExpression AMPERSAND2 inclusiveOrExpression
    ;

logicalOrExpression
    :   logicalAndExpression
    |   logicalOrExpression VBAR2 logicalAndExpression
    ;

conditionalExpression
    :   logicalOrExpression
    |   logicalOrExpression QUESTION expression COLON conditionalExpression
    ;

expression: conditionalExpression;

// gcc __attribute__
// see https://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html
// gcc's version of an s-expression tag.

attribute_specifier
    : ATTRIBUTE LPAREN LPAREN RPAREN RPAREN
    | ATTRIBUTE LPAREN LPAREN attributeList RPAREN RPAREN
    ;

attributeList
    : attributeList COMMA attribute
    | attribute
    ;

attribute
    : IDENT
    | IDENT EQUALS expression
    | IDENT LPAREN attributeList RPAREN
    ;
