# -*- Mode: Python -*-
# based on 1.5.2, stripped down
file_input: (NEWLINE | stmt)*

funcdef: 'def' NAME '(' [varargslist] ')' ':' suite
varargslist: NAME (',' NAME)* [',']

stmt: simple_stmt | compound_stmt
simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
small_stmt: expr_stmt | pass_stmt | flow_stmt
expr_stmt: testlist ('=' testlist)*

pass_stmt: 'pass'

flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt
break_stmt: 'break'
continue_stmt: 'continue'
return_stmt: 'return' [testlist]

compound_stmt: if_stmt | while_stmt | for_stmt | funcdef

if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
while_stmt: 'while' test ':' suite ['else' ':' suite]
for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT

test: or_test | lambdef
or_test: and_test ('or' and_test)*
and_test: not_test ('and' not_test)*
not_test: 'not' not_test | comparison
comparison: expr (comp_op expr)*

# partially done by the lexer
#comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
comp_op: COMP_OP | 'in' | 'not' 'in' | 'is' | 'is' 'not'

expr: xor_expr ('|' xor_expr)*
xor_expr: and_expr ('^' and_expr)*
and_expr: shift_expr ('&' shift_expr)*
shift_expr: arith_expr (('<<'|'>>') arith_expr)*
arith_expr: term (('+'|'-') term)*
term: factor (('*'|'/'|'%') factor)*
factor: ('+'|'-'|'~') factor | power
power: atom trailer* ('**' factor)*
atom: NAME | NUMBER | STRING+
lambdef: 'lambda' [varargslist] ':' test
trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
subscriptlist: subscript (',' subscript)* [',']
subscript: '.' '.' '.' | test | [test] ':' [test] [sliceop]
sliceop: ':' [test]
exprlist: expr (',' expr)* [',']
testlist: test (',' test)* [',']

arglist:  argument (',' argument)* [',']
argument: [test '='] test	# Really [keyword '='] test
