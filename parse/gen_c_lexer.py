# -*- Mode: Python -*-

import lexer

keywords = [
    "auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else",
    "enum", "extern", "float", "for", "goto", "if", "int", "long", "register", "return",
    "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union",
    "unsigned", "void", "volatile", "while"
    ]

# these are keywords we want to pretend don't even exist
# XXX some of these are still in keywords, above.
strip_keywords = [
    # clang
    "_Nullable", "_Nonnull", "_Null_unspecified",
    # C
    "const", "restrict", "volatile", "_Atomic",
    # gcc
    "__extension__",
    ]

keywords = [ x for x in keywords if x not in strip_keywords ]

literals = [
    ('%', 'percent'),
    ('&', 'ampersand'),
    ('&&', 'ampersand2'),
    ('(', 'lparen'),
    (')', 'rparen'),
    ('*', 'splat'),
    ('**', 'splatsplat'),
    ('+', 'plus'),
    (',', 'comma'),
    ('-', 'minus'),
    ('.', 'dot'),
    ('/', 'slash'),
    (':', 'colon'),
    (';', 'semicolon'),
    ('<<', 'lshift'),
    ('=', 'equals'),
    ('>>', 'rshift'),
    ('@', 'atsign'),
    ('[', 'lbracket'),
    (']', 'rbracket'),
    ('^', 'caret'),
    ('`', 'backquote'),
    ('{', 'lbrace'),
    ('|', 'vbar'),
    ('||', 'vbar2'),
    ('}', 'rbrace'),
    ('~', 'tilde'),
    ('?', 'question'),
    ('...', 'dotdotdot'),
    ]

def safe (lit):
    r = []
    for ch in lit:
        if ch in ".()[]*+|?":
            r.append ('\\')
        r.append (ch)
    return ''.join (r)

def make_alt (choices):
    return '(' + '|'.join (choices) + ')'

augassign = make_alt (r'\+ - \* / % & \| \^ << >> \*\*'.split()) + '='
comp_op   = make_alt (['<','>','==','>=','<=','<>','!='])
string    = r'"([^\\\n"]|(\\.))*"'
char      = r"'([^']+)'"
strip     = make_alt (strip_keywords)

lexicon = [
    (r'[ \n\t]+',               'WHITESPACE'),
    (r'#[^\n]*\n',              'CPPLINE'),
    # XXX WHY does this not work?
    #(r'//[^\n]*\n',       'COMMENT'),
    (strip,                     'STRIP'),
    # put keywords before IDENT to resolve ambiguity.
    ] + [(k,k.upper()) for k in keywords] + [
    # special cases
    ('__attribute__',           'ATTRIBUTE'),
    #('__extension__',           'EXTENSION'),
    (r'[A-Za-z_][A-Za-z0-9_]*', 'IDENT'),
    (r'[0-9]+',                 'NUMBER'),
    (string,                    'STRING'),
    (char,                      'CHAR'),
    (comp_op,                   'COMP_OP'),
    #(augassign,                 'AUGASSIGN'),
    ] + [(safe(k),v.upper()) for (k,v) in literals]

lit_map = {}
for k, v in literals:
    lit_map[k] = v
for k in keywords:
    lit_map[k] = k

def make_lexer (path):
    m = lexer.lexer (lexicon)
    m.gen_irken (open (path, 'wb'), 'c')

make_lexer("clextab.scm")

