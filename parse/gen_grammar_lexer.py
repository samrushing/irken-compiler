# -*- Mode: Python -*-

import lexer

# lexer for metagrammar

keywords = []
literals = [
    (':', 'COLON'),
    ('|', 'VBAR'),
    (';', 'SEMICOLON'),
    # ('(', 'lparen'),
    # (')', 'rparen'),
    # ('[', 'lbracket'),
    # (']', 'rbracket'),
    # ('+', 'plus'),
    # ('*', 'star'),
    # ('|', 'vbar')
    ]

def safe (lit):
    r = []
    for ch in lit:
        if ch in ".()[]*+|?":
            r.append ('\\')
        r.append (ch)
    return ''.join (r)

string = r"'([^\\\n']|(\\.))*'"

lexicon = [
    (r'[ \n\t]+',               'WHITESPACE'),
    (r'[ \t]*#[^\n]*\n',        'COMMENT'),
    (r'[ \t]*//[^\n]*\n',       'COMMENT'),
    (r'[A-Za-z_][A-Za-z0-9_]*', 'NAME'),
    (string,                    'STRING'),
    ] + [(safe(k),v) for (k,v) in literals]

lit_map = {}
for k, v in literals:
    lit_map[k] = v
for k in keywords:
    lit_map[k] = k

def make_lexer (path):
    m = lexer.lexer (lexicon)
    m.gen_irken (open (path, 'wb'), 'g')

make_lexer ("glextab.scm")
