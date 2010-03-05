
import lexer
import gen_parser

keywords  = [
    'and', 'as', 'assert', 'break', 'class', 'continue', 'def', 'del', 'elif', 'else', 'except',
    'exec', 'finally', 'for', 'from', 'global', 'if', 'import', 'in', 'is', 'lambda', 'not', 'or',
    'pass', 'print', 'raise', 'return', 'try', 'while', 'with', 'yield'
    ]

literals = [
    ('%', 'percent'),
    ('&', 'ampersand'),
    ('(', 'lparen'),
    (')', 'rparen'),
    ('*', 'splat'),
    ('**', 'splatsplat'),
    ('+', 'plus'),
    (',', 'comma'),
    ('-', 'minus'),
    ('.', 'dot'),
    ('/', 'slash'),
    ('//', 'slashslash'),
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
    ('}', 'rbrace'),
    ('~', 'tilde'),
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

augassign = make_alt (r'\+ - \* / % & \| \^ << >> \*\* //'.split()) + '='
comp_op   = make_alt (['<','>','==','>=','<=','<>','!='])
string    = make_alt ([r'"([^\\\n"]|(\\.))*"', r"'([^\\\n']|(\\.))*'"])

lexicon = [
    (r'[ \t]+',                 'whitespace'),
    (r'[ \t]*#[^\n]*\n',        'comment'),
    (r'[\n]',                   'NEWLINE'),
    # put keywords before NAME to resolve ambiguity.
    ] + [(k,k) for k in keywords] + [
    (r'[A-Za-z_][A-Za-z0-9_]*', 'NAME'),
    (r'[0-9]+',                 'NUMBER'),
    (string,                    'STRING'),
    (comp_op,                   'COMP_OP'),
    (augassign,                 'AUGASSIGN'),
    ] + [(safe(k),v) for (k,v) in literals]

lit_map = {}
for k, v in literals:
    lit_map[k] = v
for k in keywords:
    lit_map[k] = k

def make_lexer():
    m = lexer.lexer (lexicon)
    #m.read (open ("../nodes.py"))
    m.gen_irken (open ("lexstep.scm", 'wb'))

def make_parser (path):
    import meta
    import os
    from pprint import pprint as pp
    base, ext = os.path.splitext (path)
    g = meta.parse_grammar (path)
    t = gen_parser.translator (g, lits=lit_map)
    t.gen()
    #pp (t.rules)
    #pp (t.terminals)
    t.emit_python (base)

make_lexer()
make_parser ('t0.g')
