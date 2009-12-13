# -*- Mode: Python -*-

# This uses Jason Evans' Parsing.py LR(1) module.
# http://www.canonware.com/Parsing/
#
# a parser for the BNF-like python meta-grammar.
#
# use the builtin python tokenizer on the grammar,
#  just like python does when building itself.

# grammar grammar grammar grammar grammar....

import Parsing
import sys

class token (Parsing.Token):
    def __init__ (self, parser, val):
        Parsing.Token.__init__ (self, parser)
        self.val = val

T = token

class t_IDENT (T):
    "%token IDENT"

class t_STRING (T):
    "%token STRING"

class t_COLON (T):
    "%token COLON"

class t_LPAREN (T):
    "%token LPAREN"

class t_RPAREN (T):
    "%token RPAREN"

class t_LBRACKET (T):
    "%token LBRACKET"

class t_RBRACKET (T):
    "%token RBRACKET"

class t_STAR (T):
    "%token STAR"

class t_PLUS (T):
    "%token PLUS"

class t_VBAR (T):
    "%token VBAR"

class t_NEWLINE (T):
    "%token NEWLINE"

NT = Parsing.Nonterm

class rules (NT):
    "%start"
    def reduce_0 (self, *args):
        "%reduce rule"
        self.val = args[0].val
    def reduce_1 (self, *args):
        "%reduce rules rule"
        self.val = args[0].val + args[1].val

class rule (NT):
    "%nonterm"
    def reduce_0 (self, *args):
        "%reduce IDENT COLON alts NEWLINE"
        self.val = [(args[0].val, args[2].val)]
    def reduce_1 (self, *args):
        "%reduce NEWLINE"
        self.val = []

class alts (NT):
    "%nonterm"
    def reduce_0 (self, *args):
        "%reduce alts VBAR items"
        self.val = ('or', args[0].val, args[2].val)
    def reduce_1 (self, *args):
        "%reduce items"
        self.val = args[0].val

class items (NT):
    "%nonterm"
    def reduce_0 (self, *args):
        "%reduce item"
        self.val = args[0].val
    def reduce_1 (self, *args):
        "%reduce item items"
        self.val = args[0].val + args[1].val

class item (NT):
    "%nonterm"
    def reduce_0 (self, *args):
        "%reduce optional"
        self.val = [args[0].val]
    def reduce_1 (self, *args):
        "%reduce kleene"
        self.val = [args[0].val]

class element (NT):
    "%nonterm"
    def reduce_0 (self, *args):
        "%reduce IDENT"
        self.val = args[0].val
    def reduce_1 (self, *args):
        "%reduce group"
        self.val = args[0].val
    def reduce_2 (self, *args):
        "%reduce STRING"
        self.val = ('lit', args[0].val)

class kleene (NT):
    "%nonterm"
    def reduce_0 (self, *args):
        "%reduce element"
        self.val = args[0].val
    def reduce_1 (self, *args):
        "%reduce element STAR"
        self.val = ('star', args[0].val)
    def reduce_2 (self, *args):
        "%reduce element PLUS"
        self.val = ('plus', args[0].val)

class group (NT):
    "%nonterm"
    def reduce_0 (self, *args):
        "%reduce LPAREN alts RPAREN"
        self.val = args[1].val

class optional (NT):
    "%nonterm"
    def reduce_0 (self, *args):
        "%reduce LBRACKET alts RBRACKET"
        self.val = ('optional', args[1].val)

class parser (Parsing.Lr):
    
    def scan (self, stream):
        while 1:
            code, val = stream.next()
            if code == 'ENDMARKER':
                self.eoi()
                break
            else:
                tok = self.translate_token (code, val)
                self.token (tok)
        r = self._stack[1][0]
        return r.val

    op_toks = {
        ':' : t_COLON,
        '(' : t_LPAREN,
        ')' : t_RPAREN,
        '[' : t_LBRACKET,
        ']' : t_RBRACKET,
        '+' : t_PLUS,
        '*' : t_STAR,
        '|' : t_VBAR,
        }

    def translate_token (self, code, val):
        # we need to translate a token from the tokenizer module
        #  into a one acceptable by this parser.
        if code == 'NAME':
            return t_IDENT (self, val)
        elif code == 'STRING':
            return t_STRING (self, val)
        elif code == 'OP':
            return self.op_toks[val](self, val)
        elif code == 'NEWLINE':
            return t_NEWLINE (self, val)
        else:
            raise ValueError

def tokenize_grammar (filename):
    import tokenize
    f = open (filename, 'rb')
    g = tokenize.generate_tokens (f.readline)
    l = []
    while g:
        code, val, spos, epos, line = g.next()
        name = tokenize.tok_name[code]
        # see tokenize docs to grok the 'NL' token
        if name not in ('COMMENT', 'NL', 'INDENT'):
            #print '*****', code, name, repr(val)
            #print
            yield (tokenize.tok_name[code], val)

spec = Parsing.Spec (
    sys.modules[__name__],
    pickleFile="meta.pickle",
    #skinny=False,
    #logFile="meta.log",
    #graphFile="meta.dot",
    #verbose=True
    )

def parse_grammar (filename):
    p = parser (spec)
    #p.verbose = True
    g = tokenize_grammar (filename)
    return p.scan (g)

if __name__ == '__main__':
    import sys
    from pprint import pprint as pp
    if len(sys.argv) < 2:
        print 'Usage: %s <Grammar>' % (sys.argv[0],)
        print '  try the python grammar from the source distribution:'
        print '  e.g., "Python-2.6.4/Grammar/Grammar"'
    else:
        pp (parse_grammar (sys.argv[1]))
