# -*- Mode: Python -*-

import charset
import regular
import automata
from pprint import pprint as pp
import sys
W = sys.stdout.write

# two approaches for generating a lexer:
#  1) generate tables and dispatch through them at run-time.
#  2) generate range-test code from the dfa.

# #2 should be faster, especially if we try a little harder.
# idea: feed a bunch of sample data through the lexer, collecting
#   probabilities at each transition, use this to reorder them.
#   [a branch predictor will do the same thing, is it worth it?]

# I think tables are better once the lexer hits a certain size.

class lexer:

    def __init__ (self, lexicon):
        # combine all the regexps into one, with each sub-expression wrapped with TOKEN
        tokens = regular.n_or ([ (regular.TOKEN, regular.parse (x), y) for (x, y) in lexicon ])
        #pp (tokens)
        c0 = automata.regexp_to_nfa_converter()
        nfa, start, end = c0.go (tokens)
        #pp (nfa)
        c1 = automata.nfa_dfa_converter (nfa, start, end)
        dfa, finals, actions = c1.go (c0.tokens)
        flat = c1.coalesce()
        #print 'dfa='
        #pp (flat)
        #print 'finals=', finals
        self.dfa = flat
        #print 'actions=', actions
        # XXX finals + actions == redundant
        self.finals = finals
        self.actions = {}
        for state, action in actions:
            if not self.actions.has_key (state):
                # give preference to tokens defined earlier in the lexicon
                self.actions[state] = action
        #print 'actions=', self.actions
            
    def sort_transitions (self, trans):
        # sort transitions in a way that generates the most efficient set of
        #   range tests.
        tests = [ (sym.as_ranges()[1], sym, ts) for (sym, ts) in trans ]
        tests.sort (lambda a,b: cmp (len(a[0]), len(b[0])))
        return [ (sym, ts) for (ranges, sym, ts) in tests ]

    def read (self, file):
        # this is useful for debugging your lexicon.  feed it data you'd
        #   like to lex.  when it works the way you like, then call
        #   gen_irken/c/etc() on it.
        s = file.read()
        state = 0
        current = []
        good = None
        slen = len(s)
        i = 0
        for ch in s:
            while 1:
                entry = self.dfa[state]
                ostate = state
                for sym, ts in entry:
                    if sym.has (ch):
                        state = ts
                        break
                print '%d %r => %d' % (ostate, ch, state)
                #W ('[%d|%d]' % (ord(ch),state,))
                action = self.actions.get (state, None)
                if not good and action:
                    # transition in
                    good = action
                    break
                elif good and not action:
                    # transition out
                    print good, repr(''.join (current))
                    current = []
                    state = 0
                    good = None
                else:
                    # update final state if one...
                    good = action
                    break
            current.append (ch)
            i += 1

    def generate (self):
        print     "    def step (self, ch):"
        for i in range (len (self.dfa)):
            if i == 0:
                print "        if self.state == %d:" % i
            else:
                print "        elif self.state == %d:" % i
            state_len = len (self.dfa[i])
            trans = self.sort_transitions (self.dfa[i])
            for j in range (state_len):
                sym, ts = trans[j]
                if state_len == 1:
                    print "            self.state = %d" % (ts,)
                elif j == state_len - 1:
                    # last range is always a catch-all
                    print "            else:"
                    print "                self.state = %d" % (ts,)
                else:
                    invert, ranges = sym.as_ranges()
                    test = []
                    for start, end in ranges:
                        if start == end:
                            test.append ("ch == %d" % (start,))
                        else:
                            test.append ("%d <= ch <= %d" % (start, end))
                    test = ' or '.join (test)
                    if invert:
                        test = 'not (%s)' % (test,)
                    if j == 0:
                        print "            if %s:" % (test,)
                    else:
                        print "            elif %s:" % (test,)
                    print "                self.state = %d" % (ts,)
            if self.actions.has_key (i):
                print "                # TOKEN %s" % (self.actions[i],)

    def gen_c (self):
        print "int step (int state, char ch)"
        print "{"
        for i in range (len (self.dfa)):
            if i == 0:
                print "  if (state == %d) {" % i
            else:
                print "  } else if (state == %d) {" % i
            state_len = len (self.dfa[i])
            trans = self.sort_transitions (self.dfa[i])
            for j in range (state_len):
                sym, ts = trans[j]
                if state_len == 1:
                    print "    state = %d;" % (ts,)
                elif j == state_len - 1:
                    # last range is always a catch-all
                    print "    } else {"
                    print "      state = %d;" % (ts,)
                    print "    }"
                else:
                    invert, ranges = sym.as_ranges()
                    test = []
                    for start, end in ranges:
                        if start == end:
                            test.append ("(ch == %d)" % (start,))
                        else:
                            test.append ("(ch >= %d && ch <= %d)" % (start, end))
                    test = ' || '.join (test)
                    if invert:
                        test = '!(%s)' % (test,)
                    if j == 0:
                        print "    if (%s) {" % (test,)
                    else:
                        print "    } else if (%s) {" % (test,)
                    print "      state = %d;" % (ts,)
        print "  }"
        print "  return state;"
        print "}"

    def find_sink (self):
        # is the sink is always the second state?
        for i in range (len (self.dfa)):
            entry = self.dfa[i]
            if len(entry) == 1:
                [(sym, ts)] = entry
                if sym == charset.DOT and ts == i:
                    return i
        else:
            raise ValueError
        
    def gen_irken (self, file):
        # generate a table-based dfa, using strings
        assert (len (self.dfa) < 256)
        W = file.write
        W (";; generated by lexer.py\n")
        tables = []
        for i in range (len (self.dfa)):
            table = [1] * 256
            for sym, ts in self.dfa[i]:
                for i in range (256):
                    if sym[i]:
                        table[i] = ts
            table = ''.join ([chr(x) for x in table])
            tables.append (table)
        unique = {}
        for t in tables:
            if not unique.has_key (t):
                unique[t] = len (unique)
        indirect = []
        for t in tables:
            indirect.append (unique[t])
        W ("(define dfa \n")
        W ("  (let* (\n")
        items = unique.items()
        items.sort (lambda a,b: cmp (a[1], b[1]))
        for table, index in items:
            W ("         (t%d \"%s\")\n" % (index, ''.join (["\\x%02x" % (ord (x),) for x in table])))
        W ("        )\n")
        W ("   #(%s)))\n" % (" ".join (["t%d" % (x,) for x in indirect])))

        # find the sink state
        sink = self.find_sink()
        
        W ("(define finals\n")
        W ("  #(\n")
        for i in range (len (self.dfa)):
            f = self.actions.get (i, None)
            if f:
                W ("  '%s\n" % f)
            else:
                W ("  'not-final\n")
        W ("  ))\n")
        W ("(define (step ch state)\n")
        W ("  (char->ascii (string-ref dfa[state] (char->ascii ch))))\n")
        return tables

keywords = '|'.join ('and is in not if then else elif yield while for try def class'.split())
augassign = ('(' + '|'.join (r'\+ - \* / % & \| \^ << >> \*\* //'.split()) + ')=')

python_lexicon = [
    (r'[0-9]+',                 'number'),
    (r'[A-Za-z_][A-Za-z0-9_]*', 'ident'),
    (r'"([^\\\n"]|(\\.))*"',    'string1'),
    (r"'([^\\\n']|(\\.))*'",    'string2'),
    (r'[ \t]*#[^\n]*\n',        'comment'),
    (r'[-+]',                   'addop'),
    (r'([*/%]|//)',             'mulop'),
    (r'<|>|<=|>=|==',           'compare'),
    (r'<<|>>',                  'shift'),
    (r'\*\*',                   'power'),
    (r'[ \t]+',                 'whitespace'),
    (r'[\n]',                   'newline'),
    (r'\.',                     'getattr'),
    (r'=',                      'assign'),
    (r'\(',                     'lparen'),
    (r'\)',                     'rparen'),
    (r',',                      'comma'),
    (r':',                      'colon'),
    (r';',                      'semicolon'),
    (r'{',                      'lbracket'),
    (r'}',                      'rbracket'),
    (r'\[',                     'lbrace'),
    (r'\]',                     'rbrace'),
    (r'&',                      'bitand'),
    (r'\|',                     'bitor'),
    (r'^',                      'bitxor'),
    (r'~',                      'bitnot'),
    (keywords,                  'keyword'),
    (augassign,                 'augassign'),
    ]

if __name__ == '__main__':
    m = lexer (python_lexicon)
    #m.gen_scheme_code()
    #m.gen_scheme_table()
    m.gen_irken (open ("lexstep.scm", 'wb'))
    #m.read (open ("../nodes.py"))
