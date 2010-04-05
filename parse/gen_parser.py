# -*- Mode: Python -*-

from pdb import set_trace as trace
from pprint import pprint as pp
is_a = isinstance

# generate a Parsing.py-style parser from a Python-like metagrammar.

# To make use of the parser generated, you'll first need a lexer.
#   [note that 'meta.py' uses the python tokenizer]
# Then you'll need to add code to each 'r_%d' method to handle
#   that parse rule.
# Depending on the grammar generated, you will probably need to add
#   some precedence annotations as well.
# See Jason's 'example1.py' and 'meta.py' for examples.

def gensym (name, where):
    "generate a new non-terminal, based on <name> and <where>"
    return ':'.join ([name] + where)

class translator:
    def __init__ (self, grammar, start=None, lits=None):
        self.grammar = grammar
        if start is None:
            start = self.grammar[0][0]
        self.start = start
        self.rules = []
        if lits is None:
            lits = {}
        self.lits = lits
    
    def walk (self, nt, prod):

        def emit (name, *items):
            self.rules.append ((name, items))

        def walk_prod (name, prod, where):
            # walk a production.  when we hit something complicated,
            #  emit a separate rule for it and refer to it by name instead.
            if is_a (prod, tuple):
                # operator
                if prod[0] == 'or':
                    # nary 'or'
                    where = where + ['o']
                    alts = []
                    for i in range (len (prod[1])):
                        alts.append (walk_prod (name, prod[1][i], where+[str(i)]))
                    nt0 = gensym (name, where)
                    for alt in alts:
                        emit (nt0, alt)
                    return nt0
                elif prod[0] in ('star', 'plus'):
                    # we need to emit two sub-rules
                    r0 = walk_prod (name, prod[1], where+['s'])
                    # this will hold the prods for prod[1]
                    nt0 = gensym (name, where+['s0'])
                    # this will do the kleene-ing
                    nt1 = gensym (name, where+['s1'])
                    emit (nt0, r0)
                    emit (nt1, nt0, nt1)
                    if prod[0] == 'star':
                        emit (nt1)
                    else:
                        emit (nt1, nt0)
                    return nt1
                elif prod[0] == 'optional':
                    where = where + ['q']
                    r0 = walk_prod (name, prod[1], where)
                    nt0 = gensym (name, where)
                    emit (nt0, r0)
                    emit (nt0)
                    return nt0
                elif prod[0] == 'lit':
                    # XXX consider recording all terminals here rather than 'guessing'
                    # strip off the quotes
                    return self.name_literal (prod[1][1:-1])
                else:
                    raise ValueError
            elif is_a (prod, list):
                # straightforward concatentation of sets of alts
                where = where + ['c']
                r = []
                for i in range (len (prod)):
                    r.append (walk_prod (name, prod[i], where + [str(i)]))
                nt0 = gensym (name, where)
                emit (nt0, *r)
                return nt0
            elif is_a (prod, str):
                # straightforward terminal or nonterminal
                return prod
            else:
                raise ValueError

        nt0 = walk_prod (nt, prod, [])
        emit (nt, nt0)

    def name_literal (self, lit):
        if self.lits.has_key (lit):
            return self.lits[lit]
        else:
            safe = True
            for ch in lit:
                if not (ch.isalpha() or ch == '_'):
                    safe = False
                    break
            if not safe:
                name = 'term_%d' % (len(self.lits))
            else:
                name = lit
            self.lits[lit] = name
            return name

    def simplify (self):
        # eliminate trivial reductions
        simp = {}
        def lookup (k):
            if is_a (k, str):
                if simp.has_key (k):
                    return lookup (simp[k])
                else:
                    return [k]
            else:
                r = []
                for x in k:
                    r.extend (lookup (x))
                return r
        terminals = set()
        map = {}
        use = {}
        for nt, prod in self.rules:
            if not map.has_key (nt):
                map[nt] = []
                use[nt] = 0
            map[nt].append (prod)
        # count uses
        for nt, prod in self.rules:
            for item in prod:
                if map.has_key (item):
                    use[item] += 1
        # find trivial reductions (generated only)
        for nt, prods in map.iteritems():
            if len(prods) == 1 and ':' in nt:
                if len(prods[0]) == 1:
                    # trivial reduction
                    simp[nt] = prods[0][0]
                elif map.has_key(nt) and use[nt] == 1:
                    # this production is used only once, inline it
                    simp[nt] = prods[0]
        # replace them
        map = {}
        for nt, prod in self.rules:
            if not simp.has_key (nt):
                prod2 = []
                for p in prod:
                    prod2.extend (lookup (p))
                if not map.has_key (nt):
                    map[nt] = []
                map[nt].append (prod2)
        # second pass for trivial user rules
        rules = []
        rename = {}
        for nt, prods in map.iteritems():
            # rule0: rule1 (unless rule1 is a terminal!)
            if len(prods) == 1 and len(prods[0]) == 1 and map.has_key (prods[0][0]):
                rename[prods[0][0]] = nt
        for nt, prods in map.iteritems():
            if len(prods) == 1 and len(prods[0]) == 1 and map.has_key (prods[0][0]):
                pass
            else:
                nt = rename.get (nt, nt)
                for prod in prods:
                    prod = [ rename.get (x, x) for x in prod ]
                    rules.append ((nt, prod))
        self.rules = rules

    def find_terminals (self):
        nts = set()
        terminals = set()
        for nt, prod in self.rules:
            nts.add (nt)
        for nt, prod in self.rules:
            # assume undefined => terminal
            for p in prod:
                if p not in nts:
                    terminals.add (p)
        return terminals

    def gen (self):
        seen = set()
        for rule in self.grammar:
            nt, prod = rule
            if nt in seen:
                raise ValueError ("duplicate production: %r" % (nt,))
            else:
                seen.add (nt)
            self.walk (nt, prod)
        self.simplify()
        self.terminals = self.find_terminals()
        pp (self.rules)
        #self.renumber()
        
    def emit_python (self, name):
        # emit the grammar in the form required by Jason Evans' Parsing.py module.
        file = open ('%s.py' % (name,), 'wb')
        W = file.write
        W ('# -*- Mode: Python -*-\n\n')
        W ('import sys\n')
        W ('import Parsing\n\n')
        W ('T = Parsing.Token\n')
        W ('NT = Parsing.Nonterm\n\n')
        # emit token classes
        for tok in self.terminals:
            lname = self.lits.get (tok, tok)
            W ('class t_%s (T):\n' % (lname,))
            W ('    "%%token %s [p1]"\n' % (lname,))
            W ('\n')
        # emit production classes
        W ('class p1 (Parsing.Precedence):\n    "%right p1"\n')
        last_nt = None
        i = 0
        for nt, prod in self.rules:
            if last_nt != nt:
                W ('class %s (NT):\n' % (fix(nt),))
                if nt == self.start:
                    W ('    "%start"\n')
                else:
                    W ('    "%nonterm"\n')
                last_nt = nt
                i = 0
            # production
            W ('    def r_%d (self, *args):\n' % (i,))
            W ('        "%%reduce %s [p1]"\n' % (' '.join ([fix(x) for x in prod])))
            i += 1
        W ('\n\n')
        W ('spec = Parsing.Spec (sys.modules[__name__], skinny=False, logFile="%s.log", verbose=True)\n' % (name,))

def fix (s):
    "make this non-terminal a legal python identifier"
    return s.replace (':','_')

def go (filename, start):
    import meta
    import os
    base, ext = os.path.splitext (name)
    g = meta.parse_grammar (name)
    t = translator (g, start)
    t.gen()
    #pp (t.rules)
    #pp (t.terminals)
    pp (t.lits)
    t.emit_python (base)

if __name__ == '__main__':
    import sys
    name = sys.argv[1]
    if len (sys.argv) > 2:
        start = sys.argv[2]
    else:
        start = None
    go (name, start)
