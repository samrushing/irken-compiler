# -*- Mode: Python -*-

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

counter = 0

def gensym (name):
    global counter
    r = '%s_%d' % (name, counter)
    counter += 1
    return r

class translator:
    def __init__ (self, grammar):
        self.grammar = grammar
        self.rules = []
    
    def emit (self, name, *items):
        self.rules.append ((name, items))

    def walk (self, nt, prod):
        def walk_prod (name, prod, depth):
            # walk a production.  when we hit something complicated,
            #  emit a separate rule for it and refer to it by name instead.
            if is_a (prod, tuple):
                # operator
                if prod[0] == 'or':
                    r0 = walk_prod (name, prod[1], depth+1)
                    r1 = walk_prod (name, prod[2], depth+1)
                    nt0 = gensym (name)
                    self.emit (nt0, r0)
                    self.emit (nt0, r1)
                    return nt0
                elif prod[0] in ('star', 'plus'):
                    # we need to emit two sub-rules
                    r0 = walk_prod (name, prod[1], depth+1)
                    # this will hold the prods for prod[1]
                    nt0 = gensym (name)
                    # this will do the kleene-ing
                    nt1 = gensym (name)
                    self.emit (nt0, r0)
                    self.emit (nt1, nt1, nt0)
                    if prod[0] == 'star':
                        self.emit (nt1)
                    else:
                        self.emit (nt1, nt0)
                    return nt1
            elif is_a (prod, list):
                # straightforward concatentation of sets of alts
                r = [walk_prod (name, x, depth+1) for x in prod]
                nt0 = gensym (name)
                self.emit (nt0, *r)
                return nt0
            elif is_a (prod, str):
                # straightforward terminal or nonterminal
                return prod
            else:
                raise ValueError
        alts = walk_prod (nt, prod, 0)
        self.emit (nt, alts)

    def simplify (self):
        # eliminate trivial reductions, find terminals
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
        # find trivial reductions
        for nt, prods in map.iteritems():
            if len(prods) == 1:
                if len(prods[0]) == 1:
                    # trivial reduction
                    simp[nt] = prods[0][0]
                elif map.has_key(nt) and use[nt] == 1:
                    # this production is used only once, inline it
                    simp[nt] = prods[0]
        # replace them
        rules = []
        for nt, prod in self.rules:
            if not simp.has_key (nt):
                prod2 = []
                for p in prod:
                    prod2.extend (lookup (p))
                rules.append ((nt, prod2))
                # assume undefined => terminal
                for p in prod2:
                    if not map.has_key (p):
                        terminals.add (p)
        self.rules = rules
        self.terminals = terminals

    def gen (self):
        for rule in self.grammar:
            nt, prod = rule
            self.walk (nt, prod)
        self.simplify()
        
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
            W ('class t_%s (T):\n' % (tok,))
            W ('    "%%token %s"\n' % (tok,))
            W ('\n')
        # emit production classes
        last_nt = None
        i = 0
        for nt, prod in self.rules:
            if last_nt != nt:
                W ('class %s (NT):\n' % (nt,))
                if last_nt is None:
                    # first rule is start
                    W ('    "%start"\n')
                else:
                    W ('    "%nonterm"\n')
                last_nt = nt
                i = 0
            # production
            W ('    def r_%d (self, *args):\n' % (i,))
            W ('        "%%reduce %s"\n' % (' '.join (prod)))
            i += 1
        W ('\n\n')
        W ('spec = Parsing.Spec (sys.modules[__name__], skinny=False, logFile="%s.log", verbose=True)\n' % (name,))

if __name__ == '__main__':
    import meta
    import os
    import sys
    name = sys.argv[1]
    base, ext = os.path.splitext (name)
    g = meta.parse_grammar (name)
    t = translator (g)
    t.gen()
    pp (t.rules)
    pp (t.terminals)
    t.emit_python (base)
