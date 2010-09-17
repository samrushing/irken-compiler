# -*- Python -*-

# See "The Implementation of Functional Programming Languages",
# Chapter 5: "Efficient Compilation of Pattern-Matching".
# http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/
#
# Thanks for the hint, OCaml people! (Xavier Leroy?) They were kind enough to put this reference in
#   their source code (ocaml/bytecomp/matching.ml), otherwise I may have never found out about this
#   book.  And thanks to Simon Peyton-Jones for putting his book online.

is_a = isinstance
from pdb import set_trace as trace
from pprint import pprint as pp
from lisp_reader import atom

class variable:
    # creates a binding
    def __init__ (self, name):
        self.name = name
    def __repr__ (self):
        return '<%s>' % (self.name,)

class literal:
    # matches a literal
    def __init__ (self, value):
        self.value = value
    def __repr__ (self):
        return 'L%s' % (repr(self.value))
    def __cmp__ (self, other):
        if is_a (other, literal):
            v = self.value
            o = other.value
            return cmp ((v.kind,v.value), (o.kind,o.value))
        else:
            return -1

class constructor:
    # matches a constructor
    def __init__ (self, name, subs):
        self.datatype, self.alt = name.split (':')
        self.subs = subs
    def __repr__ (self):
        return '(%s/%s %s)' % (self.datatype, self.alt, ' '.join ([repr(x) for x in self.subs]))

class record:
    def __init__ (self, pairs):
        self.pairs = pairs
    def __repr__ (self):
        l = []
        for i in range (len (self.pairs)):
            name, sub = self.pairs[i]
            l.append ('%s=%r' % (name, sub))
        return '{%s}' % (' '.join (l))

# bad match
class MatchError:
    pass

FAIL = ['%%fail']
ERROR = ['%%match-error']

# The next step in this code is to try to optimize the generated tree, which should be a matter of
#   using heuristics to pick which pattern out of several to begin with.  This code always starts
#   with the left-most pattern, and descends recursively; see first_pats_are() below.
    
class compiler:

    def __init__ (self, context):
        self.context = context
        self.gensym_counter = 0

    def gensym (self):
        c = self.gensym_counter
        self.gensym_counter += 1
        return 'm%d' % (c,)

    def compile (self, rules, vars):
        # how many pattern args?
        nrules = len (rules)
        pats, result = rules[0]
        npats = len (pats)
        #vars = [ self.gensym() for x in range (npats) ]
        for pats, result in rules[1:]:
            # must have the same number of patterns in each
            assert (len(pats) == npats)
        rules0 = []
        for pats, code in rules:
            kinds = [ self.kind (x) for x in pats ]
            rules0.append ((kinds, code))
        return vars, self.match (vars, rules0, ERROR)
            
    def kind (self, p):
        if is_a (p, list):
            if len(p) == 0:
                # () -> (list:nil)
                return constructor ('list:nil', [])
            elif p[0] == 'quote':
                # a symbol
                assert (is_a (p[1], str))
                return literal (atom ('symbol', p[1]))
            elif is_a (p[0], list) and p[0][0] == 'colon' and len(p[0]) == 3:
                # a constructor
                return constructor ('%s:%s' % (p[0][1], p[0][2]), [self.kind (x) for x in  p[1:]])
            else:
                # (a b . c) => (list:cons ...)
                if p[0] == '.':
                    # cdr
                    return self.kind (p[1])
                else:
                    return constructor ('list:cons', [self.kind (p[0]), self.kind (p[1:])])
        elif is_a (p, str):
            return variable (p)
        elif is_a (p, atom) and p.kind == 'record':
            return record ([(name, self.kind (sub)) for (name, sub) in p.value])
        else:
            return literal (p)

    def first_pats_are (self, rules, kind):
        # are the first patterns in each rule of <kind>?
        for pats, code in rules:
            if not is_a (pats[0], kind):
                return False
        else:
            return True

    def match (self, vars, rules, default):
        #print '-------- match -------------'
        #pp ((vars, rules, default))
        # the empty rule
        if not vars:
            if len(rules):
                empty_pat, code = rules[0]
                return code
            else:
                return default
        # if every rule begins with a variable
        # apply if every rule begins with a variable
        if self.first_pats_are (rules, variable):
            vars, rules, default = self.variable_rule (vars, rules, default)
            return self.match (vars, rules, default)
        # if every rule is a constructor (i.e., no variables)
        if self.first_pats_are (rules, constructor):
            return self.constructor_rule (vars, rules, default)
        if self.first_pats_are (rules, record):
            return self.record_rule (vars, rules, default)
        # if every rule is a constant
        if self.first_pats_are (rules, literal):
            return self.constant_rule (vars, rules, default)
        # we have a mixture of variables and constructors..
        return self.mixture_rule (vars, rules, default)

    def subst (self, var0, var1, code):
        # this will record a subst to be applied during node building (nodes.py)
        if var1 == '_':
            # unless it's a wildcard, no need.
            return code
        elif is_a (code, list) and len(code) and code[0] == 'let_subst':
            return ['let_subst', code[1] + [(var1, var0)], code[2]]
        else:
            return ['let_subst', [(var1, var0)], code]

    def variable_rule (self, vars, rules, default):
        # if every rule begins with a variable, we can remove that column
        #  from the set of patterns and substitute the var within each body.
        var = vars[0]
        vars = vars[1:]
        rules0 = []
        for pats, code in rules:
            rules0.append ((pats[1:], self.subst (var, pats[0].name, code)))
        return vars, rules0, default

    def fatbar (self, e1, e2):
        if e1 == FAIL:
            return e2
        elif e2 == FAIL:
            return e1
        else:
            return ['%%fatbar', e1, e2]

    def constructor_rule (self, vars, rules, default):
        # ok, group them by constructor (retaining the order within each constructor alt).
        alts = {}
        datatype = rules[0][0][0].datatype
        dt = self.context.datatypes[datatype]
        for pats, code in rules:
            alt = pats[0].alt
            # XXX raise this as a real syntax error...
            assert (pats[0].datatype == datatype)
            if not alts.has_key (alt):
                alts[alt] = [(pats, code)]
            else:
                alts[alt].append ((pats, code))
        cases = []
        if default != ERROR:
            default0 = FAIL
        else:
            default0 = default
        for alt, rules0 in alts.iteritems():
            # new variables to stand for the fields of the constructor
            vars0 = [ self.gensym() for x in range (dt.arity (alt)) ]
            wild  = [ True for x in vars0 ]
            rules1 = []
            for pats, code in rules0:
                rules1.append ((pats[0].subs + pats[1:], code))
                for i in range (len (pats[0].subs)):
                    sub = pats[0].subs[i]
                    if not (is_a (sub, variable) and sub.name == '_'):
                        wild[i] = False
            # if every pattern has a wildcard for this arg of the constructor,
            #   then use '_' rather than the symbol we generated.
            vars1 = vars0[:]
            for i in range (len (vars0)):
                if wild[i]:
                    vars1[i] = '_'
            cases.append (
                [[['colon', None, alt]] + vars1, self.match (vars0 + vars[1:], rules1, default0)]
                )
        if len(alts) < len (dt.alts):
            # an incomplete vcase, stick in an else clause.
            cases.append (['else', default0])
        result = ['vcase', datatype, vars[0]] + cases
        if default != ERROR:
            return self.fatbar (result, default)
        else:
            return result

    def record_rule (self, vars, rules, default):
        # (define thing
        #   {a=x b=2} -> x
        #   {a=3 b=y} -> y
        #   )
        # => 
        # (define (thing r)
        #   (match r.a r.b with
        #     x 2 -> x
        #     3 y -> y
        #     ))
        
        # XXX do sanity checks on record rules, sort, etc...
        def get_sig (pat):
            sig = [x[0] for x in pat.pairs]
            sig.sort()
            return sig

        # sanity check
        sig = get_sig (rules[0][0][0])
        for pats, code in rules[1:]:
            if get_sig (pats[0]) != sig:
                raise MatchError (pats, sig)
            
        # translate
        vars0 = ['%s_%s' % (vars[0], field) for field in sig]
        rules0 = []
        for pats, code in rules:
            assert (len (pats) == 1)
            pats0 = [ x[1] for x in pats[0].pairs ]
            rules0.append ((pats0, code))
        bindings = [ [vars0[i], '%s.%s' % (vars[0], sig[i])] for i in range (len (sig)) ]
        return ['let', bindings, self.match (vars0, rules0, default)]

    def constant_rule (self, vars, rules, default):
        # This is a simplified version of the constructor rule.  Here I'm departing from the book,
        #   which treats constants quite differently - they are translated into guard clauses.  I
        #   would like to avoid doing guard clauses until I'm convinced they're necessary.  And I
        #   just don't understand why constants should be treated differently from any other
        #   constructor.
        groups = []
        last = None
        for pats, code in rules:
            if pats[0] == last:
                groups[-1].append ((pats, code))
            else:
                groups.append ([(pats,code)])
                last = pats[0]
        while groups:
            group = groups.pop()
            rules0 = []
            for pats, code in group:
                rules0.append ((pats[1:], code))
            default = ['if', ['eq?', pats[0].value, vars[0]], self.match (vars[1:], rules0, default), default]
        return default
                
    def mixture_rule (self, vars, rules, default):
        # partition the rules into runs of either variables or constructors.
        parts = []
        part = []
        last = type(None)
        for pats, code in rules:
            if not is_a (pats[0], last):
                # start a new partition
                parts.append (part)
                part = [(pats, code)]
                last = pats[0].__class__
            else:
                part.append ((pats, code))
        parts.append (part)
        parts = parts[1:]
        while parts:
            part = parts.pop()
            default = self.match (vars, part, default)
        return default


