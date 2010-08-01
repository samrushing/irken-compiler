# -*- Mode: Python -*-

from lisp_reader import atom

import itypes
import lisp_reader
import match
import nodes

is_a = isinstance

from pdb import set_trace as trace

# this file implements the scheme 'derived expression' transformations.
#   it converts 'high-level' expressions into lower-level expressions
#   that are understood by the remaining stages of the compiler...

# these transformations are performed 'textually' - think of them as macros.
#   later transformations (in analyze.py) are performed on a node tree, after
#   alpha renaming.

class RuntimeLiteral (Exception):
    pass

# this needs to be replaced by an automated mechanism.
# 1) it would solve the src/line-propagation problem, since
#    that capability would be built in.
# 2) allow the end user to extend the list.
# 3) should some core transforms be sacrosanct?  i.e., will
#    we break the compiler in some sense without them?
#
# XXX Note: now that we have a pattern-matching 'defmacro',
#  most of the derived-expression transforms in here can be
#  moved to lib/derived.scm.

class transformer:

    def __init__ (self, context):
        self.constants = {}
        self.context = context
        self.constructors = []
        self.match = match.compiler (context)
        self.macros = {}

    gensym_counter = 0

    def gensym (self, prefix='g'):
        "generate a fresh unused symbol"
        self.gensym_counter += 1
        return '%s%d' % (prefix, self.gensym_counter,)

    safe_name_map = {'!':'_bang','*':'_splat','?':'_question','+':'_plus','-':'_', '%':'_percent'}

    def frob_name (self, name):
        l = []
        for ch in name.lower():
            if self.safe_name_map.has_key (ch):
                r = self.safe_name_map[ch]
            elif ch in 'abcdefghijklmnopqrstuvwxyz_0123456789':
                r = ch
            else:
                r = '_%02x' % (ord (ch),)
            l.append (r)
        r = ''.join (l)
        if r == '_':
            # special case
            r = 'minus'
        return r

    def add_constructors (self, exp):
        if exp[0] != 'fix':
            # if it's a really simple top-level expr, wrap it in a fix
            exp = ['fix', [], [], exp]
        names = exp[1]
        inits = exp[2]
        for name, init in self.constructors:
            names.insert (0, name)
            inits.insert (0, init)
        return exp

    def go (self, exp):
        if len(exp) > 1:
            exp = self.find_declarations (exp)
            exp = self.expand_body (exp)
        else:
            exp = self.expand_exp (exp[0])
        if len(self.constructors):
            exp = self.add_constructors (exp)
        return exp

    def find_declarations (self, exp):
        r = []
        for x in exp:
            if is_a (x, list) and len(x):
                if x[0] == 'datatype':
                    self.parse_datatype (x)
                elif x[0] == 'defmacro':
                    self.parse_defmacro (x)
                else:
                    r.append (x)
            else:
                r.append (x)
        return r

    def expand_exp (self, exp):
        if is_a (exp, str):
            if '.' in exp:
                return self.expand_atom (exp)
            else:
                return exp
        elif is_a (exp, atom):
            if exp.kind == 'string':
                return exp
            elif exp.kind == 'symbol':
                return exp
            elif exp.kind == 'vector':
                return self.build_vector (exp)
            elif exp.kind == 'record':
                return self.build_record (exp)
            else:
                return exp
        elif is_a (exp, list):
            if len(exp):
                rator = self.expand_exp (exp[0])
                if is_a (rator, list) and len(rator) == 3 and rator[0] == 'colon':
                    if rator[1] is None:
                        # polymorphic variant constructor
                        rator = '%%vcon/%s' % (rator[2],)
                    else:
                        # datatype constructor
                        rator = '%%dtcon/%s/%s' % (rator[1], rator[2])
                if is_a (rator, str):
                    if rator.startswith ('%') and '/' in rator:
                        # handle names with encoded meta-parameters like %vcon
                        selector = rator.split('/')[0]
                    else:
                        selector = rator
                    probe = self.macros.get (selector, None)
                    # user-defined macros first
                    if probe:
                        # macroexpansion is 'dumb' (i.e., pure textual substitution),
                        #  so we must feed the result back through again.
                        return self.expand_exp (probe.apply (exp))
                    else:
                        # python-defined macros
                        name = 'expand_%s' % (self.frob_name (selector))
                        probe = getattr (self, name, None)
                        if probe:
                            exp[0] = rator
                            return probe (exp)
                        else:
                            return self.cheat_check ([rator] + [self.expand_exp (x) for x in exp[1:]])
                else:
                    return [self.expand_exp (x) for x in exp]
            else:
                return [self.expand_exp (x) for x in exp]
        else:
            return exp

    ZERO = lisp_reader.atom ('int', 0)
    def cheat_check (self, exp):
        if exp[0] == '>':
            if exp[2] == self.ZERO:
                return ['>0', exp[1]]
            else:
                return exp
        elif exp[0] == '<':
            if exp[2] == self.ZERO:
                return ['<0', exp[1]]
            else:
                return exp
        elif exp[0] == '=':
            if exp[2] == self.ZERO:
                return ['zero?', exp[1]]
            else:
                return exp
        else:
            return exp

    def expand_atom (self, exp):
        # a.b => (get a b)
        # a.b.c => (get (get a b) c)
        if '.' in exp:
            parts = exp.split ('.')
            result = parts[0]
            while len(parts) > 1:
                parts.pop (0)
                #result = ['get', result, parts[0]]
                result = ['%%raccess/%s' % parts[0], result]
            return result
        else:
            return exp

    # XXX should probably rename expand_exp and expand_all to avoid unexpected weirdness
    def expand_all (self, exps):
        return [self.expand_exp (x) for x in exps]

    # ----------- core forms ----------------

    def expand_if (self, exp):
        # (if <test> <then-clause> [<else-clause>])
        EE = self.expand_exp
        if len(exp) == 3:
            return ['if', EE (exp[1]), EE (exp[2]), atom ('bool', 'false')]
        else:
            return ['if', EE (exp[1]), EE (exp[2]), EE (exp[3])]

    def expand_set_bang (self, exp):
        if is_a (exp[1], str) and '.' in exp[1]:
            raccess, base = self.expand_atom (exp[1])
            assert (raccess.startswith ('%raccess/'))
            ignore, label = raccess.split ('/')
            return ['%%rset/%s' % (label,), base, self.expand_exp (exp[2])]
        elif is_a (exp[1], list) and exp[1][0] == '%%array-ref':
            [ignore, base, index] = exp[1]
            value = exp[2]
            return ['%%array-set', self.expand_exp (base), self.expand_exp (index), self.expand_exp (value)]
        else:
            return ['set_bang', exp[1], self.expand_exp (exp[2])]

    def expand_begin (self, exp):
        # sequence
        if len(exp) == 2:
            return self.expand_exp (exp[1])
        else:
            exps = []
            for exp in self.expand_all (exp[1:]):
                if is_a (exp, list) and len(exp) and exp[0] == 'begin':
                    # merge with this set
                    exps.extend (exp[1:])
                else:
                    exps.append (exp)
            return ['begin'] + exps

    def expand_quote (self, exp):
        # literal data
        return self.build_literal (exp[1], as_list=True)

    def expand_literal (self, exp):
        # here's the dilemma: QUOTE is normally used for this purpose, but if I want
        #  to allow literals involving constructors (other than lists), I need a syntax
        #  for it.  I *could* examine the operator position of a list for a constructor,
        #  but that would make it impossible to build a list with that kind of symbol
        #  at the front.  For now, punt and allow LITERAL to act as a new kind of QUOTE.
        return self.build_literal (exp[1])

    def expand_backquote (self, exp):
        # literal data
        return self.build_literal (exp[1], as_list=True, backquote=True)

    def expand__percentvcon (self, exp):
        # add some metadata about the arity of this constructor
        if exp[0].count ('/') < 2:
            nargs = len(exp) - 1
            return ['%s/%d' % (exp[0], nargs)] + [self.expand_exp (x) for x in exp[1:]]
        else:
            return [exp[0]] + [self.expand_exp (x) for x in exp[1:]]

    def expand__percentvcase (self, exp):
        # in case anyone wants to use the raw prim rather than 'vcase'
        assert (exp[1][0] == 'lambda')
        if exp[0].count ('/') < 2:
            nargs = len (exp[1][1])
            return ['%s/%d' % (exp[0], nargs)] + [self.expand_exp (x) for x in exp[1:]]
        else:
            return [exp[0]] + [self.expand_exp (x) for x in exp[1:]]

    def expand_lambda (self, exp):
        return self.exp_function (None, exp[1], self.expand_body (exp[2:]))

    def expand_function (self, exp):
        return self.exp_function (exp[1], exp[2], self.expand_body (exp[3:]))

    def exp_function (self, name, formals, body):
        formals = self.process_formals (formals)
        return ['function', (name, None), formals, body]

    def process_formals (self, formals):
        # check for variable arity
        if '.' in formals:
            raise ValueError ("variable arity not supported")
        return formals

    # ----------- special forms ----------------

    def expand_and (self, exp):
        EE = self.expand_exp
        if len(exp) == 1:
            return atom ('bool', 'true')
        elif len(exp) == 2:
            return EE (exp[1])
        else:
            return EE (['if', exp[1], ['and'] + exp[2:]])

    def expand_or (self, exp):
        EE = self.expand_exp
        if len(exp) == 1:
            return ('bool', 'false')
        elif len(exp) == 2:
            return EE (exp[1])
        else:
            return EE (
                ['if', exp[1], atom ('bool', 'true'), ['or'] + self.expand_all (exp[2:])]
                )

    def expand_cond (self, exp):
        EE = self.expand_exp
        if exp == ['cond']:
            return atom ('bool', 'false')
        elif exp[1][0] == 'else':
            # (cond (else ...))
            return EE (['begin'] + exp[1][1:])
        elif exp[1][1] == '=>':
            # (cond (test => result) ...)
            sym = self.gensym()
            return EE (['let',
                        [[sym, exp[1][0]]],
                        ['if', sym,
                         [exp[1][2], sym],
                         ['cond'] + exp[2:]]])
        elif len(exp) == 2 and len(exp[1]) == 1:
            # (cond (test)) => test
            return EE(exp[1][0])
        elif len(exp[1]) == 1:
            # (cond (test) ...)
            sym = self.gensym()
            return EE (['let',
                        [[sym, exp[1][0]]],
                        ['if', sym,
                         sym,
                         ['cond'] + exp[2:]]])
        elif len(exp[1]) > 1 and len(exp) == 2:
            # (cond (test result1 result2))
            return EE (['if', exp[1][0], ['begin'] + exp[1][1:]])
        else:
            # (cond (test result1 result2) ...)
            return EE (['if', exp[1][0], ['begin'] + exp[1][1:], ['cond'] + exp[2:]])

    def expand_case (self, exp):
        # similar to a scheme case - rather than using memv it inlines calls to eq? (NOT eqv?)
        #   this is safe to use with ints, symbols, as long as the lists are short...
        clauses = exp[2:]
        keysym = self.gensym ('case')
        # (case <key>
        #    ((x0 x1 x2 ...) <sequence0>)
        #    ((y0 y1 ...) <sequence1>)
        #    (else <sequence>))
        # => (let ((keysym <key>))
        #       (cond ((or (eq? keysym x0) ...) <sequence0>)
        #             ((or (eq? keysym y0) ...) <sequence1>)))
        cond_clauses = []
        for clause in clauses:
            keys = clause[0]
            seq  = clause[1:]
            if keys == 'else':
                # XXX assert this is the last clause...
                cond_clauses.append (['else'] + seq)
            elif len(keys) == 1:
                cond_clauses.append ([['eq?', keysym, ['quote', keys[0]]]] + seq)
            else:
                or_clauses = ['or'] + [['eq?', keysym, ['quote', k]] for k in keys]
                cond_clauses.append ([or_clauses] + seq)
        return self.expand_exp (['let', [[keysym, exp[1]]], ['cond'] + cond_clauses])

    def expand_let_splat (self, exp):
        vars = [ (x[0], self.expand_exp (x[1])) for x in exp[1] ]
        return (['let_splat', vars, self.expand_body (exp[2:])])

    opt_strict_letrec = False

    def expand_let (self, exp):
        EE = self.expand_exp
        EA = self.expand_all
        if is_a (exp[1], list):
            # normal let
            vars = exp[1]
            names = [x[0] for x in vars]
            vals  = [x[1] for x in vars]
            body = exp[2:]
            #return EE (
            #    [['function', None, names] + body] + vals
            #    )
            # replace with let*, our new core binding construct
            return EE (['let_splat'] + exp[1:])

        elif is_a (exp[1], str):
            # named let
            tag  = exp[1]
            vars = exp[2]
            names = [x[0] for x in vars]
            vals  = [x[1] for x in vars]
            body = exp[3:]
            # although this matches the one given in R5RS, I don't understand why the (tag val0 val1 ...)
            #   call is *outside* the letrec?  Why return the function and then apply it, why not make
            #   that the body?
            # Answer: this is covered in the 'pitfalls' tests.
            # See http://groups.google.com/group/comp.lang.scheme/msg/3e2d267c8f0ef180?pli=1
            # However, I'm going to leave it like this for now because This Is Not Scheme,
            #   and it looks like it might have a pretty big performance impact...
            if self.opt_strict_letrec:
                return EE ([['letrec', [[tag, ['function', tag, names] + body]], tag]] + vals)
            else:
                return EE (['letrec', [[tag, ['function', tag, names] + body]], [tag] + vals])
        else:
            raise SyntaxError ("malformed let", exp)

    # original non-fix version
    def expand_xletrec (self, exp):
        decls = exp[1]
        body = exp[2:]
        undefined_marker = atom ('undefined', 'undefined')
        empty_decls = [ [x[0], undefined_marker] for x in decls ]
        assigns = [ ['set!', x[0], x[1]] for x in decls ]
        return self.expand_exp (
            ['let', empty_decls] + assigns + body
            )

    def expand_letrec (self, exp):
        # (letrec ((f1 (lambda (...)))
        #          (f2 (lambda (...))))
        #   <body>)
        decls = exp[1]
        # sort decls so functions are first
        decls0 = []
        decls1 = []
        for x in decls:
            if is_a (x[1], list) and x[1][0] == 'function':
                decls0.append (x)
            else:
                decls1.append (x)
        decls = decls0 + decls1
        names = [x[0] for x in decls]
        inits  = [x[1] for x in decls]
        body = exp[2:]
        return ['fix', names, self.expand_all (inits), self.expand_body (body)]

    # ----------- lambda bodies ----------------

    def collect_matching_forms (self, exps, sym):
        matching = []
        others   = []
        for exp in exps:
            if is_a (exp, list) and exp[0] == sym:
                matching.append (exp)
            else:
                others.append (exp)
        return matching, others

    def expand_body (self, exp):
        definitions, forms = self.collect_matching_forms (exp, 'define')
        if not definitions:
            return self.expand_begin (['begin'] + forms)
        else:
            funs = [self.exp_define (d) for d in definitions]
            return self.expand_letrec (['letrec', funs] + forms)

    def exp_define (self, exp):
        formals = exp[1]
        body = exp[2:]
        if is_a (formals, str):
            if '->' in body:
                # (define <name> <patterns...>)
                name = exp[1]
                vars, code = self.exp_match (exp[2:])
                return [name, ['function', name, vars, code]]
            else:
                return [formals, body[0]]
        else:
            name = formals[0]
            return [name, ['function', name, formals[1:]] + body]
    
    def exp_match (self, exp, vars=None):
        # (<p00> <p01> ... -> <r0> <p10> <p11> ...)
        rules = []
        patterns = []
        i = 0
        while i < len (exp):
            if exp[i] == '->':
                i += 1
                rules.append ((patterns, exp[i]))
                patterns = []
                i += 1
            else:
                patterns.append (exp[i])
                i += 1
        assert (not exp[i:])
        if vars is None:
            vars = [self.match.gensym() for x in range (len (rules[0][0]))]
        return self.match.compile (rules, vars)

    # ----------- misc ---------------
    # this is to avoid treating the format string as a literal
    def expand__percent_percentcexp (self, exp):
        # (%%cexp <type> <format-string> arg0 arg1 ...)
        return ['%%cexp', exp[1], exp[2]] + self.expand_all (exp[3:])

    # ok, let's make two kinds of vcase.  the polymorphic variant,
    #  and the normal variant.  we can distinguish between the two
    #  by the presence of the datatype name.

    def expand_vcase (self, exp):
        if is_a (exp[2], str):
            # normal variant
            return self.exp_nvcase (exp)
        else:
            return self.exp_pvcase (exp)

    def exp_nvcase (self, exp):
        # (nvcase type x 
        #    ((<select0> <formal0> <formal1> ...) <body0>)
        #    ((<select1> <formal0> <formal1> ...) <body1>)
        #    ...)
        # =>
        # (nvcase type x
        #    ((let ((f0 x.0) (f1 x.1) (f2 x.2)) <body0>) ...))
        #
        datatype = exp[1]
        dt = self.context.datatypes[datatype]
        val = exp[2]
        # for now, only allow a varref for <exp>... later we'll automatically
        #    wrap this thing in a let if it's not.
        assert (is_a (val, str))
        alts = exp[3:]
        alt_formals = [ (x[0][0], x[0][1:]) for x in alts ]
        alts0 = []
        else_clause = None
        for i in range (len (alts)):
            binds = []
            label, formals = alt_formals[i]
            body = alts[i][1:]
            if (label, formals) == ('e', 'lse'): # sue me.
                label = 'else'
                else_clause = ['begin'] + body
                continue
            else:
                # let's stick with the colon syntax for now, it highlights nicely
                assert (is_a (label, list) and len(label) == 3 and label[0] == 'colon' and label[1] is None)
                label = label[2]
            for j in range (len (formals)):
                formal = formals[j]
                if formal != '_':
                    binds.append ([formal, ['%%nvget/%s/%s/%d' % (datatype, label, j), val]])
            if len(binds):
                names = [x[0] for x in binds]
                inits = [x[1] for x in binds]
                #alts0.append ((label, self.expand_exp ([['lambda', names] + body] + inits)))
                alts0.append ((label, self.expand_exp (['let', binds] + body)))
            else:
                alts0.append ((label, self.expand_exp (['begin'] + body)))
        if len(alts) != len (dt.alts) and not else_clause:
            #raise ValueError ("variant case does not have correct number of alternatives")
            # XXX maybe distinguish between vcase from the match compiler vs user-provided ones?
            print "variant case does not have correct number of alternatives"
        if not else_clause:
            else_clause = ['%%match-error']
        return ['nvcase', datatype, self.expand_exp (val), alts0, self.expand_exp (else_clause)]

    def exp_pvcase (self, exp):
        # (vcase <exp>
        #    ((:kind0 var0 var1) <body0>)
        #    ((:kind1 var0) <body1>)
        #    (else <body>))
        val = exp[1]
        # for now, only allow a varref for <exp>... later we'll automatically
        #    wrap this thing in a let if it's not.
        assert is_a (val, str)
        alts = exp[2:]
        r = None
        while alts:
            alt = alts.pop()
            selector = alt[0]
            body = alt[1:]
            if selector == 'else':
                # override %vfail
                r = ['lambda', ['velse']] + body
            else:
                [colon, ignore, label] = selector[0]
                formals = selector[1:]
                s = ['lambda', formals] + body
                if r is None:
                    # no else clause
                    r = ['lambda', ['vfail'], ['%vfail', 'vfail']]
                r = ['lambda', ['vval'], ['%%vcase/%s/%d' % (label, len(formals)), s, r, 'vval']]
        # discard the outermost lambda binding, use <val> instead
        r = r[2]
        r[-1] = val
        return self.expand_exp (r)

    def expand_match (self, exp):
        # (match <exp0> <exp1> ... with
        #      <pat00> <pat01> ... -> <result0>
        #      <pat10> <pat11> ... -> <result1>)
        vars = []
        inits = []
        i = 1
        while exp[i] != 'with':
            if is_a (exp[i], str):
                # i.e., a varref... don't bother generating a new variable to hold its value,
                #   simply pass it on down into the match compiler as-is. XXX worry about capture.
                vars.append (exp[i])
            else:
                v = self.match.gensym()
                vars.append (v)
                inits.append ([v, exp[i]])
            i += 1
        vars, code = self.exp_match (exp[i+1:], vars)
        if len (inits):
            return self.expand_exp (['let', inits, code])
        else:
            return self.expand_exp (code)

    def expand_cinclude (self, exp):
        self.context.cincludes.add (exp[1].value)
        return ['begin']

    # ----------- datatype ------------

    def parse_datatype (self, exp):
        # XXX since these are global, should we mandate that they appear at the top level?
        #      or might someone want to hide the constructors?
        # defines a variant datatype
        # we don't need union vs product here, since <product> datatypes should use records instead.
        # (datatype <name> (tag0 type0 type1 ...) (tag1 type0 type1 ...) ...)
        name = exp[1]
        subs = exp[2:]
        tvars = {}
        alts = []
        for sub in subs:
            tag = sub[0]
            assert (is_a (tag, list) and len(tag) == 3 and tag[0] == 'colon' and tag[1] is None)
            tag = tag[2]
            prod = [ nodes.parse_type (x, tvars) for x in sub[1:] ]
            alts.append ((tag, prod))
            args = ['arg%d' % x for x in range (len (prod))]
            # build a convenient constructor function
            self.constructors.append ((
                    ('%s:%s' % (name, tag)),
                    self.expand_exp (['lambda', args, ['%%dtcon/%s/%s' % (name, tag)] + args])
                    ))
        self.context.datatypes[name] = itypes.datatype (self.context, name, alts, tvars)

    # ----------- user macros ---------------

    def parse_defmacro (self, exp):
        # (defmacro and
        #   (and)                 -> #t
        #   (and test)            -> test
        #   (and test1 test2 ...) -> (if test1 (and test2 ...) #f)
        #   )
        import mbe
        name = exp[1]
        pats = []
        i = 2
        while i < len(exp):
            in_pat, arrow, out_pat = exp[i:i+3]
            assert (arrow == '->')
            pats.append ((in_pat, out_pat))
            i += 3
        self.macros[name] = mbe.macro (name, pats)
        if self.context.verbose:
            print "macro: %s" % (name,)

    # --------------------------------------------------------------------------------
    # literal expressions are almost like a sub-language

    def build_vector (self, exp):
        return self.expand_exp (['%%vector-literal/%d' % len(exp.value)] + exp.value)

    def build_record (self, exp):
        # convert a record literal into a set of record primapps
        r = ['%rmake']
        for name, val in exp.value:
            r = ['%%rextend/%s' % name, r, val]
        return self.expand_exp (r)

    # walk a literal, making sure it can be represented as a constructed value.

    # XXX I need to think clearly about literals, QUOTE, and LITERAL.  This code
    #   is a mess and doesn't know exactly what it's doing.

    def build_literal (self, exp, as_list=False, backquote=False):
        # urgh, can't assign to a lexical variable
        runtime_only = [False]

        def build (exp):
            if is_a (exp, atom):
                if exp.kind in ('int', 'char', 'bool', 'undefined'):
                    # XXX itypes should have a list of immediate base types
                    return exp
                elif exp.kind in ('symbol', 'string'):
                    return exp
                else:
                    runtime_only[0] = True
                    return exp
            elif is_a (exp, list):
                if len(exp):
                    if is_a (exp[0], str):
                        name = exp[0].split ('/')
                        if name[0] in ('%vector-literal', '%dtcon'):
                            return [exp[0]] + [build (x) for x in exp[1:]]
                        elif exp[0] == 'comma' and backquote:
                            runtime_only[0] = True
                            return exp[1]
                        elif exp[0] == 'constructed':
                            # redundant
                            return exp[1]
                        else:
                            return ['%dtcon/list/cons', build (exp[0]), build (exp[1:])]
                    elif as_list:
                        return ['%dtcon/list/cons', build (exp[0]), build (exp[1:])]
                    else:
                        return [build(x) for x in exp[1:]]
                else:
                    return ['%dtcon/list/nil']
            elif is_a (exp, str) and as_list:
                # in a list literal, this is a symbol
                return atom ('symbol', exp)
            else:
                runtime_only[0] = True
                return exp

        result = build (self.expand_exp (exp))
        if runtime_only[0]:
            return result
        else:
            return ['constructed', result]
                
    
