# -*- Mode: Python -*-

from lisp_reader import atom
from pprint import pprint as pp

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
        # XXX need to create constructors for all pvariants in case we need
        #     to use one in a higher-level function.
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
                return self.build_symbol (exp)
            elif exp.kind == 'vector':
                return self.build_vector (exp)
            elif exp.kind == 'record':
                return self.build_record (exp)
            elif exp.kind == 'bool':
                return self.build_bool (exp)
            else:
                return exp
        elif is_a (exp, list):
            if len (exp) == 0:
                return []
            else:
                rator = self.expand_exp (exp[0])
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
        # XXX should be done in the reader, just like ':'
        if not [ch for ch in exp if ch != '.']:
            # don't mangle symbols containing just dots like '...
            return exp
        elif '.' in exp:
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

    # XXX could this be handled with a macro?
    def expand_if (self, exp):
        # (if <test> <then-clause> [<else-clause>])
        EE = self.expand_exp
        if len(exp) == 3:
            # an if with no else clause is almost always done for side-effect,
            #  and thus should default to type <undefined>.
            return ['if', EE (exp[1]), EE (exp[2]), atom ('undefined', 'undefined')]
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

    def expand_colon (self, exp):
        colon, datatype, alt = exp
        if is_a (alt, str):
            if datatype:
                # refer to the global constructor function,
                # usually inlined if in the rator position.
                return '%s:%s' % (datatype, alt)
            else:
                # can't have one single constructor for variants, because
                #  the number of args could vary.  Instead, turn it into
                #  the name of the primapp.  [this will fail variable lookup
                #  if you refer to it outside of the rator position]
                return '%%vcon/%s' % (alt,)
        elif is_a (alt, list) and alt[0] == 'colon':
            # method invocation syntax
            ignore, ignore, method = alt
            assert (is_a (method, str))
            return '%%method/%s/%s' % (datatype, method)
        else:
            raise SyntaxError (exp)

    # ----------- special forms ----------------

    def expand_let_splat (self, exp):
        vars = [ (x[0], self.expand_exp (x[1])) for x in exp[1] ]
        return (['let_splat', vars, self.expand_body (exp[2:])])

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
        if not body:
            raise ValueError ("empty <letrec> body", exp)
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

    def expand__percentmethod (self, exp):
        # (x::add 10) => (x.o.add x 10)
        # require that <x> is a varref, to avoid code duplication.
        method = exp[0]
        ignore, ob, mname = method.split ('/')
        assert (is_a (ob, str))
        return self.expand_exp (['%s.o.%s' % (ob, mname), ob] + exp[1:])

    # ok, let's make two kinds of vcase.  the polymorphic variant,
    #  and the normal variant.  we can distinguish between the two
    #  by the presence of the datatype name.

    def expand_vcase (self, exp):
        if is_a (exp[2], str):
            # normal variant
            return self.exp_nvcase (exp)
        else:
            return self.exp_pvcase (exp)

    # nvcase/pvcase: the problem is that they're done two different ways.
    # 1. I'd like to get rid of 'magic names', e.g "vcase/name1/0" by switching
    #    to primapp-with-params.
    # 2. I'd like to stop pvcase going through a 'lambda' phase and back
    #    just to make typing 'easier' when the original syntax already has
    #    to be typed anyway.
    # 3. nvcase stays relatively sane, but unfort goes through a translation
    #    to a let binding that needs to emit %nvget/name/index primapps.  I
    #    want those to become lambdas and introduce %nvget as late as possible.
    #    [the question here is how to handle don't-cares?]
    # 4. By moving as much as possible into lib/derived.scm, I simplify the
    #    compiler, and simplify that translation into Irken.

    # read -> transform -> node -> type -> analyze -> type -> cps -> backend
    # so at which point will %nvget be called?  if we can put it off until cps
    # that would be the cleanest... but then we need to teach all the rest of
    # the compiler about it, sigh.  or do we?  Maybe leave it to the backend?

    def exp_nvcase (self, exp):
        # (nvcase type x 
        #    ((<select0> <formal0> <formal1> ...) <body0>)
        #    ((<select1> <formal0> <formal1> ...) <body1>)
        #    (else <body2>)
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
                alts0.append ((label, self.expand_exp (['let', binds] + body)))
            else:
                alts0.append ((label, self.expand_exp (['begin'] + body)))
        if len(alts) != len (dt.alts) and not else_clause:
            #raise ValueError ("variant case does not have correct number of alternatives")
            # XXX maybe distinguish between vcase from the match compiler vs user-provided ones?
            print "variant case does not have correct number of alternatives"
        if not else_clause:
            else_clause = ['%%match-error']
        return ['%nvcase', datatype, self.expand_exp (val), alts0, self.expand_exp (else_clause)]

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
                r = ['lambda', ['vval'], ['&vcase', [label, len(formals)], s, r, 'vval']]
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
        tvars = {}
        if len(exp) == 3 and not is_a (exp[2], list):
            # (datatype <name> <type>)
            # a datatype alias
            self.context.datatypes[name] = nodes.parse_type (exp[2], tvars)
        else:
            subs = exp[2:]
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

    def build_bool (self, exp):
        if exp.value == 'true':
            return self.expand_exp (['%dtcon/bool/true'])
        else:
            return self.expand_exp (['%dtcon/bool/false'])

    def build_symbol (self, exp):
        return self.expand_exp (
            ['literal',
             ['%dtcon/symbol/t', atom ('string', exp.value)]]
            )

    # walk a literal, making sure it can be represented as a constructed value.

    # XXX I need to think clearly about literals, QUOTE, and LITERAL.  This code
    #   is a mess and doesn't know exactly what it's doing.

    def build_literal (self, exp, as_list=False, backquote=False):
        
        # urgh, can't assign to a lexical variable
        runtime_only = [False]

        def build (exp):
            if is_a (exp, atom):
                if exp.kind in ('bool', 'symbol'):
                    print 'fixme'
                    trace()
                elif itypes.base_types.has_key (exp.kind):
                    return exp
                else:
                    runtime_only[0] = True
                    return exp
            elif is_a (exp, list):
                if len(exp):
                    if is_a (exp[0], str):
                        name = exp[0].split ('/')
                        if name[0].count (':') == 1:
                            # constructor, inline it here so the back end can see it correctly
                            dt, alt = name[0].split (':')
                            if dt:
                                name = '%%dtcon/%s/%s' % (dt, alt)
                            else:
                                name = '%%vcon/%s' % (alt,)
                            return [name] + [build(x) for x in exp[1:]]
                        elif name[0] in ('%vector-literal', '%dtcon'):
                            return [exp[0]] + [build (x) for x in exp[1:]]
                        elif exp[0] == 'comma' and backquote:
                            runtime_only[0] = True
                            return exp[1]
                        elif exp[0] == 'constructed':
                            # redundant
                            return exp[1]
                        elif as_list:
                            return ['%dtcon/list/cons', build (exp[0]), build (exp[1:])]
                        else:
                            raise ValueError ("I'm so confused!")
                    elif as_list:
                        return ['%dtcon/list/cons', build (exp[0]), build (exp[1:])]
                    else:
                        return [build(x) for x in exp[1:]]
                else:
                    return ['%dtcon/list/nil']
            elif is_a (exp, str) and as_list:
                # in a list literal, this is a symbol
                return ['%dtcon/symbol/t', atom ('string', exp)]
                #return atom ('symbol', exp)
            else:
                runtime_only[0] = True
                return exp

        result = build (self.expand_exp (exp))
        if runtime_only[0]:
            return result
        elif len(result) == 1:
            # in other words, a no-argument constructor... we're better off
            # just leaving it as a primapp.
            assert result[0].startswith ('%dtcon/')
            return result
        else:
            return ['constructed', result]
                
    
