# -*- Mode: Python -*-

from lisp_reader import atom

import lisp_reader
import typing
import lambda_tree as tree

is_a = isinstance

# this file implements the scheme 'derived expression' transformations.
#   it converts 'high-level' expressions types into lower-level expressions
#   that are understood by the remaining stages of the compiler...

# this needs to be replaced by an automated mechanism.
# 1) it would solve the src/line-propagation problem, since
#    that capability would be built in.
# 2) allow the end user to extend the list.
# 3) should some core transforms be sacrosanct?  i.e., will
#    we break the compiler in some sense without them?

class transformer:

    def __init__ (self, safety):
        self.constants = {}
        self.safety = safety
        self.classes = []

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

    def get_constant_binding (self, exp):
        key = exp.kind, exp.value
        probe = self.constants.get (key, None)
        if probe:
            return probe
        else:
            # XXX we can do better than this!  how about <sym_xx_null_question>,
            #    or <string_xx_rubber_ducky>??
            var = self.gensym (prefix=exp.kind)
            self.constants[key] = var
            return var

    def add_constants_and_classes (self, exp):
        if exp[0] != 'fix':
            # if it's a really simple top-level expr, wrap it in a fix
            exp = ['fix', [], [], exp]
        names = exp[1]
        inits = exp[2]
        for (kind, value), varname in self.constants.iteritems():
            names.append (varname)
            if kind == 'string':
                inits.append (atom ('string', value))
            elif kind == 'symbol':
                inits.append (['string->symbol', atom ('string', value)])
            else:
                raise ValueError
        for c in self.classes:
            if is_a (c, pxll_class):
                for fun in c.methods:
                    names.append (fun[1][0])
                    inits.append (fun)
        return exp

    def go (self, exp):
        # XXX go back later and support single-expression programs if we care
        if len(exp) > 1:
            exp = self.expand_body (exp)
        else:
            exp = self.expand_exp (exp[0])
        if len(self.constants) or len(self.classes):
            exp = self.add_constants_and_classes (exp)
        return exp

    def expand_exp (self, exp):
        if is_a (exp, str):
            if '.' in exp:
                return self.expand_atom (exp)
            else:
                return exp
        elif is_a (exp, atom):
            if exp.kind == 'string':
                return self.get_constant_binding (exp)
            elif exp.kind == 'vector':
                return self.build_vector (exp)
            else:
                return exp
        elif is_a (exp, list):
            if len(exp) and is_a (exp[0], str):
                frobbed = self.frob_name (exp[0])
                name = 'expand_%s' % (frobbed)
                probe = getattr (self, name, None)
                if probe:
                    exp[0] = frobbed
                    return probe (exp)
                else:
                    return [self.expand_atom (exp[0])] + [self.expand_exp (x) for x in exp[1:]]
            else:
                return [self.expand_exp (x) for x in exp]
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
                result = ['get', result, parts[0]]
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
            # expand_atom ('x.y.z') => (get (get x y) z)
            # (set! x.y.z 3) => (set (get x y) z 3)
            ignore, loc, name = self.expand_atom (exp[1])
            return ['set', loc, name, self.expand_exp (exp[2])]
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
            return ['begin'] + self.expand_all (exp[1:])

    def expand_quote (self, exp):
        # literal data
        if exp[1] == []:
            # not sure where's the best place to do this...
            return ['quote', atom ('nil', 'nil')]
        else:
            return self.build_literal (exp[1])

    def expand_lambda (self, exp):
        return self.exp_function (None, exp[1], self.expand_body (exp[2:]))

    def expand_function (self, exp):
        return self.exp_function (exp[1], exp[2], self.expand_body (exp[3:]))

    def exp_function (self, name, formals, body):
        formals = self.process_formals (formals)
        if name and ':' in name:
            name, type = name.split (':')
        else:
            name, type = name, None
        return ['function', (name, type), formals, body]

    def process_formals (self, formals):
        # check for variable arity
        if '.' in formals:
            # for now, we don't support fixed args
            assert (formals[0] == '.')
            assert (len(formals) == 2)
            # flag this as a special arg...
            formals = ['$$' + formals[1]]
        result = []
        nary = False
        for i in range (len (formals)):
            formal = formals[i]
            if formal == '.':
                assert (len (formals) == i)
                nary = True
            else:
                if ':' in formal:
                    name, type = formal.split (':')
                else:
                    name, type = formal, None
                result.append ((name, type))
        return nary, result

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
            sym = self.gensym()
            return EE(
                ['let',
                 [[sym, exp[1]]],
                 ['if', sym, sym, ['or'] + self.expand_all (exp[2:])]
                 ]
                )

    # simplified 'boolean' or - doesn't bother to bind and return the first true value,
    #   this generates shorter/faster code when you don't *need* that value ...
    def expand_bor (self, exp):
        EE = self.expand_exp
        if len(exp) == 1:
            return ('bool', 'false')
        elif len(exp) == 2:
            return EE (exp[1])
        else:
            return EE (
                ['if', exp[1], atom ('bool', 'true'), ['bor'] + self.expand_all (exp[2:])]
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
                cond_clauses.append ([['%eq?', keysym, ['quote', keys[0]]]] + seq)
            else:
                or_clauses = self.expand_exp (['bor'] + [['eq?', keysym, ['quote', k]] for k in keys])
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
            funs = []
            for define in definitions:
                formals = define[1]
                body = define[2:]
                if is_a (formals, str):
                    # variable
                    funs.append ([formals, body[0]])
                else:
                    # function
                    name = formals[0]
                    funs.append ([name, ['function', name, formals[1:]] + body])

            return self.expand_letrec (['letrec', funs] + forms)

    # ----------- misc ---------------
    # this is to avoid treating the format string as a literal
    def expand__percent_percentcexp (self, exp):
        # (%%cexp <type> <format-string> arg0 arg1 ...)
        return ['%%cexp', exp[1], exp[2]] + self.expand_all (exp[3:])

    # this is to avoid treating the format string as a literal
    def expand__percent_percentmake_tuple (self, exp):
        # (%%make-tuple <typecode-string> arg0 arg1 ...)
        return ['%%make-tuple', exp[1]] + self.expand_all (exp[2:])
    
    # ----------- datatype ------------
    def expand_datatype (self, exp):
        # (datatype <name> (union (tag0 type0 type1 ...) (tag1 type0 type1 ...) ...))
        name = exp[1]
        defn = exp[2]
        # for now
        if defn[0] == 'union':
            # XXX a function in <typing> to add these?
            def maybe_product (x):
                if len(x) == 1:
                    return x[0]
                else:
                    return typing.product (x)
            typing.datatypes[name] = typing.union (
                name,
                [(x[0], maybe_product (x[1:])) for x in defn[1:]]
                )
        else:
            raise ValueError ("unknown datatype constructor")
        return ['begin']

    def expand_typecase (self, exp):
        # (typecase <exp>
        #    ((kind0 var0 var1) ...)
        #    ((kind1 var0 ...) ...)
        #    ...)
        variant = exp[1]
        # for now, only allow a varref for <exp>... later we'll automatically
        #    wrap this thing in a let if it's not.
        assert is_a (variant, str)
        alts = exp[2:]
        formals = [x[0] for x in alts]
        bodies = [self.expand_body (x[1:]) for x in alts]
        return ['typecase', variant, formals, bodies]

    # --------------------------------------------------------------------------------
    # literal expressions are almost like a sub-language

    def build_vector (self, exp):
        return self.expand_exp (['%%vector-literal'] + [self.expand_exp (x) for x in exp.value])

    def build_literal (self, exp):
        if is_a (exp, atom):
            if exp.kind == 'string':
                return self.get_constant_binding (exp)
            elif exp.kind == 'vector':
                return self.build_vector (exp)
            else:
                # char, int, bool
                return exp
        elif is_a (exp, list):
            if not len(exp):
                return atom ('nil', 'nil')
            elif len(exp) == 3 and exp[1] == '.':
                return ['cons', self.build_literal (exp[0]), self.build_literal (exp[2])]
            elif exp[0] == 'comma':
                # lame attempt at backquote
                return self.expand_exp (exp[1])
            else:
                return ['cons', self.build_literal (exp[0]), self.build_literal (exp[1:])]
        elif is_a (exp, str):
            return self.get_constant_binding (atom ('symbol', exp))
        else:
            raise SyntaxError (exp)
    
    # --------------------------------------------------------------------------------
    # classes
    # --------------------------------------------------------------------------------        

    def expand_class (self, exp):
        # (class name (fields)
        #    (method0 ...)
        #    (method1 ...)
        #   )
        # a string
        cname = exp[1]
        # a list of optionally-typed names
        fields = []
        for field in exp[2]:
            if ':' in field:
                fname, type = field.split (':')
            else:
                fname, type = field, None
            fields.append ((fname, type))
        # method definitions
        methods = exp[3:]
        # no methods for now.
        assert (methods == [])
        c = pxll_class (cname, fields, methods)
        typing.classes[cname] = c
        self.classes.append (c)
        # this will get optimized away to nothing...
        return ['begin']

class pxll_class:

    def __init__ (self, name, fields, methods=[]):
        self.name = name
        self.fields = fields
        self.methods = methods

    def lookup_method (self, meth_name):
        for fun in self.methods:
            probe = '%s-%s' % (self.name, meth_name)
            if fun[1][0] == probe:
                return probe
        return None

    def gen_constructor (self):
        # build a constructor as a node tree - before analyse is called.
        formals = []
        args = []
        for i in range (len (self.fields)):
            fname, ftype = self.fields[i]
            formals.append (tree.vardef (fname, ftype))
            args.append (tree.varref (fname))
        body = tree.make_tuple (self.name, 'userobj', args)
        return tree.function (self.name, formals, body)

    def get_field_offset (self, name):
        for i in range (len (self.fields)):
            fname, type = self.fields[i]
            if name == fname:
                return i
        raise ValueError ("unknown field name?")
