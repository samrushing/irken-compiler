
# 
# ********* this is dead code **********
#

# this code used to be part of an ancient progenitor of the <analyzer>
# class now in analyze.py eventually I'll resurrect this.  This is a
# very straightforward version of the hindley-milner 'equation'
# algorithm, which actually doesn't work for this application because
# it doesn't allow for 'let polymorphism'.  it needs to be rewritten
# in the style of EOPLv3, as a recursive descent.  That will allow for
# separate instantiations of the same type.

types = ['int', 'bool', 'char', 'string', 'closure', 'vector', 'cont', 'lenv']
# function type = (<result_type>, <arg_types>)

class analyzer:
    # [...]

    def make_type_equations (self, exp):

        eqs = []

        for n in exp:

            def add_eq (var, val):
                if is_a (var, str) and is_a (val, str) and var != val:
                    raise TypeError (n, var, val)
                else:
                    eqs.append ((n, var, val))

            if is_a (n, tree.primapp):
                form, sig = primop_types[n.name]
                out_type, arg_types = sig
                nargs = len (arg_types)
                add_eq (n, out_type)
                for i in range (nargs):
                    add_eq (n.args[i], arg_types[i])
            elif is_a (n, tree.sequence):
                add_eq (n, n.exprs[-1])
            elif is_a (n, tree.conditional):
                add_eq (n.test_exp, 'bool')
                add_eq (n, n.then_exp)
                add_eq (n, n.else_exp)
            elif is_a (n, tree.function):
                add_eq (n, (n.body, tuple(n.formals)))
            elif is_a (n, tree.application):
                if n.letrec_hack:
                    # rather than insert a conflicting equation for the
                    #  argument describing it as 'undefined', let's claim
                    #  that the type of the expression is the type of the
                    #  letrec body.
                    assert (is_a (n.rator, tree.function))
                    add_eq (n, n.rator.body)
                else:
                    add_eq (n.rator, (n, tuple(n.rands)))
            
        def deref (t):
            "dereference all var refs"
            if is_a (t, tree.varref):
                return t.var
            if is_a (t, tree.literal):
                return t.type
            elif is_a (t, tuple):
                return tuple ([deref (x) for x in t])
            else:
                return t

        return [ (x[0], deref (x[1])) for x in eqs ]

            
    def print_equations (self, eqs):
        W = sys.stdout.write
        for node, var, val in eqs:
            print_type (var),
            W (' = ')
            print_type (val)
            W ('\n')

    def extend_subst (self, eqs, substs, var, val, exp):

        print 'extend_subst %r' % ((var, val),)

        def apply1 (val):
            if type(val) is str:
                # plain type (e.g., 'int', 'bool')
                return val
            elif is_a (val, tree.node):
                # a type variable
                return substs.get (val, val)
            elif type(val) is tuple:
                # a function
                out_type, arg_types = val
                return (apply1 (out_type), tuple ([ apply1 (x) for x in arg_types ]))

        # is <var> already in <substs>?
        if substs.has_key (var):
            var = apply1 (var)
        # substitute in <val>
        #print 'apply1: '
        #print '  before: ', val
        val = apply1 (val)
        #print '  after:  ', val

        if var == val:
            # 0: it's a trivial identity, ignore
            return
        elif is_a (var, tree.node):
            # 1: <var> is still a variable, continue on
            pass
        elif is_a (val, tree.node):
            # 2: <val> is a variable, swap and continue
            var, val = val, var
        elif is_a (var, tuple) and is_a (val, tuple):
            # 3: both are function types
            out0, args0 = var
            out1, args1 = val
            assert (len(args0) == len(args1))
            # push new simpler equations back
            eqs.insert (0, (exp, out0, out1))
            for i in range (len (args0)):
                eqs.insert (0, (exp, args0[i], args1[i]))
            return
        elif var == '?' or val == '?':
            # wildcard type
            return
        else:
            # 4: types don't match, it's an error
            raise TypeError (exp, var, val)

        # add this equation to the substitution
        substs[var] = val
        # run through all elements of substitution again...
        for k in substs.keys():
            # ... replacing any references to <var>
            #print 'second pass:'
            #print '  before:', substs[k]
            substs[k] = apply1 (substs[k])
            #print '   after:', substs[k]

    def infer_types (self, exp):
        eqs = self.make_type_equations (exp)
        print '------ equations ----------'
        #for eq in eqs:
        #    print eq
        self.print_equations (eqs)
        print '---------------------------'
        substs = {}
        while len(eqs):
            n, var, val = eqs.pop (0)
            try:
                self.extend_subst (eqs, substs, var, val, n)
            except TypeError, te:
                print '--- type error ---'
                te.node.pprint()
                print '<',
                print_type (var)
                print '> != <',
                print_type (val)
                print '>'
                print '--- type error ---'
                raise
                
            #self.print_equations (substs.items())
        print '<<<<<<<<>>>>>>>'
        substs = substs.items()
        substs.sort()
        self.print_equations (substs)
        for key, val in substs:
            key.type = val

def compare_types (t0, t1):
    if type(t0) is str and type(t1) is str:
        if t0 == '?' or t1 == '?':
            return True
        else:
            return t0 == t1
    else:
        out0, args0 = t0
        out1, args1 = t1
        if compare_types (out0, out1) and len(args0) == len(args1):
            for i in range (len (args0)):
                if not compare_types (args0[i], args1[i]):
                    return False
            return True
        else:
            return False


# ok, this was a working manual typer I used in a much later version, just before
#   I gave in and added type inference.  keeping this for reference.

class typer:
    def __init__ (self, safety):
        self.safety = safety

    def go (self, node):
        self.walk (node, None)

    # XXX should probably do alpha conversion *before* typing,
    #   there's no reason there should be two implemenations of this.
    def lookup (self, var, lenv):
        while lenv:
            formals, lenv = lenv
            # walk rib backwards for the sake of <let*>
            for i in range (len(formals)-1, -1, -1):
                formal = formals[i]
                if var.name == formal.name:
                    return formal
        raise analyze.UnboundVariableError (var)

    def check (self, node, expected_type):
        print 'check %r %r' % (node, expected_type)
        if node.type is not None:
            if expected_type is not None:
                if node.type != expected_type:
                    raise TypeError ("type mismatch: %r %r" % (node, expected_type))
            else:
                print 'typecheck needed %r %r' % (node, expected_type)
        else:
            node.type = expected_type

    def resolve (self, n1, n2):
        # resolve the type of two nodes.
        self.check (n1, n2.type)
        self.check (n2, n1.type)

    def walk (self, node, lenv):
        getattr (self, 'walk_%s' % (node.kind,)) (node, lenv)

    def walk_literal (self, node, lenv):
        # literals are self-typed
        pass

    def walk_primapp (self, node, lenv):
        # right now, we have a small population of prims
        # defined in backend.py.  however, we want to move toward
        # user-defined primops (defined in terms of %%cexp).  For
        # now, we'll use the type info we have there.
        cexp, (result_type, arg_types) = backend.primop_types[node.name]
        for i in range (len (node.args)):
            self.walk (node.args[i], lenv)
            self.check (node.args[i], arg_types[i])
        self.check (node, result_type)
    
    def walk_function (self, node, lenv):
        self.walk (node.body, (node.formals, lenv))
        fun_type = (node.body.type, tuple([x.type for x in node.formals]))
        self.check (node, fun_type)

    def walk_varref (self, node, lenv):
        formal = self.lookup (node, lenv)
        self.resolve (node, formal)

    def walk_varset (self, node, lenv):
        formal = self.lookup (node, lenv)
        self.walk (node.value, lenv)
        self.check (node.value, formal.type)
        self.check (node, 'undefined')
        self.resolve (node, formal)

    def walk_fix (self, node, lenv):
        # XXX do we need to do anything fancy like a topological sort?
        #     what to do with mutually recursive funs?  multiple passes?
        # extend environment
        lenv = (node.names, lenv)
        # type the inits
        for i in range (len (node.inits)):
            self.walk (node.inits[i], lenv)
            print 'FIX --------------------->', node.inits[i]
            self.check (node.names[i], node.inits[i].type)
        # walk the body
        self.walk (node.body, lenv)
        self.check (node, node.body.type)

    def walk_let_splat (self, node, lenv):
        names = []
        lenv = (names, lenv)
        # type the inits
        n = len (node.inits)
        for i in range (n):
            # add each name only after its init
            self.walk (node.inits[i], lenv)
            self.check (node.names[i], node.inits[i].type)
            names.append (node.names[i])
        # walk the body
        self.walk (node.body, lenv)
        self.check (node, node.body.type)

    def walk_sequence (self, node, lenv):
        for exp in node.exprs:
            self.walk (exp, lenv)
        self.check (node, node.exprs[-1].type)

    def walk_application (self, node, lenv):
        # walk everything
        for sub in node.subs:
            self.walk (sub, lenv)
        # do we know the type of this function?
        if not node.rator.type:
            # can we go off and figure it out?
            pass
        if node.rator.type:
            # yup, so let's verify the args and assign the result type
            result_type, arg_types = node.rator.type
            if len(arg_types) != len (node.rands):
                raise ValueError ("arg count mismatch")
            else:
                for i in range (len (node.rands)):
                    self.check (node.rands[i], arg_types[i])
            self.check (node, result_type)

    def walk_conditional (self, node, lenv):
        for sub in node.subs:
            self.walk (sub, lenv)
        self.check (node.test_exp, 'bool')
        # can we typematch the two branches?
        then_type = node.then_exp.type
        else_type = node.else_exp.type
        if then_type == else_type:
            # yup, we're good
            self.check (node, then_type)
        else:
            # union type
            self.check (node, None)

    def walk_cexp (self, node, lenv):
        result_type, arg_types = node.type_sig
        assert (len (arg_types) == len (node.args))
        for i in range (len (node.args)):
            arg = node.args[i]
            self.check (node.args[i], arg_types[i])
            self.walk (node.args[i], lenv)
        self.check (node, result_type)
