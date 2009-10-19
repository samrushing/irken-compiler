# -*- Mode: Python -*-

import nodes
from pprint import pprint as pp

is_a = isinstance

# yeah, it's a global.  get over it.
#  XXX at some point, move some of these funs into a class, and fix these.
classes = {}
datatypes = {}
field_names = {}
verbose = False

# the_subst is not really used, just helps to keep track of
#  tvars that have been assigned.
the_subst = {}

class type_variable:

    def __init__ (self, num=None):
        if num is None:
            self.num = nodes.serial.next()
        else:
            self.num = num
        self.val = None
        the_subst[self.num] = self

    def __cmp__ (self, other):
        if is_a (other, type_variable):
            return cmp (self.num, other.num)
        else:
            return -1

    def __hash__ (self):
        return self.num

    def __repr__ (self):
        if self.val is None:
            return '<t%d>' % (self.num,)
        else:
            return '<t%d %r>' % (self.num, self.val)

class unit:
    def __cmp__ (self, other):
        if is_a (other, unit):
            return 0
        else:
            return -1
    def __repr__ (self):
        return '<unit>'

class array:
    def __init__ (self, type):
        self.type = type

    def __cmp__ (self, other):
        if is_a (other, array):
            return cmp (self.type, other.type)
        else:
            return -1

    def __repr__ (self):
        return '%s[]' % (self.type,)

class product:

    def __init__ (self, types, name=None):
        self.name = name
        self.types = []
        for t in types:
            if t == '?':
                # XXX this should be done in transform.py
                self.types.append (type_variable())
            elif is_a (t, list):
                # XXX need a central type parser
                self.types.append (nodes.parse_type (t))
            else:
                self.types.append (t)

    def __cmp__ (self, other):
        if is_a (other, product):
            return cmp (self.types, other.types)
        else:
            return -1

    def gen_constructor (self):
        n = len (self.types)
        names = ['p%d' % i for i in range (n)]
        vardefs = [nodes.vardef (names[i]) for i in range (n)]
        varrefs = [nodes.varref(x) for x in names]
        body = nodes.make_tuple (self.name, 0, varrefs)
        fun = nodes.function (self.name, vardefs, body)
        fun.constructor = True
        return self.name, fun

    def get_datatype_constructors (self):
        return [self.name]

    def get_constructor_types (self, ignore_fun):
        return self.types

    def __repr__ (self):
        return '{' + '*'.join ([str(x) for x in self.types]) + '}'

class union:
    def __init__ (self, name, alts):
        self.name = name
        self.alts = []
        for sname, stype in alts:
            if stype == '?':
                # XXX this should be done in transform.py
                stype = type_variable()
            self.alts.append ((sname, stype))

    def __cmp__ (self, other):
        # NOTE! these __cmp__ methods are more important than they seem,
        #  because they are responsible for catching type errors!
        if is_a (other, union):
            if self.name == other.name:
                return cmp (self.alts, other.alts)
            else:
                return -1
        else:
            return -1

    def get_field_type (self, selector):
        for i in range (len (self.alts)):
            sname, stype = self.alts[i]
            if selector == sname:
                return stype, i
        raise ValueError ("union: no such field/selector")

    def gen_constructor (self, selector):
        # build a constructor as a node tree - before analyse is called.
        stype, index = self.get_field_type (selector)
        if is_a (stype, product):
            formals = [nodes.vardef ('%s_%d' % (selector, i)) for i in range (len (stype.types))]
            args = [nodes.varref (x.name) for x in formals]
        elif is_a (stype, unit):
            formals = []
            args = []
        else:
            formals = [nodes.vardef (selector)]
            args = [nodes.varref (selector)]
        body = nodes.make_tuple (self.name, index, args)
        name = '%s/%s' % (self.name, selector)
        fun = nodes.function (self.name, formals, body)
        fun.constructor = True
        fun.selector = selector
        return name, fun

    def get_datatype_constructors (self):
        return ['%s/%s' % (self.name, alt[0]) for alt in self.alts]

    def get_constructor_types (self, fun):
        stype, index = self.get_field_type (fun.selector)
        if is_a (stype, product):
            return stype.types
        elif is_a (stype, unit):
            return []
        else:
            return [stype]

    def __repr__ (self):
        alts = ' '.join ([('%s:%s' % tuple(x)) for x in self.alts])
        return '{union %s %s}' % (self.name, alts)

field_name_map = {}

class klass:

    def __init__ (self, name, fields, methods):
        self.name = name
        # fields = [(<name>, <type>), ...]
        self.fields = []
        for fname, ftype in fields:
            # XXX need the generic type parser!!!
            if ftype is None:
                self.fields.append ((fname, type_variable()))
            else:
                self.fields.append ((fname, ftype))
            if field_name_map.has_key (fname):
                if field_name_map[fname] != self.name:
                    raise TypeError ("field %r already defined in class %r" % (fname, field_name_map[fname]))
            else:
                field_name_map[fname] = self.name
        self.methods = methods

    def gen_constructor (self):
        n = len (self.fields)
        names = [name for name, type in self.fields]
        # we don't want to use that <type> param, it might be polymorphic
        vardefs = [nodes.vardef (names[i]) for i in range (n)]
        varrefs = [nodes.varref (names[i]) for i in range (n)]
        body = nodes.make_tuple (self.name, 0, varrefs)
        fun = nodes.function (self.name, vardefs, body)
        fun.constructor = True
        return self.name, fun

    def get_datatype_constructors (self):
        return [self.name]

    def get_field_type (self, name):
        for fname, ftype in self.fields:
            if name == fname:
                if ftype == self.name:
                    # recursive data type
                    return self
                else:
                    return ftype
        return None

    def get_method (self, name):
        for mname, method in self.methods:
            if name == mname:
                return '%s-%s' % (self.name, mname)
        raise ValueError ("unknown field/method name: %s" % (name,))

    def get_field_offset (self, name):
        for i in range (len (self.fields)):
            fname, type = self.fields[i]
            if name == fname:
                return i
        raise ValueError ("unknown field name: %s" % (name,))

    def get_constructor_types (self, ignore_fun):
        return [ ftype for fname, ftype in self.fields ]

    def __cmp__ (self, other):
        if is_a (other, klass):
            if self.name == other.name:
                # is this good enough??
                return cmp (self.fields, other.fields)
            else:
                return -1
        else:
            return -1

    def __repr__ (self):
        return '{%s %s}' % (self.name, ' '.join (['%s:%s' % x for x in self.fields]))

def apply_subst_to_type (t):
    # replace all known tvars in <t>
    if is_a (t, str):
        # plain type
        return t
    elif is_a (t, type_variable):
        # a type variable
        probe = lookup_subst (t)
        if probe:
            return probe
        else:
            return t
    elif is_a (t, tuple):
        # a function
        result_type, arg_types = t
        return (
            apply_subst_to_type (result_type),
            tuple ([ apply_subst_to_type (x) for x in arg_types ])
            )
    elif is_a (t, klass):
        fields = []
        for fname, ftype in t.fields:
            fields.append ((fname, apply_subst_to_type (ftype)))
        return klass (t.name, fields, t.methods)
    elif is_a (t, array):
        return array (apply_subst_to_type (t.type))
    elif is_a (t, product):
        return product ([apply_subst_to_type (x) for x in t.types])
    elif is_a (t, union):
        return union (t.name, [(fn, apply_subst_to_type (ft)) for (fn, ft) in t.alts])
    elif is_a (t, unit):
        return t
    elif is_a (t, forall):
        # XXX
        return t
    else:
        raise ValueError

def extend_subst (tvar, type):
    #print '%r == %r' % (tvar, type)
    if tvar.val is not None and tvar.val != type:
        raise KeyError ("type var already bound!")
    else:
        tvar.val = type

def get_type_variable (num):
    if the_subst.has_key (num):
        return the_subst[num]
    else:
        return type_variable (num)

def lookup_subst (tvar):
    t = tvar
    while is_a (t, type_variable):
        if t.val is not None:
            t = t.val
            # path compression
            tvar.val = t
        else:
            break
    return t

# reconcile types t1 and t2 from <exp> given <subst>
def unify (ot1, ot2, tenv, exp):
    t1 = apply_subst_to_type (ot1)
    t2 = apply_subst_to_type (ot2)
    print 'unify', t1, '  ====  ', t2
    if t1 == t2:
        # happy happy joy joy
        pass
    elif is_a (t1, type_variable):
        # type variable
        occurrence_check (t1, t2, exp)
        extend_subst (t1, t2)
    elif is_a (t2, type_variable):
        # other way
        occurrence_check (t1, t2, exp)
        extend_subst (t2, t1)
    elif is_a (t1, tuple) and is_a (t2, tuple):
        # function types
        r1, args1 = t1
        r2, args2 = t2
        # extend with arg types
        if len(args1) != len(args2):
            raise TypeError (("arg count mismatch", t1, t2, exp))
        for i in range (len (args1)):
            unify (args1[i], args2[i], tenv, exp)
        # extend with result type
        unify (r1, r2, tenv, exp)
    elif is_a (t1, klass) and is_a (t2, klass):
        if t1.name == t2.name and len(t1.fields) == len(t2.fields):
            for i in range (len (t1.fields)):
                unify (t1.fields[i][1], t2.fields[i][1], tenv, exp)
        else:
            raise TypeError ((t1, t2, exp))
    elif is_a (t1, array) and is_a (t2, array):
        unify (t1.type, t2.type, tenv, exp)
    elif is_a (t1, product) and is_a (t2, product):
        for i in range (len (t1.types)):
            unify (t1.types[i], t2.types[i], tenv, exp)
    elif is_a (t1, union) and is_a (t2, union):
        if t1.name == t2.name and len(t1.alts) == len(t2.alts):
            for i in range (len (t1.alts)):
                sname1, stype1 = t1.alts[i]
                sname2, stype2 = t2.alts[i]
                unify (stype1, stype2, tenv, exp)
        else:
            raise TypeError ((t1, t2, exp))
    # ahhh... this may be the hack I need to support recursive types...
    elif is_a (t1, str):
        t1 = apply_tenv (tenv, t1)
        if is_a (t1, str):
            if t1 != t2:
                raise TypeError ((t1, t2, exp))
        else:
            unify (t1, t2, tenv, exp)
    elif is_a (t2, str):
        t2 = apply_tenv (tenv, t2)
        if is_a (t2, str):
            if t2 != t1:
                raise TypeError ((t1, t2, exp))
        else:
            unify (t2, t1, tenv, exp)
    else:
        raise TypeError ((t1, t2, exp))

def occurs_in_type (tvar, t):
    # does <tvar> occur in <t>?
    if is_a (t, str):
        # type
        return False
    elif is_a (t, type_variable):
        # type variable
        return t == tvar
    elif is_a (t, klass):
        for fname, ftype in t.fields:
            if occurs_in_type (tvar, ftype):
                return True
        else:
            return False
    elif is_a (t, array):
        return occurs_in_type (tvar, t.type)
    elif is_a (t, product):
        for tp in t.types:
            if occurs_in_type (tvar, tp):
                return True
    elif is_a (t, union):
        for sname, stype in t.alts:
            if occurs_in_type (tvar, stype):
                return True
    elif is_a (t, unit):
        return False
    else:
        # function
        result_type, arg_types = t
        for arg in arg_types:
            if occurs_in_type (tvar, arg):
                return True
        else:
            if occurs_in_type (tvar, result_type):
                return True
            else:
                return False

def occurrence_check (tvar, t, exp):
    if occurs_in_type (tvar, t):
        raise TypeError ((tvar, t, exp))

# XXX understand this.
def occurs_free_in_tenv (tvar, tenv):
    while tenv:
        rib, tenv = tenv
        for var, type in rib:
            # XXX this doesn't look right - shouldn't it look into
            #  <forall>'s and ignore just the bound type vars?
            if not is_a (type, forall) and occurs_in_type (tvar, type):
                return True
    return False

# if a node has user-supplied type, use it.  otherwise
#   treat it as a type variable.
def optional_type (exp, tenv):
    if exp.type:
        return apply_tenv (tenv, exp.type)
    else:
        return get_type_variable (exp.serial)

the_type_map = {
    'int':'int',
    'string':'string',
    'bool':'bool',
    'char':'char',
    'continuation':'continuation',
    'undefined':'undefined',
    'symbol':'symbol',
}

def initial_type_environment():
    constructors = []
    for name, dt in datatypes.iteritems():
        poly_dt = build_type_scheme (dt, None, name)
        # store this type scheme in the type map
        the_type_map[name] = poly_dt
        for name in dt.get_datatype_constructors():
            constructors.append ((name, poly_dt))
    return constructors

class forall:
    def __init__ (self, gens, type):
        self.gens = gens
        self.type = type

    def __repr__ (self):
        return '<forall %r %r>' % (self.gens, self.type)

# build_tscheme (type, tenv):
#   this will build a type scheme given a type, by finding all the
#   non-free tvars in the expression.

def build_type_scheme (type, tenv, name):
    
    gens = set()

    def list_generic_tvars (t):
        if is_a (t, type_variable):
            if not occurs_free_in_tenv (t, tenv):
                gens.add (t)
        elif is_a (t, tuple):
            # procedure
            result_type, arg_types = t
            for arg_type in arg_types:
                list_generic_tvars (arg_type)
            list_generic_tvars (result_type)
        elif is_a (t, str):
            pass
        elif is_a (t, klass):
            for fname, ftype in t.fields:
                list_generic_tvars (ftype)
        elif is_a (t, array):
            list_generic_tvars (t.type)
        elif is_a (t, union):
            for sname, stype in t.alts:
                list_generic_tvars (stype)
        elif is_a (t, product):
            for tp in t.types:
                list_generic_tvars (tp)
        elif is_a (t, unit):
            pass
        else:
            raise ValueError

    list_generic_tvars (type)
    if not gens:
        return type
    else:
        r = forall (gens, type)
        if verbose:
            print 'built type scheme for %s:%r' % (name, r)
        return r

def instantiate_type (type, tvar, fresh_tvar):
    def f (t):
        if is_a (t, type_variable):
            if t == tvar:
                return fresh_tvar
            else:
                return t
        elif is_a (t, str):
            return t
        elif is_a (t, tuple):
            result_type, arg_types = t
            return (f (result_type), tuple ([f(x) for x in arg_types]))
        elif is_a (t, klass):
            return klass (t.name, [ (fname, f (ftype)) for (fname, ftype) in t.fields ], t.methods)
        elif is_a (t, array):
            return array (f (t.type))
        elif is_a (t, product):
            return product ([f(x) for x in t.types])
        elif is_a (t, union):
            return union (t.name, [ (sn, f(st)) for sn, st in t.alts ])
        elif is_a (t, unit):
            return t
        else:
            raise ValueError
    return f (type)

def instantiate_type_scheme (tscheme):
    gens = tscheme.gens
    body = tscheme.type
    for gen in gens:
        body = instantiate_type (body, gen, type_variable())
    return body

def apply_tenv (tenv, name):

    def inst (t):
        if is_a (t, forall):
            r = instantiate_type_scheme (t)
            if verbose:
                print 'type of %r %r instantiated as %r' % (name, t, r)
            return r
        else:
            return t

    while tenv:
        rib, tenv = tenv
        # walk the rib backwards for the sake of let*
        for i in range (len(rib)-1, -1, -1):
            var, type = rib[i]
            if var == name:
                # is this a type scheme?
                return inst (type)

    if the_type_map.has_key (name):
        return inst (the_type_map[name])
    else:
        raise ValueError (name)

def parse_cexp_type (t, exp, tenv):
    if t == '?':
        return type_variable (exp.serial)
    elif classes.has_key (t):
        # XXX I think this is a hack
        return apply_tenv (tenv, t)
    elif is_a (t, list):
        import pdb; pdb.set_trace()
    else:
        return t

class typer:

    def __init__ (self, verbose=False):
        self.verbose = verbose
        self.attributes_to_resolve = {}

    def go (self, exp):
        global the_dep_graph, the_scc_graph
        the_dep_graph = build_dependency_graph (exp)
        the_scc_graph, scc_map = strongly (the_dep_graph)
        pp (the_scc_graph)
        tenv = (initial_type_environment(), None)
        result = self.type_of (exp, tenv)
        pp (the_subst)
        if self.verbose:
            pp (the_subst)
            for n in exp:
                if n.is_a ('function'):
                    print n.name, print_type (n.type)
        return result
        
    def type_of (self, exp, tenv):
        kind = exp.kind
        method = getattr (self, 'type_of_%s' % (kind,))
        t_exp = method (exp, tenv)
        t_exp = apply_subst_to_type (t_exp)
        if is_a (t_exp, str):
            # XXX resurrected this hack again for klasses.
            t_exp = apply_tenv (tenv, t_exp)
        exp.type = t_exp
        return t_exp

    def type_of_literal (self, exp, tenv):
        return exp.type

    def type_of_cexp (self, exp, tenv):
        result_type, arg_types = exp.type_sig
        result_type = parse_cexp_type (result_type, exp, tenv)
        for i in range (len (arg_types)):
            arg_type = arg_types[i]
            arg = exp.args[i]
            if is_a (arg_type, str):
                # strip off wrap options (like "string/raw")
                arg_type = arg_types[i].split('/')[0]
            ta = self.type_of (arg, tenv)
            arg_type = parse_cexp_type (arg_type, arg, tenv)
            unify (ta, arg_type, tenv, arg)
        return result_type

    def type_of_varref (self, exp, tenv):
        result = apply_tenv (tenv, exp.name)
        return result

    def type_of_varset (self, exp, tenv):
        # XXX implement the no-generalize rule for vars that are assigned.
        t1 = apply_tenv (tenv, exp.name)
        t2 = self.type_of (exp.value, tenv)
        unify (t1, t2, tenv, exp.value)
        return 'undefined'

    def type_of_conditional (self, exp, tenv):
        t1 = self.type_of (exp.test_exp, tenv)
        unify (t1, 'bool', tenv, exp.test_exp)
        t2 = self.type_of (exp.then_exp, tenv)
        t3 = self.type_of (exp.else_exp, tenv)
        unify (t2, t3, tenv, exp)
        return t2

    def type_of_let_splat (self, exp, tenv):
        n = len (exp.inits)
        for i in range (n):
            ta = self.type_of (exp.inits[i], tenv)
            tenv = ([(exp.names[i].name, ta)], tenv)
        return self.type_of (exp.body, tenv)

    def type_of_function (self, exp, tenv):
        if exp.constructor:
            assert (exp.body.is_a ('make_tuple'))
            body_type = apply_tenv (tenv, exp.body.type)
            arg_types = body_type.get_constructor_types (exp)
            for i in range (len (arg_types)):
                ft = optional_type (exp.formals[i], tenv)
                exp.formals[i].type = arg_types[i]
            return (body_type, tuple (arg_types))
        else:
            type_rib = []
            arg_types = []
            for formal in exp.formals:
                t = optional_type (formal, tenv)
                arg_types.append (t)
                type_rib.append ((formal.name, t))
            body_type = self.type_of (exp.body, (type_rib, tenv))
            return (body_type, tuple (arg_types))

    def type_of_application (self, exp, tenv):
        n = len (exp.rands)
        # I think these exceptions should maybe be done as primapp instead?
        #  like this?: (primapp (record list) ...)
        rator = exp.rator
        if rator.is_a ('get'):
            # catch method invocation
            ob_type = self.type_of (rator.ob, tenv)
            # two possibilities here: invoking a *method*, or invoking a function
            #  stored in a slot.  for now, handle methods only.
            method = ob_type.get_method (rator.name)
            # transform the expression
            # ((get ob meth) arg0 arg1 ...)
            # => (class-meth ob arg0 arg1 ...)
            exp.rands.insert (0, rator.ob)
            exp.rator = nodes.varref (method)
            exp.subs = [exp.rator] + exp.rands
            # XXX ugh, hacks
            exp.fix_attribute_names()
            exp.rator.fix_attribute_names()
            #import pdb; pdb.set_trace()
            return self.type_of_application (exp, tenv)
        else:
            rator_type = self.type_of (exp.rator, tenv)
            if is_a (rator_type, array):
                for i in range (n):
                    ta = self.type_of (exp.rands[i], tenv)
                    unify (ta, rator_type.type, tenv, exp)
                return rator_type
            else:
                # normal application
                arg_types = []
                for i in range (n):
                    ta = self.type_of (exp.rands[i], tenv)
                    arg_types.append (ta)
                result_type = get_type_variable (exp.serial) # new type variable
                unify (rator_type, (result_type, tuple(arg_types)), tenv, exp)
                return result_type

    def type_of_fix (self, exp, tenv):
        partition = partition_fix (exp)
        check_partition (exp, partition)
        # reorder fix into dependency order
        partition = reorder_fix (exp, partition)
        n = len (exp.inits)
        init_tvars = [None] * n
        init_types = [None] * n
        n2 = 0
        # new type var for each init
        for i in range (n):
            init_tvars[i] = get_type_variable (exp.inits[i].serial)
        for part in partition:
            type_rib = []
            # build temp tenv for typing the inits
            for i in part:
                # for each function
                init = exp.inits[i]
                name = exp.names[i].name
                type_rib.append ((name, init_tvars[i]))
            temp_tenv = (type_rib, tenv)
            # type each init in temp_tenv
            for i in part:
                init = exp.inits[i]
                ti = self.type_of (init, temp_tenv)
                unify (ti, init_tvars[i], temp_tenv, init)
                init_types[i] = ti
            # now extend the environment with type schemes instead
            type_rib = []
            for i in part:
                name = exp.names[i].name
                tsi = build_type_scheme (init_types[i], tenv, name)
                type_rib.append ((name, tsi))
            # we now have a polymorphic environment for this subset
            tenv = (type_rib, tenv)
            n2 += len (type_rib)
        assert (n2 == n)
        # and type the body in that tenv
        return self.type_of (exp.body, tenv)

    def type_of_sequence (self, exp, tenv):
        for sub in exp.subs[:-1]:
            # everything but the last, type it as don't-care
            ti = self.type_of (sub, tenv)
        return self.type_of (exp.subs[-1], tenv)

    def type_of_get (self, exp, tenv):
        ob_type = self.type_of (exp.ob, tenv)
        # look up the class from the globally unique field name
        probe = field_name_map.get (exp.name, None)
        if probe is None:
            raise TypeError ("unknown class / field name", exp)
        else:
            base_type = apply_tenv (tenv, probe)
            # XXX hack
            exp.ob.type = base_type
            unify (ob_type, base_type, tenv, exp)
            tf = base_type.get_field_type (exp.name)
            print ob_type, base_type, tf
            return tf

    def type_of_set (self, exp, tenv):
        ob_type = self.type_of (exp.ob, tenv)
        # XXX implement the no-generalize rule for vars that are assigned.
        probe = field_name_map.get (exp.name, None)
        if probe is None:
            raise TypeError ("unknown class / field name", exp)
        else:
            base_type = apply_tenv (tenv, probe)
            tf = base_type.get_field_type (exp.name)
            tval = self.type_of (exp.val, tenv)
            unify (ob_type, base_type, tenv, exp)
            unify (tval, tf, tenv, exp.val)
            return 'undefined'

    def type_of_make_tuple (self, exp, tenv):
        # XXX examine the types available - think about complicated
        #     cases like 'closure'...
        result_type = apply_tenv (tenv, exp.type)
        for arg in exp.args:
            # this is kindof a 'dead' type judgement?
            ta = self.type_of (arg, tenv)
        return result_type

    def type_of_primapp (self, exp, tenv):
        if exp.name == '%%vector-literal':
            n = len (exp.args)
            if n == 0:
                raise ValueError ("don't know how to type a zero-length vector yet")
            else:
                ta0 = self.type_of (exp.args[0], tenv)
                for i in range (1, n):
                    ta = self.type_of (exp.args[i], tenv)
                    unify (ta0, ta, tenv, exp)
            return array (ta0)
        elif exp.name in ('%%array-ref', '%%product-ref'):
            # (%%array-ref <array> <index>)
            base = exp.args[0]
            index = exp.args[1]
            bt = self.type_of (base, tenv)
            if is_a (bt, str):
                bt = apply_tenv (tenv, bt)
            if exp.name == '%%array-ref':
                if is_a (bt, array):
                    it = self.type_of (index, tenv)
                    # we don't know the item type yet...
                    item_type = type_variable()
                    ta = array (item_type)
                    unify (bt, ta, tenv, exp)
                    unify (it, 'int', tenv, exp)
                    return item_type
                else:
                    raise ValueError ("illegal type for array-reference")
            elif exp.name == '%%product-ref':
                if is_a (bt, product):
                    assert (index.is_a ('literal') and index.type == 'int')
                    assert (index.value < len(bt.types))
                    print bt, index.value
                    item_type = bt.types[index.value]
                    return item_type
                else:
                    raise ValueError ("illegal base type for tuple reference")
        # implement %%product-set
        elif exp.name == '%%array-set':
            # (%%array-set <array> <index> <value>)
            base, index, value = exp.args
            bt = self.type_of (base, tenv)
            it = self.type_of (index, tenv)
            vt = self.type_of (value, tenv)
            ta = array (vt)
            unify (bt, ta, tenv, exp)
            unify (it, 'int', tenv, exp)
            return 'undefined'
        else:
            raise ValueError ("can't type unknown primop %s" % (exp.name,))

    def type_of_typecase (self, exp, tenv):
        tt = apply_tenv (tenv, exp.vtype)
        vt = self.type_of (exp.value, tenv)
        unify (vt, tt, tenv, exp)
        # verify that all of the variants are accounted for...
        names0 = set ([sname for sname, stype in tt.alts])
        names1 = set ([x[0] for x in exp.alt_formals])
        if names0 != names1:
            raise ValueError ("set of union/sum type alternatives does not match in typecase")
        else:
            n = len (exp.alt_formals)
            texp = get_type_variable (exp.serial)
            exp.vtype = tt
            new_alts = [None] * n
            new_alt_formals = [None] * n
            for i in range (n):
                sname, formals = exp.alt_formals[i]
                body = exp.alts[i]
                ftype, index = tt.get_field_type (sname)
                if is_a (ftype, product):
                    type_rib = []
                    for j in range (len (ftype.types)):
                        type_rib.append ((formals[j].name, ftype.types[j]))
                elif is_a (ftype, unit):
                    type_rib = []
                else:
                    type_rib = [(formals[0].name, ftype)]
                tenv2 = (type_rib, tenv)
                bt = self.type_of (body, tenv2)
                unify (texp, bt, tenv2, exp)
                # sort 'em
                new_alts[index] = exp.alts[i]
                new_alt_formals[index] = formals
            exp.alts = new_alts
            exp.alt_formals = new_alt_formals
            exp.params = exp.vtype, exp.alt_formals
            exp.subs = [exp.value] + exp.alts
        return texp

def build_dependency_graph (root):
    g = {}
    def search (exp, current_fun):
        if exp.is_a ('varref'):
            current_fun.add (exp.params)
        elif exp.is_a ('fix'):
            for i in range (len (exp.names)):
                name = exp.names[i].name
                init = exp.inits[i]
                fun = set()
                g[name] = fun
                search (init, fun)
            search (exp.body, current_fun)
        else:
            for sub in exp.subs:
                search (sub, current_fun)
    g['top'] = set()
    search (root, g['top'])
    #pp (g)
    return g

def transpose (g):
    gt = {}
    for k in g.keys():
        gt[k] = set()
    for k, vl in g.items():
        for v in vl:
            if gt.has_key (v):
                gt[v].add (k)
            else:
                gt[v] = set ([k])
    return gt

# http://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
#
# Finds the strongly-connected components of the graph.  We need this to find
# out how a pedantic programmer might have grouped a set of functions carefully
# into letrecs, so that we can isolate such groups - otherwise they're all typed
# together as a single letrec.  That causes polymorphic instantiation to fail in
# many cases, because HM disallows polymorphism in recursive functions.  [yes,
# it's hard to explain]

def strongly (g):
    s = []
    visited = set()
    unknown = set()

    def visit0 (u):
        visited.add (u)
        if g.has_key (u):
            for v in g[u]:
                if v not in visited:
                    visit0 (v)
        else:
            unknown.add (u)
        s.append (u)

    # walk the graph forward, pushing finished nodes onto <s>
    for u in g.keys():
        if u not in visited:
            visit0 (u)
        
    gt = transpose (g)
    visited = set()

    def visit1 (u):
        visited.add (u)
        for v in gt[u]:
            if v not in visited:
                visit1 (v)
        r1.add (u)

    # walk backward, popping strongly connected components off <s>
    r0 = []
    while s:
        u = s.pop()
        if u not in visited:
            r1 = set()
            visit1 (u)
            # a strongly-connected component, collect it.
            r0.append (r1)

    # I think this puts the subcomponents in topological order.
    r0.reverse()
    # make a handy map from vertex => component
    map = {}
    for component in r0:
        for v in component:
            map[v] = component
    return r0, map

def partition_fix (exp):
    # partition the functions in this fix into sets of mutually-recursive functions
    vardefs = exp.names
    name_map = {}
    # map of <name> => <index>
    for i in range (len (vardefs)):
        name_map[vardefs[i].name] = [i, False]
    names = [x.name for x in vardefs]
    inits = exp.inits
    n = len (inits)
    leftover = range (n)
    parts = [[]]
    for component in the_scc_graph:
        if len(parts[-1]):
            parts.append ([])
        for name in component:
            probe = name_map.get (name, None)
            if probe and not probe[1]:
                # index
                parts[-1].append (probe[0])
                # flag it as done
                probe[1] = True
                leftover.remove (probe[0])
    # the leftovers should all be non-functions
    parts.insert (0, leftover)
    return parts

def check_partition (exp, partition):
    names = [ x.name for x in exp.names ]
    n = len (names)
    reordered = []
    for part in partition:
        for i in part:
            name = names[i]
            reordered.append (name)
    for i in range (n):
        name = reordered[i]
        deps = the_dep_graph[name]
        for dep in deps:
            for j in range (i+1, n):
                if dep == reordered[j]:
                    print '***** bad dependency order ****'
                    import pdb; pdb.set_trace()

def reorder_fix (exp, partition):
    n = len(exp.inits)
    names = []
    inits = []
    r = []
    i = 0
    for part in partition:
        r.append ([])
        for j in part:
            names.append (exp.names[j])
            inits.append (exp.inits[j])
            r[-1].append (i)
            i += 1
    # XXX rejigger node data
    exp.names = exp.params = names
    exp.inits = inits
    body = exp.subs[-1]
    exp.subs = inits + [body]
    assert (len(exp.inits) == n)
    return r

# XXX this wouldn't be necessary if we had an 'arrow' type,
#    i.e., repr() would be good enough.

def print_type (t):
    if is_a (t, str):
        return t
    elif is_a (t, tuple):
        result_type, arg_types = t
        arg_types = ' * '.join ([print_type(x) for x in arg_types])
        return '(%s -> %s)' % (arg_types, print_type (result_type))
    elif is_a (t, type_variable):
        if t.val is None:
            return '%d' % (t.num,)
        else:
            return print_type (t.val)
    else:
        return repr (t)
        #raise NotImplementedError

if __name__ == '__main__':
    s = test_strongly()
         
