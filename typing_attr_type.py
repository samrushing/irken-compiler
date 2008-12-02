# -*- Mode: Python -*-

import lambda_tree as tree
from pprint import pprint as pp

is_a = isinstance

# yeah, it's a global.  get over it.
#  XXX at some point, move some of these funs into a class, and fix these.
classes = {}
verbose = False

# the schizoid style of this file.
# this material is pretty challenging... my pattern is to start with working code,
#  modeled directly on either scheme or ml (depending on the reference I'm using at the time),
#  and as I understand it more completely I'll rewrite it in a more pythonic style.

# inference.
#  we're solving a series of equations.
# see eoplv3:7.4

# types of 'type':
# a string means a base type - 'int', 'bool', etc...
# an integer means a type variable.  think of '34' as 't34'.
# a tuple means a function type: (int, (bool, string)) means a function
#    taking (bool, string) args and returning an int.
# an attr_type instance is a class attribute reference, and acts like
#    a type variable.
# XXX I should probably use a 'constructor' based design.

def apply1 (t0, tvar, t1):
    # substitute t1 for tvar in t0
    if is_a (t0, str):
        # plain type
        return t0
    elif is_a (t0, int):
        # a type variable
        if t0 == tvar:
            return t1
        else:
            return t0
    elif is_a (t0, attr_type):
        # a different kind of type variable
        if t0 == tvar:
            return t1
        else:
            return attr_type (apply1 (t0.base_type, tvar, t1), t0.name)
    else:
        # a function
        result_type, arg_types = t0
        return (
            apply1 (result_type, tvar, t1),
            tuple ([ apply1 (x, tvar, t1) for x in arg_types ])
            )

def apply_subst_to_type (t, subst):
    # replace all known tvars in <t>
    if is_a (t, str):
        # plain type
        return t
    elif is_a (t, int):
        # a type variable
        probe = lookup_subst (subst, t)
        if probe:
            return probe
        else:
            return t
    elif is_a (t, attr_type):
        t2 = attr_type (apply_subst_to_type (t.base_type, subst), t.name)
        probe = lookup_subst (subst, t2)
        if probe:
            return probe
        else:
            return t2
    else:
        # a function
        result_type, arg_types = t
        return (
            apply_subst_to_type (result_type, subst),
            tuple ([ apply_subst_to_type (x, subst) for x in arg_types ])
            )

# a subst is a lisp-like list of pairs, implemented with 2-tuples.

# this is classic lisp <map> on tuple-pairs.
def map_pair (proc, pair):
    if pair is None:
        return None
    else:
        car, cdr = pair
        return (proc (car), map_pair (proc, cdr))

def extend_subst (subst, tvar, type):
    # return a new <subst> where <type> is substituted for <tvar>
    def proc (p):
        lhs, rhs = p
        return (lhs, apply1 (rhs, tvar, type))
    return ((tvar, type), map_pair (proc, subst))

# scheme/lisp 'assoc' on a subst.
def lookup_subst (subst, tvar):
    while subst:
        car, cdr = subst
        lhs, rhs = car
        if lhs == tvar:
            return rhs
        else:
            subst = cdr
    else:
        return None

# attribute type variable - this acts just like a type variable, with
#   a little extra magic.  Since we're using ints as 'normal' type
#   variables, we need to make these act a little like ints in order
#   for things to work out.  Someday this will all be redone with
#   beautiful ADT's and shiny type-safe class definitions.

class attr_type:
    def __init__ (self, base_type, name):
        self.base_type = base_type
        self.name = name

    def __cmp__ (self, other):
        if not is_a (other, attr_type):
            return -1
        elif self.base_type == other.base_type and self.name == other.name:
            return 0
        else:
            return 1

    def __hash__ (self):
        return hash ('%s:%s' % (self.base_type, self.name))

    def __repr__ (self):
        return "%s.%s" % (self.base_type, self.name)

# lookup the type of a class.field attribute reference in our metadata.
def lookup_class_attribute_type (name, field, tenv):
    c = classes[name]
    for fname, type in c.fields:
        if fname == field:
            return type
    for fun in c.methods:
        meth = '%s-%s' % (name, field)
        if meth == fun[1][0]:
            # got a match, look it up in tenv
            result_type, arg_types = apply_tenv (tenv, meth)
            # strip off the <self> arg
            return (result_type, arg_types[1:])
    raise AttributeError ("class %r has no %r attribute" % (name, field))

# reconcile types t1 and t2 from <exp> given <subst>
def unify (t1, t2, subst, tenv, exp):
    t1 = apply_subst_to_type (t1, subst)
    t2 = apply_subst_to_type (t2, subst)
    print 'unify', t1, '  ====  ', t2
    if t1 == t2:
        # happy happy joy joy
        return subst
    elif is_a (t1, int):
        # type variable
        occurrence_check (t1, t2, exp)
        return extend_subst (subst, t1, t2)
    elif is_a (t2, int):
        # other way
        occurrence_check (t1, t2, exp)
        return extend_subst (subst, t2, t1)
    elif is_a (t1, tuple) and is_a (t2, tuple):
        # function types
        r1, args1 = t1
        r2, args2 = t2
        # extend with arg types
        for i in range (len (args1)):
            subst = unify (args1[i], args2[i], subst, tenv, exp)
        # extend with result type
        return unify (r1, r2, subst, tenv, exp)
    elif is_a (t2, attr_type):
        return unify_attr_types (t1, t2, subst, tenv, exp)
    elif is_a (t1, attr_type):
        return unify_attr_types (t2, t1, subst, tenv, exp)
    else:
        raise TypeError ((t1, t2, exp))

def unify_attr_types (t1, t2, subst, tenv, exp):
    occurrence_check (t2, t1, exp)
    if is_a (t2.base_type, str):
        # we know which class it is, unify with the attribute
        t = lookup_class_attribute_type (t2.base_type, t2.name, tenv)
        if t is not None:
            subst = unify (t1, t, subst, tenv, exp)
            return extend_subst (subst, t2, t)
        else:
            return extend_subst (subst, t2, t1)
    elif is_a (t1, attr_type) and is_a (t1.base_type, str):
        # Both sides are attr types, but we haven't resolved the base
        # type of t2 yet.  Have we resolved t1's base type yet?
        t = lookup_class_attribute_type (t1.base_type, t1.name, tenv)
        import pdb; pdb.set_trace()
        if t is not None:
            subst = unify (t2, t, subst, tenv, exp)
            return extend_subst (subst, t1, t)
        else:
            return extend_subst (subst, t1, t2)
    else:
        return extend_subst (subst, t2, t1)

def occurs_in_type (tvar, t):
    # does <tvar> does occur in <t>?
    if is_a (t, str):
        # type
        return False
    elif is_a (t, int):
        # type variable
        return t == tvar
    elif is_a (t, attr_type):
        if t == tvar:
            return True
        else:
            return occurs_in_type (tvar, t.base_type)
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
            if not is_a (type, forall) and occurs_in_type (tvar, type):
                return True
    return False

# if a node has user-supplied type, use it.  otherwise
#   treat it as a type variable. (exp.serial is an int)
def optional_type (exp):
    if exp.type:
        return exp.type
    else:
        return exp.serial

def subst_as_dict (subst):
    r = {}
    while subst:
        (k, v), subst = subst
        r[k] = v
    return r

def build_constructor_types():
    rib = []
    for name, klass in classes.items():
        rib.append (('make-%s' % (name,), klass.get_constructor_type()))
    return rib

def lookup_method (node):
    base_ob = node.subs[0]
    # XXX this might make assumptions that break (x.y.z arg0 arg1 ...)
    base_type = base_ob.type
    name = node.params
    c = classes[base_type]
    return c.lookup_method (name)

def type_program (exp):
    class_rib = build_constructor_types()
    tenv = (class_rib, None)
    tenv = None
    t_exp, subst = type_of (exp, tenv, None)
    map = subst_as_dict (subst)
    if verbose:
        pp (map)
    for node in exp:
        # XXX all of this seems really clumsy, I may
        #     be making this harder than it is...
        if node.type:
            node.type = apply_subst_to_type (node.type, subst)
        if node.is_a ('application') and node.rator.is_a ('get'):
            # see if this is a known method, call it directly.
            get_rator = node.rator
            name = lookup_method (get_rator)
            # replace the rator
            node.rator = node.subs[0] = tree.varref (name)
            # insert <self> into args
            node.rands.insert (0, get_rator.ob)
            node.subs.insert (1, get_rator.ob)
        if node.binds():
            names = node.get_names()
            for name in names:
                t_var = apply_subst_to_type (name.serial, subst)
                name.type = t_var

class forall:
    def __init__ (self, gens, type):
        self.gens = gens
        self.type = type

    def __repr__ (self):
        return '<forall %r %r>' % (self.gens, self.type)

# build_tscheme (type, tenv):
#   this will build a type scheme given a type, by finding all the
#   non-free tvars in the expression.  in the eoplv1 code,
#   type-dispatch automatically performs the tvar-end-value procedure.
#   this is done at the time the 'poly' type is inserted into the type
#   environment.  when apply_tenv() is called, a new instantiation of the
#   type scheme will be created for that call site.

def build_type_scheme (type, tenv, subst):
    
    gens = []

    def list_generic_tvars (t):
        # XXX attr_type?  since it's also a kind of tvar?
        t = apply_subst_to_type (t, subst)
        if is_a (t, int) or is_a (t, attr_type):
            if is_a (t, attr_type):
                import pdb; pdb.set_trace()
            if not occurs_free_in_tenv (t, tenv):
                gens.append (t)
        elif is_a (t, tuple):
            # procedure
            result_type, arg_types = t
            for arg_type in arg_types:
                list_generic_tvars (arg_type)
            list_generic_tvars (result_type)
        elif is_a (t, str):
            pass
        else:
            raise ValueError

    #import pdb; pdb.set_trace()
    list_generic_tvars (type)
    if not gens:
        return type
    else:
        r = forall (gens, type)
        print 'built type scheme', r        
        return r

def subst_repr (subst):
    result = []
    while subst:
        (k, v), subst = subst
        result.append ('%r:%r' % (k, v))
    return '{' + (', '.join (result)) + '}'

def wildcard (t, exp):
    if t == '?':
        print '>>>>>>>>>>', exp
        return exp.serial
    else:
        return t

# a wrapper for type_of helpful when debugging
def type_of (exp, tenv, subst):
    t_exp, subst = _type_of (exp, tenv, subst)
    if False:
        print 'type_of %r == %r' % (exp, t_exp)
        print tenv
        print subst_repr (subst)
    exp.type = apply_subst_to_type (t_exp, subst)
    return t_exp, subst

def _type_of (exp, tenv, subst):
    if exp.is_a ('literal'):
        return exp.type, subst
    elif exp.is_a ('cexp'):
        result_type, arg_types = exp.type_sig
        result_type = wildcard (result_type, exp)
        for i in range (len (arg_types)):
            arg = exp.args[i]
            arg_type = arg_types[i]
            ta, subst = type_of (arg, tenv, subst)
            arg_type = wildcard (arg_type, arg)
            subst = unify (ta, arg_type, subst, tenv, arg)
        return result_type, subst
    elif exp.is_a ('varref'):
        return apply_tenv (tenv, exp.name), subst
    elif exp.is_a ('varset'):
        t1 = apply_tenv (tenv, exp.name)
        t2, subst = type_of (exp.value, tenv, subst)
        subst = unify (t1, t2, subst, tenv, exp.value)
        return 'undefined', subst
    elif exp.is_a ('conditional'):
        t1, subst = type_of (exp.test_exp, tenv, subst)
        subst = unify (t1, 'bool', subst, tenv, exp.test_exp)
        t2, subst = type_of (exp.then_exp, tenv, subst)
        t3, subst = type_of (exp.else_exp, tenv, subst)
        subst = unify (t2, t3, subst, tenv, exp)
        return t2, subst
    elif exp.one_of ('let_splat'):
        n = len (exp.inits)
        type_rib = []
        for i in range (n):
            ta, subst = type_of (exp.inits[i], tenv, subst)
            type_rib.append ((exp.names[i].name, ta))
        return type_of (exp.body, (type_rib, tenv), subst)
    elif exp.is_a ('function'):
        type_rib = []
        arg_types = []
        for formal in exp.formals:
            t = optional_type (formal)
            arg_types.append (t)
            type_rib.append ((formal.name, t))
        body_type, subst = type_of (exp.body, (type_rib, tenv), subst)
        print 'function', (body_type, tuple (arg_types))
        return (body_type, tuple (arg_types)), subst
    elif exp.is_a ('application'):
        result_type = exp.serial # new type variable
        rator_type, subst = type_of (exp.rator, tenv, subst)
        n = len (exp.rands)
        arg_types = []
        for i in range (n):
            ta, subst = type_of (exp.rands[i], tenv, subst)
            arg_types.append (ta)
        #import pdb; pdb.set_trace()
        subst = unify (rator_type, (result_type, tuple(arg_types)), subst, tenv, exp)
        return result_type, subst
    elif exp.is_a ('fix'):
        n = len (exp.inits)
        type_rib = []
        init_types = []
        # build temp tenv for typing the inits
        for i in range (n):
            # for each function
            init = exp.inits[i]
            name = exp.names[i].name
            # new type var for each init
            type_rib.append ((name, init.serial))
        temp_tenv = (type_rib, tenv)
        # type each init in temp_tenv
        init_types = []
        for i in range (n):
            init = exp.inits[i]
            ti, subst = type_of (init, temp_tenv, subst)
            subst = unify (ti, init.serial, subst, temp_tenv, init)
            init_types.append (ti)
        # now extend the environment with type schemes instead
        type_rib = []
        for i in range (n):
            name = exp.names[i].name
            tsi = build_type_scheme (init_types[i], tenv, subst)
            #import pdb; pdb.set_trace()
            type_rib.append ((name, tsi))
        poly_tenv = (type_rib, tenv)
        # and type the body in that tenv
        return type_of (exp.body, poly_tenv, subst)
    elif exp.is_a ('sequence'):
        for sub in exp.subs[:-1]:
            # everything but the last, type it as don't-care
            ti, subst = type_of (sub, tenv, subst)
        return type_of (exp.subs[-1], tenv, subst)
    elif exp.is_a ('get'):
        result_type = exp.serial # new type variable
        ob_type, subst = type_of (exp.ob, tenv, subst)
        t1 = attr_type (ob_type, exp.name)
        subst = unify (t1, result_type, subst, tenv, exp)
        return t1, subst
    elif exp.is_a ('set'):
        ob_type, subst = type_of (exp.ob, tenv, subst)
        tval, subst = type_of (exp.val, tenv, subst)
        t1 = attr_type (ob_type, exp.name)
        subst = unify (t1, tval, subst, tenv, exp)
        result_type = exp.serial # new type variable
        subst = unify (result_type, 'undefined', subst, tenv, exp)
        return result_type, subst
    elif exp.is_a ('make_tuple'):
        # XXX examine the types available - think about complicated
        #     cases like 'closure'...
        result_type = exp.type
        for arg in exp.args:
            # this is kindof a 'dead' type judgement?
            ta, subst = type_of (arg, tenv, subst)
        return result_type, subst
    else:
        raise ValueError (exp)

def instantiate_type (type, tvar, fresh_tvar):
    def f (t):
        if is_a (t, int):
            if t == tvar:
                return fresh_tvar
            else:
                return t
        elif is_a (t, str):
            return t
        elif is_a (t, tuple):
            result_type, arg_types = t
            return (f (result_type), tuple ([f(x) for x in arg_types]))
        else:
            raise ValueError
    return f (type)

def fresh_tvar():
    return tree.serial.next()

def instantiate_type_scheme (tscheme):
    gens = tscheme.gens
    body = tscheme.type
    for gen in gens:
        body = instantiate_type (body, gen, fresh_tvar())
    return body

def apply_tenv (tenv, name):
    # which has precedence?  classes or variables?
    while tenv:
        rib, tenv = tenv
        for var, type in rib:
            if var == name:
                # is this a type scheme?
                if is_a (type, forall):
                    result = instantiate_type_scheme (type)
                    print 'type of %r %r instantiated as %r' % (name, type, result)
                    return result
                else:
                    return type
    raise ValueError (name)

# XXX to be redone - holdover from manual typing.
def add_typechecks (root):
    for node in root:
        if node.one_of ('let_splat', 'fix'):
            for i in range (len (node.names)):
                name = node.names[i]
                init = node.inits[i]
                print name, name.type, init.type
                if name.type and not init.type:
                    init.typecheck = name.type
        elif node.is_a ('cexp'):
            result_type, arg_types = node.type_sig
            for i in range (len (arg_types)):
                if arg_types[i] and not node.args[i].type:
                    node.args[i].typecheck = arg_types[i]
