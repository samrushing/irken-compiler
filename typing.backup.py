# -*- Mode: Scheme -*-

is_a = isinstance

# yeah, it's a global.  get over it.
classes = {}

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
        if tvar == t0:
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
        return attr_type (
            apply_subst_to_type (t.base_type, subst),
            t.name
            )
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
def lookup_class_attribute_type (name, field):
    c = classes[name]
    for fname, type in c.fields:
        if fname == field:
            return type
    raise AttributeError ("class %r has no %r attribute" % (name, field))

# reconcile types t1 and t2 from <exp> given <subst>
def unify (t1, t2, subst, exp):
    t1 = apply_subst_to_type (t1, subst)
    t2 = apply_subst_to_type (t2, subst)
    # can this wildcard thing work?
    if t1 == t2 or t1 == '?' or t2 == '?':
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
            subst = unify (args1[i], args2[i], subst, exp)
        # extend with result type
        return unify (r1, r2, subst, exp)
    elif is_a (t2, attr_type):
        return unify_attr_types (t1, t2, subst, exp)
    elif is_a (t1, attr_type):
        return unify_attr_types (t2, t1, subst, exp)
    else:
        raise TypeError ((t1, t2, exp))

def unify_attr_types (t1, t2, subst, exp):
    occurrence_check (t2, t1, exp)
    if is_a (t1, str):
        # we know which class it is, unify this attribute
        t = lookup_class_attribute_type (t2.base_type, t2.name)
        if t is not None:
            subst = unify (t1, t, subst, exp)
            return extend_subst (subst, t2, t)
        else:
            return extend_subst (subst, t2, t1)
    else:
        # we're unifying two attr types
        return extend_subst (subst, t1, t2)

def occurrence_check (tvar, t, exp):
    # ensure that <tvar> does not occur in <t>
    if is_a (t, str):
        # type
        pass
    elif is_a (t, int):
        # type variable
        if t == tvar:
            raise TypeError ((tvar, t, exp))
    elif is_a (t, attr_type):
        occurrence_check (tvar, t.base_type, exp)
    else:
        # function
        result_type, arg_types = t
        for arg in arg_types:
            occurrence_check (tvar, arg, exp)
        occurrence_check (tvar, result_type, exp)

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

def type_program (exp):
    class_rib = build_constructor_types()
    t_exp, subst = type_of (exp, (class_rib, None), None)
    map = subst_as_dict (subst)
    import pdb; pdb.set_trace()
    for node in exp:
        # XXX all of this seems really clumsy, I may
        #     be making this harder than it is...
        if is_a (node.type, int):
            node.type = map[node.type]
        elif map.has_key (node.serial):
            node.type = map[node.serial]
        if node.binds():
            names = node.get_names()
            for name in names:
                if map.has_key (name.serial):
                    name.type = map[name.serial]

# a wrapper for type_of helpful when debugging
def type_of (exp, tenv, subst):
    t_exp, subst = _type_of (exp, tenv, subst)
    #print t_exp, ' == type_of (', exp, tenv, subst, ')'
    exp.type = apply_subst_to_type (t_exp, subst)
    return t_exp, subst

def _type_of (exp, tenv, subst):
    if exp.is_a ('literal'):
        return exp.type, subst
    elif exp.is_a ('cexp'):
        result_type, arg_types = exp.type_sig
        for i in range (len (arg_types)):
            ta, subst = type_of (exp.args[i], tenv, subst)
            subst = unify (ta, arg_types[i], subst, exp.args[i])
        return result_type, subst
    elif exp.is_a ('varref'):
        return apply_tenv (tenv, exp.name), subst
    elif exp.is_a ('varset'):
        t1 = apply_tenv (tenv, exp.name)
        t2, subst = type_of (exp.value, tenv, subst)
        subst = unify (t1, t2, subst, exp.value)
        return 'undefined', subst
    elif exp.is_a ('conditional'):
        t1, subst = type_of (exp.test_exp, tenv, subst)
        subst = unify (t1, 'bool', subst, exp.test_exp)
        t2, subst = type_of (exp.then_exp, tenv, subst)
        t3, subst = type_of (exp.else_exp, tenv, subst)
        subst = unify (t2, t3, subst, exp)
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
        return (body_type, tuple (arg_types)), subst
    elif exp.is_a ('application'):
        result_type = exp.serial # new type variable
        rator_type, subst = type_of (exp.rator, tenv, subst)
        n = len (exp.rands)
        arg_types = []
        for i in range (n):
            ta, subst = type_of (exp.rands[i], tenv, subst)
            arg_types.append (ta)
        subst = unify (rator_type, (result_type, tuple(arg_types)), subst, exp)
        return result_type, subst
    elif exp.is_a ('fix'):
        # 1) pull out optional type signatures from each function
        # 2) extend tenv with the function names
        # 3) for each fun, call type_of() on its body with a tenv extended with the formals.
        # 4) finally, call type_of() on the fix body.
        n = len (exp.inits)
        type_rib = []
        fun_types = []
        for i in range (n):
            # for each function
            init = exp.inits[i]
            name = exp.names[i].name
            if init.is_a ('function'):
                result_type = optional_type (init)
                nargs = len (init.formals)
                args = init.formals
                arg_types = tuple ([optional_type (x) for x in args])
                sig = (result_type, arg_types)
                fun_types.append (sig)
                type_rib.append ((name, sig))
            else:
                raise ValueError ("not yet implemented")
        # plug all the *letrec* bindings into the type environment
        tenv = (type_rib, tenv)
        for i in range (n):
            # unify each procedure's type
            init = exp.inits[i]
            # for each fun, extend tenv with its formals/types
            fun_type_rib = []
            for j in range (len (init.formals)):
                fun_type_rib.append ((init.formals[j].name, fun_types[i][1][j]))
            p_tenv = (fun_type_rib, tenv)
            pi_type, subst = type_of (init.body, p_tenv, subst)
            pi_result_type, pi_arg_types = fun_types[i]
            subst = unify (pi_type, pi_result_type, subst, init.body)
        return type_of (exp.body, tenv, subst)
    elif exp.is_a ('sequence'):
        for sub in exp.subs[:-1]:
            # everything but the last, type it as don't-care
            ti, subst = type_of (sub, tenv, subst)
        return type_of (exp.subs[-1], tenv, subst)
    elif exp.is_a ('get'):
        result_type = exp.serial # new type variable
        ob_type, subst = type_of (exp.ob, tenv, subst)
        t1 = attr_type (ob_type, exp.name)
        subst = unify (t1, result_type, subst, exp)
        return t1, subst
    elif exp.is_a ('set'):
        ob_type, subst = type_of (exp.ob, tenv, subst)
        tval, subst = type_of (exp.val, tenv, subst)
        t1 = attr_type (ob_type, exp.name)
        subst = unify (t1, tval, subst, exp)
        exp.type = 'undefined'
        return 'undefined', subst
    else:
        raise ValueError (exp)

def apply_tenv (tenv, name):
    # which has precedence?  classes or variables?
    while tenv:
        rib, tenv = tenv
        for var, type in rib:
            if var == name:
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
