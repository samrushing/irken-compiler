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

# XXX need to switch from <subst> as a lookup table to metavariables as
#   a pair of <int>, <current-value>.

# inference.
#  we're solving a series of equations.
# see eoplv3:7.4

# types of 'type':
# a string means a base type - 'int', 'bool', etc...
# an integer means a type variable.  think of '34' as 't34'.
# a tuple means a function type: (int, (bool, string)) means a function
#    taking (bool, string) args and returning an int.
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
    elif is_a (t0, record):
        fields = []
        for fname, ftype in t0.fields:
            fields.append ((fname, apply1 (ftype, tvar, t1)))
        return record (t0.name, fields)
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
    elif is_a (t, tuple):
        # a function
        result_type, arg_types = t
        return (
            apply_subst_to_type (result_type, subst),
            tuple ([ apply_subst_to_type (x, subst) for x in arg_types ])
            )
    elif is_a (t, record):
        # constructor
        fields = []
        for fname, ftype in t.fields:
            fields.append ((fname, apply_subst_to_type (ftype, subst)))
        return record (t.name, fields)
    else:
        raise ValueError

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

class record:
    def __init__ (self, name, fields):
        self.name = name
        # fields = [(<name>, <type>), ...]
        self.fields = fields

    def get_field_type (self, name):
        for fname, ftype in self.fields:
            if name == fname:
                if ftype == self.name:
                    # recursive data type
                    return self
                else:
                    return ftype

    def __cmp__ (self, other):
        if is_a (other, record):
            return cmp ((self.name, self.fields), (other.name, other.fields))
        else:
            return -1

    def __repr__ (self):
        return '{%s %s}' % (self.name, ' '.join (['%s:%s' % x for x in self.fields]))

# reconcile types t1 and t2 from <exp> given <subst>
def unify (t1, t2, subst, tenv, exp):
    t1 = apply_subst_to_type (t1, subst)
    t2 = apply_subst_to_type (t2, subst)
    #print 'unify', t1, '  ====  ', t2
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
    elif is_a (t1, record) and is_a (t2, record):
        if t1.name == t2.name:
            for i in range (len (t1.fields)):
                subst = unify (t1.fields[i][1], t2.fields[i][1], subst, tenv, exp)
            return subst
        else:
            raise TypeError ((t1, t2, exp))
    else:
        raise TypeError ((t1, t2, exp))

def occurs_in_type (tvar, t):
    # does <tvar> does occur in <t>?
    if is_a (t, str):
        # type
        return False
    elif is_a (t, int):
        # type variable
        return t == tvar
    elif is_a (t, record):
        for fname, ftype in t.fields:
            if occurs_in_type (tvar, ftype):
                return True
        else:
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
            if not is_a (type, forall) and occurs_in_type (tvar, type):
                return True
    return False

# if a node has user-supplied type, use it.  otherwise
#   treat it as a type variable. (exp.serial is an int)
def optional_type (exp, tenv):
    if exp.type:
        if classes.has_key (exp.type):
            # XXX I think this is a hack
            return apply_tenv (tenv, exp.type)
        else:
            return exp.type
    else:
        return exp.serial

def subst_as_dict (subst):
    r = {}
    while subst:
        (k, v), subst = subst
        r[k] = v
    return r

def initial_type_environment():
    base_types = [
        # XXX think about these...
        ('int', 'int'),
        ('string', 'string'),
        ('bool', 'bool'),
        ('nil', 'nil'),
        ('undefined', 'undefined'),
        ]
    constructors = []
    for name, c in classes.items():
        cname = c.name
        fields = []
        meta = []
        n = len (c.fields)
        for fname, ftype in c.fields:
            if ftype is None:
                # note that since we're not building a type *scheme* here,
                #  we're going to get monomorphic record types.
                tvar = fresh_tvar()
                meta.append (tvar)
                ftype = tvar
            fields.append ((fname, ftype))
        con = record (cname, fields)
        if len (meta):
            # assuming here that we don't need to call build_type_scheme()
            #  since the type environment and subst are completely empty here.
            con = forall (meta, con)
        constructors.append ((cname, con))
    return base_types + constructors

def lookup_method (node):
    base_ob = node.subs[0]
    # XXX this might make assumptions that break (x.y.z arg0 arg1 ...)
    base_type = base_ob.type
    name = node.params
    c = classes[base_type]
    return c.lookup_method (name)

def type_program (exp):
    tenv = (initial_type_environment(), None)
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
        t = apply_subst_to_type (t, subst)
        if is_a (t, int):
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
        elif is_a (t, record):
            for fname, ftype in t.fields:
                list_generic_tvars (ftype)
        else:
            raise ValueError

    #import pdb; pdb.set_trace()
    list_generic_tvars (type)
    if not gens:
        return type
    else:
        r = forall (gens, type)
        if verbose:
            print 'built type scheme', r        
        return r

def subst_repr (subst):
    result = []
    while subst:
        (k, v), subst = subst
        result.append ('%r:%r' % (k, v))
    return '{' + (', '.join (result)) + '}'

def wildcard (t, exp, tenv):
    if t == '?':
        return exp.serial
    elif classes.has_key (t):
        # XXX I think this is a hack
        return apply_tenv (tenv, t)
    else:
        return t

# a wrapper for type_of helpful when debugging
def type_of (exp, tenv, subst):
    t_exp, subst = _type_of (exp, tenv, subst)
    # this may actually be necessary...
    t_exp = apply_subst_to_type (t_exp, subst)
    exp.type = t_exp
    return t_exp, subst

def _type_of (exp, tenv, subst):
    if exp.is_a ('literal'):
        return exp.type, subst
    elif exp.is_a ('cexp'):
        result_type, arg_types = exp.type_sig
        result_type = wildcard (result_type, exp, tenv)
        for i in range (len (arg_types)):
            arg = exp.args[i]
            arg_type = arg_types[i]
            ta, subst = type_of (arg, tenv, subst)
            arg_type = wildcard (arg_type, arg, tenv)
            subst = unify (ta, arg_type, subst, tenv, arg)
        return result_type, subst
    elif exp.is_a ('varref'):
        return apply_tenv (tenv, exp.name), subst
    elif exp.is_a ('varset'):
        # XXX implement the no-generalize rule for vars that are assigned.
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
            t = optional_type (formal, tenv)
            arg_types.append (t)
            type_rib.append ((formal.name, t))
        body_type, subst = type_of (exp.body, (type_rib, tenv), subst)
        return (body_type, tuple (arg_types)), subst
    elif exp.is_a ('application'):
        result_type = exp.serial # new type variable
        rator_type, subst = type_of (exp.rator, tenv, subst)
        if is_a (rator_type, record):
            # a constructor
            n = len (exp.rands)
            arg_types = []
            assert (n == len (rator_type.fields))
            for i in range (n):
                ta, subst = type_of (exp.rands[i], tenv, subst)
                fname, tf = rator_type.fields[i]
                if is_a (tf, str):
                    if tf == rator_type.name:
                        # hack to grok recursive data types
                        tf = rator_type
                    else:
                        tf = apply_tenv (tenv, tf)
                subst = unify (ta, tf, subst, tenv, exp)
            return rator_type, subst
        else:
            # normal application
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
        if is_a (ob_type, record):
            #t_field = apply_tenv (tenv, ob_type.get_field_type (exp.name))
            t_field = ob_type.get_field_type (exp.name)
            return t_field, subst
        else:
            raise TypeError ("unable to find class for <get> expression")
    elif exp.is_a ('set'):
        raise NotImplementedError
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
        elif is_a (t, record):
            return record (t.name, [ (fname, f (ftype)) for (fname, ftype) in t.fields ])
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
    while tenv:
        rib, tenv = tenv
        for var, type in rib:
            if var == name:
                # is this a type scheme?
                if is_a (type, forall):
                    result = instantiate_type_scheme (type)
                    if verbose:
                        print 'type of %r %r instantiated as %r' % (name, type, result)
                    return result
                else:
                    return type
    raise ValueError (name)
