# -*- Mode: Python -*-

import lambda_tree as tree
from pprint import pprint as pp

is_a = isinstance

# yeah, it's a global.  get over it.
#  XXX at some point, move some of these funs into a class, and fix these.
classes = {}
# probably merge these
datatypes = {}
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

class type_variable:
    def __init__ (self, num=None):
        if num is None:
            self.num = tree.serial.next()
        else:
            self.num = num
        self.val = None

    def __cmp__ (self, other):
        if is_a (other, type_variable):
            return cmp (self.num, other.num)
        else:
            return -1

    def __hash__ (self):
        # XXX temporary, remove when subst is no longer an assoc list
        return hash (self.num)

    def __repr__ (self):
        if self.val is None:
            return '<t%d>' % (self.num,)
        else:
            return '<t%d %r>' % (self.num, self.val)

def apply1 (t0, tvar, t1):
    # substitute t1 for tvar in t0
    if is_a (t0, str):
        # plain type
        return t0
    elif is_a (t0, type_variable):
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
    elif is_a (t0, array):
        return array (apply1 (t0.type, tvar, t1))
    else:
        # a function
        result_type, arg_types = t0
        return (
            apply1 (result_type, tvar, t1),
            tuple ([ apply1 (x, tvar, t1) for x in arg_types ])
            )

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
    elif is_a (t, record):
        fields = []
        for fname, ftype in t.fields:
            fields.append ((fname, apply_subst_to_type (ftype)))
        return record (t.name, fields)
    elif is_a (t, array):
        return array (apply_subst_to_type (t.type))
    elif is_a (t, union):
        alts = []
        for fname, ftype in t.alts:
            alts.append ((fname, apply_subst_to_type (ftype)))
        return union (t.name, alts)
    else:
        raise ValueError

the_subst = {}

def extend_subst (tvar, type):
    tvar.val = type
    if the_subst.has_key (tvar.num):
        raise KeyError ("type var already bound!")
    else:
        the_subst[tvar.num] = type

def lookup_subst (tvar):
    t = tvar
    while is_a (t, type_variable):
        t = t.val
    # path compression
    tvar.val = t
    return t

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

class union:
    def __init__ (self, name, alts):
        self.name = name
        self.alts = alts

    def __cmp__ (self, other):
        if is_a (other, union):
            # assumes unique by name...
            return cmp (self.name, other.name)
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
        formals = [tree.vardef (selector, stype)]
        arg = tree.varref (selector)
        body = tree.make_tuple (self.name, index, [arg])
        name = '%s/%s' % (self.name, selector)
        return name, tree.function (self.name, formals, body)

    def get_datatype_constructors (self):
        constructors = []
        for sname, stype in self.alts:
            constructors.append (('%s/%s' % (self.name, sname), self))
        return constructors

    def __repr__ (self):
        alts = ' '.join ([('%s:%s' % tuple(x)) for x in self.alts])
        return '{union %s %s}' % (self.name, alts)

# reconcile types t1 and t2 from <exp> given <subst>
def unify (t1, t2, tenv, exp):
    t1 = apply_subst_to_type (t1)
    t2 = apply_subst_to_type (t2)
    #print 'unify', t1, '  ====  ', t2
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
        for i in range (len (args1)):
            unify (args1[i], args2[i], tenv, exp)
        # extend with result type
        return unify (r1, r2, tenv, exp)
    elif is_a (t1, record) and is_a (t2, record):
        if t1.name == t2.name:
            for i in range (len (t1.fields)):
                unify (t1.fields[i][1], t2.fields[i][1], tenv, exp)
        else:
            raise TypeError ((t1, t2, exp))
    elif is_a (t1, array) and is_a (t2, array):
        return unify (t1.type, t2.type, tenv, exp)
    else:
        raise TypeError ((t1, t2, exp))

def occurs_in_type (tvar, t):
    # does <tvar> does occur in <t>?
    if is_a (t, str):
        # type
        return False
    elif is_a (t, type_variable):
        # type variable
        return t == tvar
    elif is_a (t, record):
        for fname, ftype in t.fields:
            if occurs_in_type (tvar, ftype):
                return True
        else:
            return False
    elif is_a (t, array):
        return occurs_in_type (tvar, t.type)
    elif is_a (t, union):
        for sname, stype in t.alts:
            if occurs_in_type (tvar, stype):
                return True
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
#   treat it as a type variable.
def optional_type (exp, tenv):
    if exp.type:
        if classes.has_key (exp.type):
            # XXX I think this is a hack
            return apply_tenv (tenv, exp.type)
        else:
            return exp.type
    else:
        return type_variable (exp.serial)

#
# I think there need to be *two* type environments... maybe this is the two sigmas in Appel?
#  1) a local environment mapping variable names to types
#  2) a global type environment describing builtin types and user datatypes
# what does having a single environment get us?  the ability to override a type constructor?
# Note that poly instantiation is built into 'varref', though...
#

def initial_type_environment():
    base_types = [
        # XXX think about these...
        ('int', 'int'),
        ('string', 'string'),
        ('bool', 'bool'),
        ('nil', 'nil'),
        ('undefined', 'undefined'),
        # think about this...
        ('%%vector-literal', forall ([10000], array (10000)))
        ]
    constructors = []
    for name, c in classes.items():
        cname = c.name
        fields = []
        meta = []
        n = len (c.fields)
        for fname, ftype in c.fields:
            if ftype is None:
                tvar = type_variable()
                meta.append (tvar)
                ftype = tvar
            fields.append ((fname, ftype))
        con = record (cname, fields)
        if len (meta):
            # assuming here that we don't need to call build_type_scheme()
            #  since the type environment and subst are completely empty here.
            con = forall (meta, con)
        constructors.append ((cname, con))
    # datatype constructors
    for name, dt in datatypes.iteritems():
        constructors.extend (dt.get_datatype_constructors())
        constructors.append ((name, dt))
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
    t_exp = type_of (exp, tenv)
    pp (the_subst)
    for node in exp:
        # XXX all of this seems really clumsy, I may
        #     be making this harder than it is...
        if node.type:
            node.type = apply_subst_to_type (node.type)
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
                name.type = apply_subst_to_type (type_variable (name.serial))

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

def build_type_scheme (type, tenv):
    
    gens = []

    def list_generic_tvars (t):
        t = apply_subst_to_type (t)
        if is_a (t, type_variable):
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
        elif is_a (t, array):
            list_generic_tvars (t.type)
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

def wildcard (t, exp, tenv):
    if t == '?':
        return type_variable (exp.serial)
    elif classes.has_key (t):
        # XXX I think this is a hack
        return apply_tenv (tenv, t)
    else:
        return t

# a wrapper for type_of helpful when debugging
def type_of (exp, tenv):
    t_exp = _type_of (exp, tenv)
    # this may actually be necessary...
    t_exp = apply_subst_to_type (t_exp)
    exp.type = t_exp
    return t_exp

def _type_of (exp, tenv):
    if exp.is_a ('literal'):
        return exp.type
    elif exp.is_a ('cexp'):
        result_type, arg_types = exp.type_sig
        result_type = wildcard (result_type, exp, tenv)
        for i in range (len (arg_types)):
            arg = exp.args[i]
            arg_type = arg_types[i]
            ta = type_of (arg, tenv)
            arg_type = wildcard (arg_type, arg, tenv)
            unify (ta, arg_type, tenv, arg)
        return result_type
    elif exp.is_a ('varref'):
        result = apply_tenv (tenv, exp.name)
        return result
    elif exp.is_a ('varset'):
        # XXX implement the no-generalize rule for vars that are assigned.
        t1 = apply_tenv (tenv, exp.name)
        t2 = type_of (exp.value, tenv)
        unify (t1, t2, tenv, exp.value)
        return 'undefined'
    elif exp.is_a ('conditional'):
        t1 = type_of (exp.test_exp, tenv)
        unify (t1, 'bool', tenv, exp.test_exp)
        t2 = type_of (exp.then_exp, tenv)
        t3 = type_of (exp.else_exp, tenv)
        unify (t2, t3, tenv, exp)
        return t2
    elif exp.one_of ('let_splat'):
        n = len (exp.inits)
        #type_rib = []
        for i in range (n):
            ta = type_of (exp.inits[i], tenv)
            #type_rib.append ((exp.names[i].name, ta))
            tenv = ([(exp.names[i].name, ta)], tenv)
        return type_of (exp.body, tenv)
    elif exp.is_a ('function'):
        type_rib = []
        arg_types = []
        for formal in exp.formals:
            t = optional_type (formal, tenv)
            arg_types.append (t)
            type_rib.append ((formal.name, t))
        body_type = type_of (exp.body, (type_rib, tenv))
        return (body_type, tuple (arg_types))
    elif exp.is_a ('application'):
        result_type = type_variable (exp.serial) # new type variable
        rator_type = type_of (exp.rator, tenv)
        n = len (exp.rands)
        # I think these exceptions should maybe be done as primapp instead?
        #  like this?: (primapp (record list) ...)
        if is_a (rator_type, record):
            # a constructor
            arg_types = []
            assert (n == len (rator_type.fields))
            for i in range (n):
                ta = type_of (exp.rands[i], tenv)
                fname, tf = rator_type.fields[i]
                if is_a (tf, str):
                    if tf == rator_type.name:
                        # hack to grok recursive data types
                        tf = rator_type
                    else:
                        tf = apply_tenv (tenv, tf)
                unify (ta, tf, tenv, exp)
            return rator_type
        elif is_a (rator_type, array):
            for i in range (n):
                ta = type_of (exp.rands[i], tenv)
                unify (ta, rator_type.type, tenv, exp)
            return rator_type
        elif is_a (rator_type, union):
            # bit of a hack here, transform.py should assert() that
            #  the name is is a string and not an expression...
            # XXX assumes exp.rator is a varref
            [base, selector] = exp.rator.name.split ('/')
            # which type is selected?
            tf, index = rator_type.get_field_type (selector)
            tf = apply_tenv (tenv, tf)
            # XXX lookup type
            tv = type_of (exp.rands[0], tenv)
            unify (tf, tv, tenv, exp)
            # XXX hack: frob this application.
            # XXX this should be a method of <union>
            return rator_type
        else:
            # normal application
            arg_types = []
            for i in range (n):
                ta = type_of (exp.rands[i], tenv)
                arg_types.append (ta)
            unify (rator_type, (result_type, tuple(arg_types)), tenv, exp)
            return result_type
    elif exp.is_a ('fix'):
        n = len (exp.inits)
        type_rib = []
        init_types = []
        init_tvars = []
        # build temp tenv for typing the inits
        for i in range (n):
            # for each function
            init = exp.inits[i]
            name = exp.names[i].name
            # new type var for each init
            tvi = type_variable (init.serial)
            init_tvars.append (tvi)
            type_rib.append ((name, tvi))
        temp_tenv = (type_rib, tenv)
        # type each init in temp_tenv
        init_types = []
        for i in range (n):
            init = exp.inits[i]
            ti = type_of (init, temp_tenv)
            unify (ti, init_tvars[i], temp_tenv, init)
            init_types.append (ti)
        # now extend the environment with type schemes instead
        type_rib = []
        for i in range (n):
            name = exp.names[i].name
            tsi = build_type_scheme (init_types[i], tenv)
            #import pdb; pdb.set_trace()
            type_rib.append ((name, tsi))
        poly_tenv = (type_rib, tenv)
        # and type the body in that tenv
        return type_of (exp.body, poly_tenv)
    elif exp.is_a ('sequence'):
        for sub in exp.subs[:-1]:
            # everything but the last, type it as don't-care
            ti = type_of (sub, tenv)
        return type_of (exp.subs[-1], tenv)
    elif exp.is_a ('get'):
        result_type = type_variable (exp.serial) # new type variable
        ob_type = type_of (exp.ob, tenv)
        if is_a (ob_type, record):
            return ob_type.get_field_type (exp.name)
        else:
            raise TypeError ("unable to find class for <get> expression")
    elif exp.is_a ('set'):
        raise NotImplementedError
        ob_type = type_of (exp.ob, tenv)
        tval = type_of (exp.val, tenv)
        t1 = attr_type (ob_type, exp.name)
        unify (t1, tval, tenv, exp)
        result_type = type_variable (exp.serial) # new type variable
        unify (result_type, 'undefined', tenv, exp)
        return result_type
    elif exp.is_a ('make_tuple'):
        # XXX examine the types available - think about complicated
        #     cases like 'closure'...
        result_type = exp.type
        for arg in exp.args:
            # this is kindof a 'dead' type judgement?
            ta = type_of (arg, tenv)
        return result_type
    elif exp.is_a ('primapp'):
        if exp.name == '%%vector-literal':
            n = len (exp.args)
            if n == 0:
                raise ValueError ("don't know how to type a zero-length vector yet")
            else:
                ta0 = type_of (exp.args[0], tenv)
                for i in range (1, n):
                    ta = type_of (exp.args[i], tenv)
                    unify (ta0, ta, tenv, exp)
            return array (ta0)
        elif exp.name == '%%array-ref':
            # (%%array-ref <array> <index>)
            base = exp.args[0]
            index = exp.args[1]
            bt = type_of (base, tenv)
            it = type_of (index, tenv)
            # we don't know the item type yet...
            item_type = type_variable()
            ta = array (item_type)
            unify (bt, ta, tenv, exp)
            unify (it, 'int', tenv, exp)
            return item_type
        elif exp.name == '%%array-set':
            # (%%array-set <array> <index> <value>)
            base, index, value = exp.args
            bt = type_of (base, tenv)
            it = type_of (index, tenv)
            vt = type_of (value, tenv)
            ta = array (vt)
            unify (bt, ta, tenv, exp)
            unify (it, 'int', tenv, exp)
            return 'undefined'
        else:
            raise ValueError ("can't type unknown primop %s" % (exp.name,))
    elif exp.is_a ('typecase'):
        vt = type_of (exp.varname, tenv)
        if not is_a (vt, union):
            raise ValueError ("typecase expects a union/sum type")
        else:
            # verify that all of the variants are accounted for...
            names0 = set ([sname for sname, stype in vt.alts])
            names1 = set (exp.alt_names)
            if names0 != names1:
                raise ValueError ("set of union/sum type alternatives does not match in typecase")
            else:
                n = len (exp.alt_names)
                texp = type_variable (exp.serial)
                new_alts = [None] * n
                for i in range (n):
                    sname = exp.alt_names[i]
                    body = exp.alts[i]
                    type, index = vt.get_field_type (sname)
                    type_rib = [(exp.varname.name, type)]
                    tenv2 = (type_rib, tenv)
                    bt = type_of (body, tenv2)
                    unify (texp, bt, tenv2, exp)
                    # sort 'em
                    new_alts[index] = exp.alts[i]
                exp.alts = new_alts
                exp.subs = [exp.varname] + exp.alts
            return texp
    else:
        raise ValueError (exp)

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
        elif is_a (t, record):
            return record (t.name, [ (fname, f (ftype)) for (fname, ftype) in t.fields ])
        elif is_a (t, array):
            return array (f (t.type))
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
    while tenv:
        rib, tenv = tenv
        # walk the rib backwards for the sake of let*
        for i in range (len(rib)-1, -1, -1):
            var, type = rib[i]
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
