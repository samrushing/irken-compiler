# -*- Mode:Python; coding: utf-8 -*-

# This is an implementation of the constraint-based inference algorithm described by
#  Pottier and Rémy in "Advanced Topics in Types and Programming Languages", chapter 10:
#  "The Essence of ML Type Inference".
#
# Another great reference is a somewhat simplified presentation of the
#  same material, but (thankfully) with some context, by Pottier: "A
#  modern eye on ML type inference - Old techniques and recent
#  developments", available from his home page:
#  http://cristal.inria.fr/~fpottier/

#
# For now, I have ignored subtyping - by having '<' mean '='.  This should
#   be a pretty easy thing to change, once I'm ready to wrap my head around it.
#

import nodes
import graph
import sys
import pdb
trace = pdb.set_trace
is_a = isinstance

# the simply typed lambda calculus:
#   e ::= x | λx.e | e e
# 
# expressions:
# x    : <varref>
# λx.e : <function>
# e e  : <application>

# types
# t ::= a | (arrow t t)
# (where a = <tvar>)

# constraints:
# C ::= (equals t t) | (and C C) | (exists a C)

# types
from itypes import *

# constraints

def constraint_repr (kind, args):
    if args:
        return '[%s %s]' % (kind, ' '.join ([repr(x) for x in args]))
    else:
        return '%s' % (kind,)

class constraint:
    kind = 'abstract'
    args = ()

    def __repr__ (self):
        return constraint_repr (self.kind, self.args)

class c_true (constraint):
    kind = 'true'
    args = ()

class c_equals (constraint):
    kind = 'equals'
    def __init__ (self, *args):
        if None in args:
            trace()
        self.args = args
    def __repr__ (self):
        return constraint_repr ('=', self.args)

class c_and (constraint):
    kind = 'and'
    def __init__ (self, c0, c1):
        self.args = (c0, c1)
        
class c_exists (constraint):
    kind = 'exists'
    def __init__ (self, vars, sub):
        self.args = (vars, sub)
        self.vars = vars
        self.sub = sub

class c_is (constraint):
    # <x> has type <t> iff <t> is an instance of the type scheme associated with <x>
    kind = 'is'
    def __init__ (self, x, t):
        self.args = (x, t)
        self.x = x
        self.t = t

class c_let (constraint):
    kind = 'let'
    def __init__ (self, names, vars, constraint, body):
        self.args = (names, vars, constraint, body)
        self.names = names
        self.vars = vars
        self.constraint = constraint
        self.body = body

class c_forall (constraint):
    kind = 'forall'
    def __init__ (self, vars, constraint):
        self.args = (vars, constraint)
        self.vars = vars
        self.constraint = constraint

def flatten_conj (c):
    l = []
    def p (c):
        if is_a (c.args[0], c_and):
            p (c.args[0])
        else:
            l.append (c.args[0])
        l.append (c.args[1])
    if is_a (c, c_and):
        p (c)
        return l
    else:
        return [c]

def pprint_constraint (c):
    W = sys.stdout.write
    def pp (c, d):
        W ('\n' + ('  ' * d))
        if is_a (c, c_let):
            W ('let %s' % ' '.join (['%s:%r' % (c.names[i].name,c.vars[i]) for i in range (len (c.names))]))
            if not is_a (c.constraint, c_true):
                pp (c.constraint, d+1)
            W ('\n' + ('  ' * d))
            W ('in')
            pp (c.body, d+1)
        elif is_a (c, c_forall):
            W ('forall (%s)' % (','.join ([repr(v) for v in c.vars])))
            pp (c.constraint, d+1)
        elif is_a (c, c_and):
            W ('and')
            for t in flatten_conj (c):
                pp (t, d+1)
        elif is_a (c, c_exists):
            W ('exists (%s)' % (','.join ([repr(v) for v in c.vars])))
            pp (c.sub, d+1)
        else:
            W (repr (c))
    pp (c, 0)
    W ('\n')

def check_constraint (c, top_tv):
    # verify that all tvars are bound correctly in the constraint <c>

    def lookup (v, env):
        count = 0
        while env is not None:
            rib, env = env
            if v in rib:
                count += 1
        if count > 1:
            raise ValueError ("variable bound more than once!")
        elif count == 1:
            return
        else:
            raise UnboundVariable

    def pp (c, env):
        if is_a (c, c_let):
            pp (c.constraint, env)
            pp (c.body, env)
        elif is_a (c, c_forall):
            rib = c.vars
            pp (c.constraint, (rib, env))
        elif is_a (c, c_and):
            for t in flatten_conj (c):
                pp (t, env)
        elif is_a (c, c_exists):
            pp (c.sub, (c.vars, env))
        elif is_a (c, t_predicate):
            for arg in c.args:
                pp (arg, env)
        elif is_a (c, c_is):
            pp (c.t, env)
        elif is_a (c, c_equals):
            for arg in c.args:
                pp (arg, env)
        elif is_a (c, c_true):
            pass
        elif is_a (c, t_var):
            lookup (c, env)
        elif is_a (c, t_base):
            pass
        elif is_a (c, str):
            # row labels
            pass
        else:
            raise ValueError
    pp (c, (set([top_tv]), None))

# stack frames
class frame:
    kind = 'abstract'

class s_empty (frame):
    kind = 'empty'

empty = s_empty()

class s_and (frame):
    def __init__ (self, c):
        self.constraint = c

class s_exists (frame):
    def __init__ (self, vars):
        self.vars = vars

class s_let (frame):
    def __init__ (self, names, types, vars, body, rank):
        if len(names) == 0:
            raise ValueError
        self.names = names
        self.types = types
        self.vars = set (vars)
        self.body = body
        self.rank = rank
        for v in vars:
            v.rank = rank
        self.counter = 0

    def add_vars (self, vars):
        self.vars.update (vars)
        for v in vars:
            v.rank = self.rank
        self.counter += len (vars)

class s_env (frame):
    # after a <let> type scheme has been solved, an <env> frame
    #   binds the schemes to the formals.
    def __init__ (self, names, types, vars, u):
        self.names = names
        self.types = types
        self.vars = set (vars)
        self.u = u

# this is a two-phase algorithm
# 1) constraint generation
# 2) constraint solving

class constraint_generator:

    def __init__ (self, context):
        self.context = context

    def go (self, exp):
        t = t_var()
        c, top_tv = self.gen (exp, t), t
        #check_constraint (c, top_tv)
        return c, top_tv

    def gen (self, exp, t):
        exp.tv = t
        name = 'gen_%s' % exp.kind
        probe = getattr (self, name)
        if probe:
            return probe (exp, t)
        else:
            raise ValueError (exp.kind)
        
    def gen_varref (self, exp, t):
        return c_is (exp.name, t)

    def gen_varset (self, exp, t):
        x = t_var()
        return c_exists (
            (x,),
            c_and (
                c_and (
                    c_is (exp.name, x),
                    self.gen (exp.value, x)
                    ),
                c_equals (t, t_undefined())
                )
            )

    def gen_function (self, exp, t):
        if is_pred (t, 'arrow'):
            # lemma 10.4.7
            rtv, args = t.args[0], list(t.args[1:])
        else:
            rtv, args = t_var(), [t_var() for x in exp.formals]
        if len(args):
            c = c_let (exp.formals, args, c_true(), self.gen (exp.body, rtv))
        else:
            c = self.gen (exp.body, rtv)
        # XXX: in ATTPL, this is a c_supertype relation
        if is_pred (t, 'arrow'):
            return c
        else:
            sub1 = c_equals (t, arrow (rtv, *args))
            return c_exists ([rtv] + args, c_and (c, sub1))

    def gen_application (self, exp, t):
        args = [t_var() for x in exp.rands]
        c = self.gen (exp.rator, arrow (t, *args))
        for i in range (len(exp.rands)):
            c = c_and (c, self.gen (exp.rands[i], args[i]))
        return c_exists (args, c)

    def gen_primapp (self, exp, t):
        args = [t_var() for x in exp.args]
        if exp.name.startswith ('%vcon/'):
            # XXX don't like having to do this here.
            if len(args) == 1:
                sig = args[0]
            else:
                sig = product (*args)
            c = c_is (exp.name, arrow (t, sig))
        else:
            c = c_is (exp.name, arrow (t, *args))
        for i in range (len(exp.args)):
            c = c_and (c, self.gen (exp.args[i], args[i]))
        return c_exists (args, c)

    def gen_cexp (self, exp, t):
        tvars, sig = exp.type_sig
        scheme = instantiate_scheme (c_forall (tvars, sig))
        sig = scheme.constraint
        tvars = scheme.vars
        if is_pred (sig, 'arrow'):
            # result type
            c = c_equals (t, sig.args[0])
            for i in range (len (exp.args)):
                # arg types
                sig_arg = sig.args[i+1]
                if is_pred (sig_arg, 'raw'):
                    # hack: magically hide the 'raw' predicate from the solver
                    sig_arg = sig_arg.args[0]
                c = c_and (c, self.gen (exp.args[i], sig_arg))
            if len(tvars):
                return c_exists (tvars, c)
            else:
                return c
        elif is_a (sig, t_base):
            # plain type?
            return c_equals (t, sig)
        else:
            raise ValueError ("unhandled cexp type")

    def gen_let_splat (self, exp, t):
        r = self.gen (exp.body, t)
        n = len (exp.names)
        for i in range (n-1,-1,-1):
            name = exp.names[i]
            init = exp.inits[i]
            var = t_var()
            r = c_let ([name], [var], c_forall ((var,), self.gen (init, var)), r)
        return r

    def gen_fix (self, exp, t):
        partition = graph.reorder_fix (exp, self.context.scc_graph)
        partition.reverse()
        c0 = self.gen (exp.body, t)
        # XXX deep partitioning magic here
        for part in partition:
            names = [exp.names[i] for i in part]
            funs  = [exp.inits[i] for i in part]
            #print names,
            # one var for each function
            fvars = tuple ([t_var() for x in names])
            c1 = list_to_conj (
                [self.gen (funs[i], fvars[i]) for i in range (len (part))]
                )
            # inner/monomorphic binding
            c1 = c_let (names, fvars, c_true(), c1)
            # outer/polymorphic binding
            c1 = c_let (names, fvars, c_forall (fvars, c1), c0)
            c0 = c1
        #print
        return c0

    def gen_conditional (self, exp, t):
        test_exp = self.gen (exp.test_exp, t_bool())
        then_exp = self.gen (exp.then_exp, t)
        else_exp = self.gen (exp.else_exp, t)
        return c_and (test_exp, c_and (then_exp, else_exp))

    def gen_sequence (self, exp, t):
        n = len (exp.subs)
        tvars = [t_var() for x in range (n-1)]
        c = self.gen (exp.subs[-1], t)
        for i in range (n-1):
            # everything but the last, type it as don't-care
            c = c_and (c, self.gen (exp.subs[i], tvars[i]))
        return c_exists (tvars, c)

    def gen_literal (self, exp, t):
        return c_equals (t, base_types[exp.ltype])

    def gen_constructed (self, exp, t):
        return self.gen (exp.value, t)

    def gen_make_tuple (self, exp, t):
        # XXX don't care about the type of the arg?
        v = t_var()
        return c_exists ((v,), c_and (
                self.gen (exp.subs[0], v),
                c_equals (t, base_types[exp.ttype])
                ))

    def gen_pvcase (self, exp, t):
        # (pvcase <alt_formals> <alt0> <alt1> ...)
        # each <alt> binds a separate set of variables (possibly empty)
        # the last alt binds against either "else" (not yet implemented),
        # or rdefault(abs()).
        alts = exp.alts[:]
        vars = []
        conj = []
        if len(alts) == len (exp.alt_formals):
            # no else clause, a closed sum type
            row = rdefault (abs())
        else:
            # with an else clause, open sum type
            row = t_var()
            vars.append (row)
            conj.append (self.gen (alts[-1], t))
        for i in range (len (exp.alt_formals)):
            alt = alts[i]
            label, type, formals = exp.alt_formals[i]
            # row type extended with this label and its type
            args = [t_var() for x in range (len (formals))]
            vars.extend (args)
            ptype = t_var()
            vars.append (ptype)
            row = rlabel (label, pre(ptype), row)
            if len(formals):
                conj.append (c_let (formals, args, c_true(), self.gen (alt, t)))
            else:
                conj.append (self.gen (alt, t))
        
        conj.append (self.gen (exp.value, rsum (row)))
        return c_exists (vars, list_to_conj (conj))

    def gen_nvcase (self, exp, t):
        # (nvcase <vtype> <val> <alt0> <alt1> ...)
        # like a conditional, but with more branches.
        dt = self.context.datatypes[exp.vtype]
        if len(dt.tvars):
            # it's a type scheme, instantiate it
            scheme = instantiate_scheme (c_forall (dt.tvars, dt.scheme))
            conj = [self.gen (exp.value, scheme.constraint)]
        else:
            conj = [self.gen (exp.value, dt.scheme)]
        for alt in exp.alts:
            if alt is not None:
                conj.append (self.gen (alt, t))
        # this will work even when else_clause is a dummy %%match-error
        conj.append (self.gen (exp.else_clause, t))
        if len(dt.tvars):
            return c_exists (scheme.vars, list_to_conj (conj))
        else:
            return list_to_conj (conj)

class UnboundVariable (Exception):
    pass

class multi:
    # a 'standard' multi-equation of the form A=B=C=T where A,B,C are
    # type variables and T is an optional type.
    def __init__ (self, vars, type):
        self.vars = vars
        self.type = type
        self.rep = self.min_rank()
        for v in self.vars:
            # point them all at the rep var
            if v is not self.rep:
                v.next = self.rep
            v.eq = self
        self.rank = self.rep.rank
        #sys.stderr.write ('(%d)' %(len(vars)))

    def min_rank (self):
        # choose the variable with lowest <rank,id>
        mr = sys.maxint
        mv = None
        for v in self.vars:
            if v.rank < mr:
                mr = v.rank
                mv = v
            elif v.rank == mr:
                if v.id < mv.id:
                    mv = v
        return mv

    def free (self):
        free = set()
        if is_a (self.type, t_predicate):
            for arg in self.type.args:
                if is_a (arg, t_var):
                    free.add (arg)
        return free

    def __repr__ (self):
        vars = list(self.vars)
        vars.sort (lambda a,b: cmp (a.id, b.id))
        r = '='.join (['%r' % v for v in vars])
        if self.type:
            return r + '=%r' % (self.type,)
        else:
            return r

def get_compress_key (t):
    if is_a (t, t_base):
        return t.name
    elif is_a (t, str):
        return ('str', t)
    elif is_a (t, t_var):
        return t.id
    elif is_a (t, t_predicate):
        return (t.name,) + tuple([get_compress_key (arg) for arg in t.args])
    else:
        raise ValueError

class unifier:

    # Maintains a conjunction of multi-equations.  In the typical HM
    #  algorithm, this would be called the 'subst'.  It implements the
    #  'union find' algorithm ('disjoint set' data structure).

    def __init__ (self, step=False):
        self.step = step
        self.eqs = set()
        self.exists = []
        # memoize decoded tvars
        self.decoded = {}
        self.max_size = 0

    def add (self, vars, type):
        # add a term to the conjunction, e.g. A=B=C=T  (where T is optional)
        assert (is_a (vars, set))
        assert (not is_a (type, t_var))

        if is_a (type, t_predicate):
            type = self.try_name_1 (type)

        if (type is None and len(vars) == 1) or len(vars) == 0:
            #self.dprint ('s-single')
            pass
        else:
            # any of these vars already present?
            for v in vars:
                if v.in_u is self:
                    # if so, then fuse
                    self.fuse (v.eq, vars, type)
                    return
            # nope, a new equation
            eq = multi (vars, type)

            for v in vars:
                v.in_u = self
            
            self.eqs.add (eq)

    def add2 (self, *args):
        # add an equation between a random collection of variables and types
        vars = set()
        types = []
        for arg in args:
            if is_a (arg, t_var):
                vars.add (arg)
            else:
                types.append (arg)
        if len(types) == 2:
            self.decompose ((vars, types[0]), (vars, types[1]))
        elif len(types) > 2:
            raise ValueError ("too many types")
        elif len(types) == 1:
            self.add (vars, types[0])
        else:
            self.add (vars, None)

    def try_name_1 (self, type):
        # ensure that a predicate's arguments are type variables,
        #  naming them if necessary (rule S-NAME-1).
        args2 = []
        flag = False
        for arg in type.args:
            if is_a (arg, str):
                # XXX row labels, must be a better way.
                args2.append (arg)
            elif not is_a (arg, t_var):
                #self.dprint ('s-name-1')
                x = t_var()
                self.exists.append (x)
                self.add (set([x]), arg)
                args2.append (x)
                flag = True
            else:
                args2.append (arg)
        if flag:
            return t_predicate (type.name, args2)
        else:
            return type

    def forget (self, eq):
        self.eqs.remove (eq)
        for v in eq.vars:
            v.in_u = False

    def fuse (self, eq, tvs0, ty0):
        tvs1 = eq.vars
        ty1  = eq.type
        # is a three-way fuse possible? (e.g. A=T0 B=T1; A=B=T2)
        # I don't think so, so let's ignore that possibility for now.
        #self.dprint ('s-fuse')
        self.forget (eq)
        if ty0 and ty1:
            # must unify types
            # A=B=T0 ^ B=C=T1 => A=B=C=T0=T1
            self.decompose ((tvs0, ty0), (tvs1, ty1))
        else:
            # A=B=T0 ^ B=C => A=B=C=T0
            self.add (tvs0.union (tvs1), ty0 or ty1)

    def decompose (self, t0, t1):
        tvs0, ty0 = t0
        tvs1, ty1 = t1
        tvs = tvs0.union (tvs1)
        if ty0 == ty1:
            # a=b=int=int, etc... => a=b=int
            self.add (tvs, ty0)
        elif is_pred (ty0, 'rlabel', 'rdefault') or is_pred (ty1, 'rlabel', 'rdefault'):
            self.unify_rows (ty0, ty1, tvs)
        elif (is_a (ty0, t_predicate) and is_a (ty1, t_predicate)
              and ty0.name == ty1.name
              and len(ty0.args) == len(ty1.args)):
            #self.dprint ('s-decompose')
            # P(a,b,c)=P(d,e,f)=ε => a=d ^ b=e ^ c=f ^ P(a,b,c)=ε
            for i in range (len (ty0.args)):
                self.add2 (ty0.args[i], ty1.args[i])
            self.add (tvs, ty0)
        elif is_a (ty0, t_int) and is_a (ty1, t_int):
            # covers int16 (need subtyping!)
            self.add (tvs, t_int16())
        else:
            #self.dprint ('s-clash')
            raise TypeError ((ty0, ty1))

    def unify_rows (self, ty0, ty1, tvs):
        if is_pred (ty0, 'rlabel') and is_pred (ty1, 'rlabel'):
            if ty0.args[0] != ty1.args[0]:
                # distinct head labels
                #self.dprint ('s-mutate-ll')
                # XXX be concerned about how one of these may have types
                #     and the other has variables.  do we need to check
                #     and reorder them?
                l0, t0, d0 = ty0.args
                l1, t1, d1 = ty1.args
                x = t_var()
                self.exists.append (x)
                self.add2 (d0, rlabel (l1, t1, x))
                self.add2 (d1, rlabel (l0, t0, x))
                self.add (tvs, rlabel (l0, t0, d0))
            else:
                # XXX this should be handled by the normal s-decompose
                l0, t0, d0 = ty0.args
                l1, t1, d1 = ty1.args
                self.add2 (t0, t1)
                self.add2 (d0, d1)
                self.add (tvs, ty0)
        elif is_pred (ty0, 'rlabel') or is_pred (ty1, 'rlabel'):
            # only one is an rlabel
            if is_pred (ty1, 'rlabel'):
                # ensure that ty0 is the rlabel
                ty0, ty1 = ty1, ty0
            if is_pred (ty1, 'rdefault'):
                #self.dprint ('s-mutate-dl')
                x = ty1.args[0]
                assert (is_a (x, t_var))
                self.add2 (x, ty0.args[1])
                self.add2 (ty1, ty0.args[2])
                self.add (tvs, ty1)
            elif is_a (ty1, t_predicate):
                # some other predicate
                #self.dprint ('s-mutate-gl')
                n = len (ty1.args)
                tvars0 = [t_var() for x in ty1.args]
                tvars1 = [t_var() for x in ty1.args]
                self.exists.extend (tvars0)
                self.exists.extend (tvars1)
                l0, t0, d0 = ty0.args
                g = ty1.name
                self.add2 (t_predicate (g, tvars0), t0)
                self.add2 (t_predicate (g, tvars1), d0)
                for i in range (n):
                    self.add2 (ty1.args[i], rlabel (l0, tvars0[i], tvars1[i]))
                self.add (tvs, ty1)
            else:
                #self.dprint ('s-clash')
                raise TypeError ((ty0, ty1))
        elif is_pred (ty0, 'rdefault',) or is_pred (ty1, 'rdefault'):
            if is_pred (ty1, 'rdefault'):
                # ensure that ty0 is the rdefault/δ
                ty0, ty1 = ty1, ty0
            if is_pred (ty1, 'rdefault'):
                # they're both rdefault - normal decompose here
                assert (len(ty0.args) == 1 and len(ty1.args) == 1)
                self.add2 (ty0.args[0], ty1.args[0])
                self.add (tvs, ty0)
            elif is_a (ty1, t_predicate):
                # some other predicate
                #self.dprint ('s-mutate-gd')
                n = len (ty1.args)
                g = ty1.name
                tvars = [ t_var() for x in ty1.args ]
                self.exists.extend (tvars)
                self.add2 (ty0.args[0], t_predicate (g, tvars))
                for i in range (n):
                    self.add2 (ty1.args[i], rdefault (tvars[i]))
                self.add (tvs, ty0)
            else:
                #self.dprint ('s-clash')
                raise TypeError ((ty0, ty1))
        else:
            #self.dprint ('s-clash')
            raise TypeError ((ty0, ty1))

    split_count = 0
    def split (self, sz):
        # leave in only equations made entirely of 'old' variables
        # this is the U1,U2 split from the rule S-POP-LET
        self.split_count += 1
        young = sz.vars
        u2 = unifier()
        to_add = []
        remove = []
        #self.sanity()
        for eq in self.eqs:
            #print 'split eq=',eq
            if eq.rep in young or eq.free().intersection (young):
                to_add.append ((eq.vars, eq.type))
                remove.append (eq)
        for eq in remove:
            self.eqs.remove (eq)
            for var in eq.vars:
                var.in_u = False
        for vars, type in to_add:
            u2.add (vars, type)
        #print 'u: %d eqs u2: %d eqs' % (len(self.eqs), len (u2.eqs))
        #self.sanity()
        return u2

    def dprint (self, msg):
        if self.step:
            sys.stderr.write ('*** ')
            sys.stderr.write (msg)
            sys.stderr.write ('\n')
            self.pprint()

    def sanity (self):
        all = set()
        for eq in self.eqs:
            for v in eq.vars:
                if not v.in_u:
                    print 'wtf?'
                    raise ValueError
                all.add (v)
        if all != set (self.vars.keys()):
            raise ValueError

    def simplify (self, vars, types):
        # remove/unname extra variables, and replace all tvars in types
        #   with the rep.
        #print 'before simplify'
        #self.pprint()
        #self.sanity()
        #def p (t):
        #    r = _p (t)
        #    print '%r => %r' % (t, r)
        #    return r
        def p (t):
            if is_a (t, t_var):
                if t.in_u is self:
                    # if it's in our set, return its rep
                    return t.eq.rep
                else:
                    # free variable
                    return t
            elif is_a (t, t_predicate):
                return t_predicate (t.name, [p(x) for x in t.args])
            else:
                return t
        unname = set()
        forget = set()
        for eq in list(self.eqs):
            new_vars = set([eq.rep])
            for v in eq.vars:
                if v is not eq.rep:
                    if v in vars and not v in types:
                        unname.add (v)
                        v.next = eq.rep
                    else:
                        new_vars.add (v)
            eq.vars = new_vars
            if eq.type:
                eq.type = p (eq.type)
            #else:
            #    # 'j=j' helps no one.
            #    forget.add (eq)
        for v in unname:
            v.in_u = False
        for eq in forget:
            self.forget (eq)
        if types:
            types = [p(x) for x in types]
        #self.sanity()
        return unname, types

    def prune (self, types, vars):
        # cut this conjunction down to equations referenced by <types>
        if len (self.eqs) == 0:
            # XXX could we still trim vars via types?
            return vars
        keep = set()
        seen = set()
        def p (v):
            seen.add (v)
            if v.in_u is self:
                eq = v.eq
                if not eq in keep:
                    keep.add (eq)
                    t = eq.type
                    if is_a (t, t_predicate):
                        for arg in t.args:
                            if is_a (arg, t_var):
                                p (arg)
        for v in types:
            p (v)
        pruned = 0
        total = len (self.eqs)
        for eq in list (self.eqs):
            if eq not in keep:
                self.forget (eq)
                pruned += 1

        #print 'pruned %d equations out of %d' % (pruned, total)
        total = len(vars)
        new_vars = []
        for v in vars:
            if v.in_u is self or v in seen:
                new_vars.append (v)
        #print 'pruned %d vars out of %d' % (total-len(new_vars), total)
        return new_vars
        
    def find_free (self, bound):
        # find the free variables of this unifier
        for eq in self.eqs:
            bound.update (eq.vars)
        free = set()
        for eq in self.eqs:
            t = eq.type
            if is_a (t, t_var) and t not in bound:
                free.add (t)
            elif is_a (t, t_predicate):
                for arg in t.args:
                    if is_a (arg, t_var) and arg not in bound:
                        free.add (arg)
        return free

    def reverse_graph (self):
        g = {}

        def add (k, v):
            if not g.has_key (k):
                g[k] = set([v])
            else:
                g[k].add(v)

        for eq in self.eqs:
            t = eq.type
            if t and is_a (t, t_predicate):
                for arg in t.args:
                    if is_a (arg, t_var):
                        add (arg, eq.rep)

        return g

    def do_letall (self, xbar, free):
        # find <ybar>, the subset of <xbar> that is 'determined' by <free>
        #print 'do_letall',
        xbar = set (xbar)
        rg = self.reverse_graph()
        # first pass, any types using vars from <free>
        y = []
        for eq in self.eqs:
            t = eq.type
            if is_a (t, t_predicate):
                for v in t.args:
                    if is_a (v, t_var) and v in free:
                        y.append (eq.rep)
                        break
        y2 = set()
        seen = set()
        while y:
            v = y.pop(0)
            if v not in seen:
                seen.add (v)
                if v in xbar:
                    y2.add (v)
                if rg.has_key (v):
                    y.extend (rg[v])
        
        return y2

    def pprint (self):
        sys.stdout.write ('U: ')
        eqs = list (self.eqs)
        # sort the equations by representative tvar
        eqs.sort (lambda a,b: cmp (a.rep.id, b.rep.id))
        for eq in eqs:
            sys.stdout.write ('\t%r\n' % (eq,))
        sys.stdout.write ('\n')

def instantiate_scheme (scheme):
    # instantiate a human-style type scheme (as returned from lookup_special_names())
    if not is_a (scheme, c_forall):
        return scheme
    else:
        vars = scheme.vars
        nvars = []
        map = {}
        for v in vars:
            # fresh tvar for each quantified tvar
            tv = t_var()
            map[v] = tv
            nvars.append (tv)
        def f (c):
            if is_a (c, t_predicate):
                return t_predicate (c.name, [f(x) for x in c.args])
            elif is_a (c, t_var) or is_a (c, int):
                if c in vars:
                    return map[c]
                else:
                    return c
            else:
                return c
        return c_forall (nvars, f (scheme.constraint))

class solver:

    def __init__ (self, context, verbose=False, step=False):
        self.context = context
        self.step = step
        # xxx need to split the notion of verbose and step
        self.step = step
        self.try_unname = False

    def dprint (self, msg):
        if self.step:
            sys.stderr.write (msg)
            sys.stderr.write ('\n')

    def solve (self, c):

        #self.dprint ('\nHit <return> at each pause (or "t<return>" to enter the debugger)')

        pvars = {}
        self.exists = []
        # ensure there are always two items on the stack
        s = [empty, empty]
        u = unifier (step=self.step)
        c = c

        orig_c = c
        rank = 0

        steps = 0

        def push (x):
            s.append (x)

        def pop ():
            s.pop()

        while 1:

            if self.step:
                print 'step %d' % steps
                print 'S:',
                self.pprint_stack (s)
                u.pprint()
                print 'C:', c
                print 'exists:', self.exists

            steps += 1

            #u.sanity()

            # the top two elements of the stack
            sy, sz = s[-2], s[-1]

            if self.step:
                print '-----------------------------'
                if raw_input().startswith ('t'):
                    trace()

            if self.try_unname:
                self.do_extra_unname (u, s)

            # --- solver ---            

            if u.exists:
                #self.dprint ('s-ex-1')
                self.move_exists (s, u.exists)
                u.exists = []
            elif is_a (sz, s_exists):
                #self.dprint ('s-record-ex')
                self.exists.extend (sz.vars)
                pop()
            elif is_a (c, c_equals):
                #self.dprint ('s-solve-eq')
                #try:
                u.add2 (*c.args)
                #except TypeError, terr:
                #    self.print_type_error (terr, c.args, u, s)
                c = c_true()
            elif is_a (c, c_is) and is_a (c.x, str):
                #self.dprint ('s-solve-id')
                # if I pass c.t down into lookup(), it's easier to attach the necessary
                #  c_equals() since there are two types of instantiation.
                con = self.lookup (c.x, s, c.t)
                # "Recall that if σ is of the form ∀X0..XN[U].X
                # where X0..XN#ftv(T), then c_is(σ, T) stands for ∃X0..XN.(U ^ X=T)."
                #self.dprint ('name=%s' % (c.x,))
                #self.dprint ('scheme= %r' % (scheme,))
                #self.dprint ('type=%r' % c.t)
                if is_a (con, c_forall):
                    c = c_exists (con.vars, con.constraint)
                else:
                    c = con
            elif is_a (c, c_and):
                #self.dprint ('s-solve-and')
                push (s_and (c.args[1]))
                c = c.args[0]
            elif is_a (c, c_exists):
                #self.dprint ('s-solve-ex')
                self.move_exists (s, c.vars)
                c = c.sub
            elif is_a (c, c_let):
                #self.dprint ('s-solve-let')
                if is_a (c.constraint, c_forall):
                    push (s_let (c.names, c.vars, c.constraint.vars, c.body, rank))
                    rank += 1
                    c = c.constraint.constraint
                else:
                    # let x: T in C == let x: ∀∅[true].T in C
                    push (s_let (c.names, c.vars, (), c.body, rank))
                    rank += 1
                    c = c_true()
            elif is_a (c, c_true):
                if is_a (sz, s_and):
                    #self.dprint ('s-pop-and')
                    pop()
                    c = sz.constraint
                elif is_a (sz, s_let):
                    unname, types = u.simplify (sz.vars, sz.types)
                    if unname and sz.vars:
                        self.dprint ('s-unname %r' % (unname,))
                        vars = [x for x in sz.vars if x not in unname]
                        self.dprint ('  old vars=%r' % (sz.vars,))
                        self.dprint ('  new vars=%r' % (vars,))
                        self.dprint ('  old types=%r' % (sz.types,))
                        self.dprint ('  new types=%r' % (types,))
                        #print 'unnamed %s %d' % (sz.names, len(unname))
                        sz.vars.difference_update (unname)
                        sz.types = types
                        #pop()
                        #push (s_let (sz.names, types, vars, sz.body, sz.rank))
                    else:
                        if len(sz.vars):
                            free = u.find_free (set (sz.vars))
                            # am I sure about this?
                            free.difference_update (set (sz.types))
                            if free:
                                # s-letall is only applicable if there are free variables in U.
                                # partition sz.vars into Ybar and Xbar, where ∃Xbar.U determines Ybar
                                # ------------- s-letall
                                #trace()
                                ybar = u.do_letall (sz.vars, free)
                                # make these guys 'old'
                                pop()
                                #print 'moving...', ybar
                                #trace()
                                self.move_exists (s, ybar)
                                vars = [x for x in sz.vars if x not in ybar]
                                push (s_let (sz.names, sz.types, vars, sz.body, sz.rank))
                                # XXX if we move Ybar up, will the changes in rank be visible in U,
                                #   or do we need to do that manually?
                                # ------------- s-letall
                                # XXX FIXME
                                sz = s[-1]

                        # the conditions have been met; turn the <let> into an <env>.
                        #self.dprint ('s-pop-let')

                        u2 = u.split (sz)
                        #print 'split'
                        #u2.pprint()
                        # if we do this, we lose detail with row types.  not sure what
                        #   other effects it may cause.
                        sz.vars = u2.prune (sz.types, sz.vars)
                        pop()
                        #sys.stderr.write ('[%d %d]%r\n' % (len (sz.vars), len(u.eqs), sz.names))
                        push (s_env (sz.names, sz.types, sz.vars, u2))
                        rank -= 1
                        c = sz.body
                elif is_a (sz, s_env):
                    #self.dprint ('s-pop-env')
                    pop()
                elif is_a (sz, s_empty):
                    # we're done!
                    #self.dprint ('exists=%r' % self.exists)
                    #self.dprint ('constraint=%r' % orig_c)
                    return pvars
                else:
                    raise ValueError ("unexpected")
            else:
                raise ValueError ("no rule applies")

    def move_exists (self, s, vars):
        # this implements the various S-EX-? rules that attach a set of tvars to
        #   the nearest <let> on the stack.
        n = len (s)
        for i in range (-1, -n, -1):
            if is_a (s[i], s_let):
                #print 'adding %r to s_let binding %r' % (vars, s[i].names)
                for v in vars:
                    if v in s[i].vars:
                        trace()
                s[i].add_vars (vars)
                # 1000 - empirically gives the fastest solver times
                if s[i].counter > 1000:
                    self.try_unname = True
                    s[i].counter = 0
                #sys.stderr.write ('{%d}' % (len(s[i].vars)))
                break
        else:
            self.exists.extend (vars)

    def do_extra_unname (self, u, s):
        # find the first s_let on the stack
        i = -1
        while 1:
            if is_a (s[i], s_let):
                si = s[i]
                break
            i -= 1
        unname, ignore = u.simplify (si.vars, set())
        si.vars.difference_update (unname)
        self.try_unname = False

    def instantiate_constraint (self, i, env, t, generalize=True):
        # instantiate a constraint-based type scheme (as found on the stack in an s_env)
        # can we do most of this work inside the unifier class?
        # ok, we have an env frame.  we want to instantiate this scheme.
        # env.vars are the quantified ones, we need to replace them
        #print 'instantiating', env.names[i], env.vars, env.types, env.u.eqs
        scheme = env.types[i]
        new = {}
        if generalize:
            for v in env.vars:
                new[v] = t_var()
        used = set()
        eqs = list(env.u.eqs)
        conj = []
        #print 'new=', new
        for i in range (len (eqs)):
            eq = eqs[i]
            new_vars = [new.get (x,x) for x in eq.vars]
            new_vars.sort (lambda a,b: cmp (a.id, b.id))
            if is_a (eq.type, t_predicate):
                new_args = []
                for arg in eq.type.args:
                    if is_a (arg, t_var):
                        # first, replace any var with its rep
                        if arg.in_u is env.u:
                            v = arg.eq.rep
                        else:
                            v = arg
                        # then, rename it if applicable
                        v = new.get (v, v)
                        new_args.append (v)
                    elif is_a (arg, str):
                        # row labels
                        new_args.append (arg)
                    else:
                        raise ValueError ("small term constraint?")
                new_type = t_predicate (eq.type.name, new_args)
            elif is_a (eq.type, t_base):
                new_type = eq.type
            else:
                # XXX can this make sense?  references to variables should
                #   have been simplified to pick the highest-ranking variable,
                #   so a type-less equation is not useful.
                #raise ValueError
                new_type = None
            # v=v=v=t => v=v & v=v & v=v & v=t
            if new_type:
                obs = new_vars + [new_type]
            else:
                obs = new_vars
            c = c_equals (obs[0], obs[1])
            for ob in obs[2:]:
                c = c_and (c, c_equals (obs[0], ob))
            conj.append (c)
        # XXX we should remove any vars from new_vars that were not referenced!
        if scheme in env.vars and generalize:
            scheme = new[scheme]
        conj.append (c_equals (t, scheme))
        if len(new):
            return c_forall (new.values(), list_to_conj (conj))
        else:
            return list_to_conj (conj)

    def lookup (self, x, s, t):
        n = -1
        while 1:
            f = s[n]
            n -= 1
            if is_a (f, s_env):
                for i in range (len (f.names)):
                    if f.names[i].name == x:
                        # our cheap version of the value restriction: restrict <ref> cells
                        #  to a monomorphic type.  later, let's do the whole
                        #  'expansive/non-expansive' version.
                        generalize = not f.names[i].assigns
                        return self.instantiate_constraint (i, f, t, generalize)               
            elif f is empty:
                break
            else:
                continue
        scheme = instantiate_scheme (self.lookup_special_names (x))
        return c_forall (scheme.vars, c_equals (t, scheme.constraint))

    # A trick I've used here is to encode the arity into the name of
    #  some of the prims, making it possible to return a correct arrow
    #  type for each.  I'm not entirely happy with it, but at least
    #  it's clear.

    def lookup_special_names (self, name):
        if name == '%rmake':
            return c_forall ((), arrow (rproduct (rdefault (abs()))))
        elif name.startswith ('%rextend/'):
            what, label = name.split ('/')
            # ∀XYZ.(Π(l:X;Y), Z) → Π(l:pre(Z);Y)
            return c_forall (
                (0,1,2),
                arrow (
                    rproduct (rlabel (label, pre(2), 1)),
                    rproduct (rlabel (label, 0, 1)),
                    2
                    )
                )
        elif name.startswith ('%raccess/'):
            what, label = name.split ('/')
            # ∀XY.Π(l:pre(X);Y) → X
            return c_forall ((0,1), arrow (0, rproduct (rlabel (label, pre(0), 1))))
        elif name.startswith ('%rset/'):
            what, label = name.split ('/')
            # ∀XY.(Π(l:pre(X);Y), X) → undefined
            return c_forall ((0,1), arrow (t_undefined(), rproduct (rlabel (label, pre(0), 1)), 0))
        elif name == '%vfail':
            return c_forall ((0,), arrow (0, rsum (rdefault (abs()))))
        elif name.startswith ('%dtcon/'):
            # lookup the type of the particular constructor
            what, dtname, label = name.split ('/')
            dt = self.context.datatypes[dtname]
            # e.g. list := nil | cons X list
            # %dtcons/list/cons := ∀X.(X,list(X)) → list(X)
            args = dt.constructors[label]
            return c_forall (dt.tvars, arrow (dt.scheme, *args))
        elif name.startswith ('%vcon/'):
            what, label, arity = name.split ('/')
            arity = int(arity)
            # remember each unique variant label
            self.remember_variant_label (label)
            if arity == 0:
                # ∀X.() → Σ(l:pre (Π());X)
                return c_forall ((1,), arrow (rsum (rlabel (label, pre (product()), 1)), product()))
            elif arity == 1:
                # ∀XY.X → Σ(l:pre X;Y)
                return c_forall ((0,1), arrow (rsum (rlabel (label, pre(0), 1)), 0))
            else:
                # ∀ABCD.Π(A,B,C) → Σ(l:pre (Π(A,B,C));D)
                args = tuple(range (arity))
                return c_forall (range(arity+1), arrow (rsum (rlabel (label, pre (product(*args)), arity)), product (*args)))
        elif name.startswith ('%vcase/'):
            what, label, arity = name.split ('/')
            arity = int (arity)
            # ∀012345.(3,4,5) → 0, Σ(l:1;2) → 0, Σ(l:pre(Π(3,4,5);2) → 0
            # ∀012345.f0,f1,s1 → 0
            args = range (3, arity+3)
            # success continuation
            f0 = arrow (0, *args)
            # failure continuation
            f1 = arrow (0, rsum (rlabel (label, 1, 2)))
            # the sum argument
            if arity == 1:
                t = args[0]
            else:
                t = product (*args)
            s1 = rsum (rlabel (label, pre (t), 2))
            return c_forall (range(arity+3), arrow (0, f0, f1, s1))
        elif name.startswith ('%vget/'):
            what, label, arity, index = name.split ('/')
            arity = int (arity)
            index = int (index)
            args = range (arity)
            rest = arity
            # e.g., to pick the second arg:
            # ∀0123. Σ(l:pre (0,1,2);3) → 1
            if arity > 1:
                vtype = rsum (rlabel (label, pre (product (*args)), rest))
            else:
                vtype = rsum (rlabel (label, pre (args[0]), rest))
            return c_forall (args + [arity], arrow (args[index], vtype))
        elif name.startswith ('%nvget/'):
            what, dtype, label, index = name.split ('/')
            dt = self.context.datatypes[dtype]
            ti = dt.constructors[label][int(index)]
            return c_forall (dt.tvars[:], arrow (ti, dt.scheme))
        elif name.startswith ('%vector-literal/'):
            what, arity = name.split ('/')
            arg_types = (0,) * int (arity)
            return c_forall ((0,), arrow (vector(0), *arg_types))
        elif name.startswith ('%make-vector'):
            return c_forall ((0,), arrow (vector(0), t_int(), 0))
        elif name.startswith ('%make-vec16'):
            return c_forall ((), arrow (vector(t_int16()), t_int()))
        elif name == '%%array-ref':
            return c_forall ((0,), arrow (0, vector (0), t_int()))
        elif name == '%%array-set':
            return c_forall ((0,), arrow (t_undefined(), vector (0), t_int(), 0))
        elif name == '%vec16-set':
            return c_forall ((), arrow (t_undefined(), vector(t_int16()), t_int(), t_int16()))
        # ------
        # pattern matching
        # ------
        elif name == '%%match-error':
            return c_forall ((0,), arrow (0))
        elif name == '%%fatbar':
            return c_forall ((0,0), arrow (0, 0, 0))
        elif name == '%%fail':
            return c_forall ((0,), arrow (0))
        # -------
        elif name.count (':') == 1:
            # a constructor used in a 'constructed literal'
            dt, alt = name.split (':')
            return self.lookup_special_names ('%%dtcon/%s/%s' % (dt, alt))
        else:
            raise UnboundVariable (name)

    # XXX consider recording record labels at this point as well
    def remember_variant_label (self, label):
        vl = self.context.variant_labels
        if not vl.has_key (label):
            # adjust for the hacked pre-installed labels like 'cons' and 'nil'.
            vl[label] = len (vl) - self.context.nvariant_offset

    def pprint_stack (self, s):

        W = sys.stdout.write

        W ('\n')
        n = len(s)
        # the 2 is for the two <empty> sentinels
        for i in range (2,n):
            W ('%2d: ' % (i-2,))
            si = s[i]
            if is_a (si, s_empty):
                W ('[]')
            elif is_a (si, s_and):
                W ('[] ^ %s' % si.constraint)
            elif is_a (si, s_exists):
                W ('exists%r.[]' % si.vars)
            elif is_a (si, s_let):
                names = ';'.join (['%s:%r' % (si.names[i].name, si.types[i]) for i in range (len (si.names))])
                W ('let %s: forall %r[[]] in %r' % (names, si.vars, si.body))
            elif is_a (si, s_env):
                names = ';'.join (['%s:%r' % (si.names[i].name, si.types[i]) for i in range (len (si.names))])
                W ('env %s: %r %r in []' % (names, si.vars, si.u.eqs))
            else:
                raise NotImplementedError
            W ('\n')

    def print_type_error (self, terr, args, u, s):
        self.pprint_stack (s)
        ty0, ty1 = terr.args[0]
        print 'Type Error', args
        # XXX decode() is gone...
        #raise TypeError (u.decode (ty0), u.decode (ty1))
        raise TypeError (ty0, ty1)

def list_to_conj (l):
    # convert list <l> into a conjunction built with <c_and>
    if len(l) == 0:
        return c_true()
    elif len(l) == 1:
        return l[0]
    else:
        r = l[0]
        for x in l[1:]:
            r = c_and (r, x)
        return r

def read_string (s):
    import cStringIO
    import lisp_reader
    sf = cStringIO.StringIO (s)
    r = lisp_reader.reader (sf)
    return r.read()

def get_record_sig (t):
    # rproduct (rlabel (...))
    if is_a (t, c_forall):
        t = t.constraint
    assert (is_pred (t, 'rproduct'))
    labels = []
    t = t.args[0]
    while 1:
        if is_pred (t, 'rlabel'):
            label, type, rest = t.args
            if is_pred (type, 'pre'):
                labels.append (label)
            t = rest
        elif is_pred (t, 'rdefault'):
            break
        elif is_a (t, t_var):
            labels.append ('...')
            break
        else:
            return None
    labels.sort()
    return tuple (labels)

class typer:

    def __init__ (self, context, verbose, step):
        self.context = context
        self.verbose = verbose
        self.step = step

    def go (self, exp):
        cg = constraint_generator (self.context)
        c, top_tv = cg.go (exp)
        print 'solving...'
        if self.verbose:
            pprint_constraint (c)
        s = solver (self.context, self.verbose, self.step)
        m = s.solve (c)
        print 'decoding...'
        for node in exp:
            node.type = self.decode (node.tv)
            #if node.is_a ('function') and node.name:
            #    print node.name, '\t', node.type

    def decode (self, t):
        seen = set()
        def p (t):
            if t in seen:
                return t
            else:
                seen.add (t)
            if is_a (t, t_var):
                if t.eq and t.eq.type:
                    return p (t.eq.type)
                elif t.next:
                    return p (t.next)
                return t
            elif is_a (t, t_predicate):
                return t_predicate (t.name, [p (x) for x in t.args])
            else:
                return t
        return p (t)
        
def test (s, step=True):
    import transform
    import nodes
    from pprint import pprint as pp
    global tvar_counter
    tvar_counter = -1
    # wrap everything in a top-level <let>
    s = "(let ((top %s)) top)" % s
    exp = read_string (s)
    t = transform.transformer (1)
    exp2 = t.go ([exp])
    w = nodes.walker()
    exp3 = w.go (exp2)
    # alpha conversion
    var_dict = nodes.rename_variables (exp3)
    t = typer ({}, step)
    c = t.go (exp3)

if __name__ == '__main__':
    if '-v' in sys.argv:
        step = True
    else:
        step = False
    # interactive test mode
    while 1:
        sys.stdout.write ('> ')
        line = raw_input()
        if not line:
            break
        else:
            test (line, step=step)
