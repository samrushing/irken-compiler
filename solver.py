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

class c_false (constraint):
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
        self.vars = vars
        self.body = body
        self.rank = rank
        for v in vars:
            v.rank = rank

    def add_vars (self, vars):
        self.vars += tuple (vars)
        for v in vars:
            v.rank = self.rank

class s_env (frame):
    # after a <let> type scheme has been solved, an <env> frame
    #   binds the schemes to the formals.
    def __init__ (self, names, types):
        self.names = names
        self.types = types

# this is a two-phase algorithm
# 1) constraint generation
# 2) constraint solving

class constraint_generator:

    def __init__ (self, scc_graph):
        self.scc_graph = scc_graph

    def go (self, exp):
        t = t_var()
        return self.gen (exp, t), t

    def gen (self, exp, t):
        if is_a (t, t_var):
            t.node = exp
        if exp.is_a ('varref'):
            return c_is (exp.name, t)
        elif exp.is_a ('function'):
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
        elif exp.is_a ('application'):
            args = [t_var() for x in exp.rands]
            c = self.gen (exp.rator, arrow (t, *args))
            for i in range (len(exp.rands)):
                c = c_and (c, self.gen (exp.rands[i], args[i]))
            return c_exists (args, c)
        elif exp.is_a ('primapp'):
            args = [t_var() for x in exp.args]
            c = c_is (exp.name, arrow (t, *args))
            for i in range (len(exp.args)):
                c = c_and (c, self.gen (exp.args[i], args[i]))
            return c_exists (args, c)
        elif exp.is_a ('cexp'):
            sig = parse_cexp_type (exp.type_sig)
            r = c_equals (t, sig.args[0]) # result type
            for i in range (len (exp.args)):
                r = c_and (r, self.gen (exp.args[i], sig.args[i+1]))
            return r
        elif exp.is_a ('let_splat'):
            r = self.gen (exp.body, t)
            n = len (exp.names)
            for i in range (n-1,-1,-1):
                name = exp.names[i]
                init = exp.inits[i]
                var = t_var()
                r = c_let ([name], [var], c_forall ((var,), self.gen (init, var)), r)
            return r
        elif exp.is_a ('fix'):
            partition = graph.reorder_fix (exp, self.scc_graph)
            partition.reverse()
            c0 = self.gen (exp.body, t)
            # XXX deep partitioning magic here
            for part in partition:
                names = [exp.names[i] for i in part]
                funs  = [exp.inits[i] for i in part]
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
            return c0
        elif exp.is_a ('conditional'):
            test_exp = self.gen (exp.test_exp, t_bool())
            then_exp = self.gen (exp.then_exp, t)
            else_exp = self.gen (exp.else_exp, t)
            return c_and (test_exp, c_and (then_exp, else_exp))
        elif exp.is_a ('sequence'):
            n = len (exp.subs)
            tvars = [t_var() for x in range (n-1)]
            c = self.gen (exp.subs[-1], t)
            for i in range (n-1):
                # everything but the last, type it as don't-care
                c = c_and (c, self.gen (exp.subs[i], tvars[i]))
            return c_exists (tvars, c)
        elif exp.is_a ('literal'):
            return c_equals (t, base_types[exp.type])
        else:
            raise ValueError


class UnboundVariable (Exception):
    pass

class TypeError (Exception):
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
        self.free = set()
        ftv (self.free, type)

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

    def __repr__ (self):
        vars = list(self.vars)
        vars.sort (lambda a,b: cmp (a.id, b.id))
        r = '='.join (['%r' % v for v in vars])
        if self.type:
            return r + '=%r' % (self.type,)
        else:
            return r

class unifier:

    # Maintains a conjunction of multi-equations.  In the typical HM
    #  algorithm, this would be called the 'subst'.  It implements the
    #  'union find' algorithm ('disjoint set' data structure).

    def __init__ (self, step=False):
        self.step = step
        self.eqs = set()
        self.exists = []

    def add (self, vars, type=None):
        # add a term to the conjunction, e.g. A=B=C=T  (where T is optional)
        assert (is_a (vars, set))
        assert (not is_a (type, t_var))

        if is_a (type, t_predicate):
            type = self.try_name_1 (type)

        if (not type and len(vars) == 1) or (type and len(vars) == 0):
            self.dprint ('s-single')
        else:
            # any of these vars already present?
            for v in vars:
                if v.eq:
                    # if so, then fuse
                    self.fuse (v.eq, vars, type)
                    return
            # nope, a new equation
            eq = multi (vars, type)
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

    def is_free (self, var):
        # is <var> free in this equation?
        # XXX pg 444 states that we can use <rank> to do this in constant time.
        for eq in self.eqs:
            # any var referenced in a type (that does not
            #  point to another var) is 'free'
            if var in eq.free and not var.next:
                return True
        else:
            return False

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
                self.dprint ('s-name-1')
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
            v.eq = None
            v.next = None

    def fuse (self, eq, tvs0, ty0):
        tvs1 = eq.vars
        ty1  = eq.type
        # is a three-way fuse possible? (e.g. A=T0 B=T1; A=B=T2)
        # I don't think so, so let's ignore that possibility for now.
        self.forget (eq)
        self.dprint ('s-fuse')
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
            self.dprint ('s-decompose')
            # P(a,b,c)=P(d,e,f)=ε => a=d ^ b=e ^ c=f ^ P(a,b,c)=ε
            for i in range (len (ty0.args)):
                self.add2 (ty0.args[i], ty1.args[i])
            self.add (tvs, ty0)
        else:
            self.dprint ('s-clash')
            raise TypeError ((ty0, ty1))

    def unify_rows (self, ty0, ty1, tvs):
        if is_pred (ty0, 'rlabel') and is_pred (ty1, 'rlabel'):
            if ty0.args[0] != ty1.args[0]:
                # distinct head labels
                self.dprint ('s-mutate-ll')
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
                self.dprint ('s-mutate-dl')
                x = ty1.args[0]
                assert (is_a (x, t_var))
                self.add2 (x, ty0.args[1])
                self.add2 (ty1, ty0.args[2])
                self.add (tvs, ty1)
            elif is_a (ty1, t_predicate):
                # some other predicate
                self.dprint ('s-mutate-gl')
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
                self.dprint ('s-clash')
                raise TypeError ((ty0, ty1))
        elif is_pred (ty0, 'rdefault',) or is_pred (ty1, 'rdefault'):
            if is_pred (ty1, 'rdefault'):
                # ensure that ty0 is the rdefault/δ
                ty0, ty1 = ty1, ty0
            if is_a (ty1, t_predicate):
                # some other predicate
                self.dprint ('s-mutate-gd')
                n = len (ty1.args)
                g = ty1.name
                tvars = [ t_var() for x in ty1.args ]
                self.exists.extend (tvars)
                self.add2 (ty0.args[0], t_predicate (g, tvars))
                for i in range (n):
                    self.add2 (ty1.args[i], rdefault (tvars[i]))
                self.add (tvs, ty0)
            else:
                self.dprint ('s-clash')
                raise TypeError ((ty0, ty1))
        else:
            self.dprint ('s-clash')
            raise TypeError ((ty0, ty1))

    def split (self, sz):
        # leave in only equations made entirely of 'old' variables
        # this is the U1,U2 split from the rule S-POP-LET
        young = set (sz.vars)
        u2 = {}
        forget = []
        for eq in self.eqs:
            if eq.rep in young or eq.free.intersection (eq.vars):
                decoded = self.decode (eq.rep)
                if eq.type:
                    u2[eq.rep] = decoded
                forget.append (eq)
                # XXX a hack to try to recover types for every node
                for v in list(eq.vars):
                    if hasattr (v, 'node'):
                        v.node.type = decoded
        for eq in forget:
            self.forget (eq)
        return u2

    def dprint (self, msg):
        if self.step:
            sys.stderr.write ('*** ')
            sys.stderr.write (msg)
            sys.stderr.write ('\n')
            self.pprint()

    def decode (self, t):
        # decode this type as much as possible (i.e., follow every known tvar)
        def p (t):
            if is_a (t, t_var):
                if t.eq:
                    if t.eq.type is None:
                        return t.eq.rep
                    else:
                        return p (t.eq.type)
                else:
                    return t
            elif is_a (t, t_predicate):
                return t_predicate (t.name, [p(x) for x in t.args])
            else:
                return t
        return p (t)

    def renumber (self):
        # first, collect every tvar referenced
        tvars = set()
        for eq in self.eqs:
            tvars.update (eq.vars)
            tvars.update (eq.free)
        tvars = list(tvars)
        tvars.sort (lambda a,b: cmp (a.id, b.id))
        print 'renumbering, %d tvars' % (len(tvars),)
        # heh, don't look!
        for i in range (len (tvars)):
            tvars[i].id = i

    def pprint (self):
        sys.stdout.write ('U: ')
        eqs = list (self.eqs)
        # sort the equations by representative tvar
        eqs.sort (lambda a,b: cmp (a.rep.id, b.rep.id))
        for eq in eqs:
            sys.stdout.write ('\t%r\n' % (eq,))
        sys.stdout.write ('\n')

class solver:

    def __init__ (self, datatypes, step=True):
        self.datatypes = datatypes
        self.step = step

    def dprint (self, msg):
        if self.step:
            sys.stderr.write (msg)
            sys.stderr.write ('\n')

    def solve (self, c):

        self.dprint ('\nHit <return> at each pause (or "t<return>" to enter the debugger)')

        pvars = {}
        self.exists = []
        # ensure there are always two items on the stack
        s = [empty, empty]
        u = unifier (self.step)
        c = c

        orig_c = c
        rank = 0

        def push (x):
            s.append (x)

        def pop ():
            s.pop()

        while 1:

            if self.step:
                print 'S:',
                self.pprint_stack (s)
                u.pprint()
                print 'C:', c
                print 'exists:', self.exists

            # the top two elements of the stack
            sy, sz = s[-2], s[-1]

            if self.step:
                print '-----------------------------'
                if raw_input().startswith ('t'):
                    trace()

            # --- solver ---            

            if u.exists:
                self.dprint ('s-ex-1')
                self.move_exists (s, u.exists)
                u.exists = []
            elif is_a (sz, s_exists):
                self.dprint ('s-record-ex')
                self.exists.extend (sz.vars)
                pop()
            elif is_a (c, c_equals):
                self.dprint ('s-solve-eq')
                u.add2 (*c.args)
                c = c_true()
            elif is_a (c, c_is) and is_a (c.x, str):
                self.dprint ('s-solve-id')
                scheme = self.lookup (c.x, s)
                scheme = self.instantiate (scheme)
                # "Recall that if σ is of the form ∀X0..XN[U].X
                # where X0..XN#ftv(T), then c_is(σ, T) stands for ∃X0..XN.(U ^ X=T)."
                self.dprint ('name=%s' % (c.x,))
                self.dprint ('scheme= %r' % (scheme,))
                self.dprint ('type=%r' % c.t)
                if is_a (scheme, c_forall):
                    c = c_exists (scheme.vars, c_equals (scheme.constraint, c.t))
                else:
                    c = c_equals (scheme, c.t)
            elif is_a (c, c_and):
                self.dprint ('s-solve-and')
                push (s_and (c.args[1]))
                c = c.args[0]
            elif is_a (c, c_exists):
                self.dprint ('s-solve-ex')
                self.move_exists (s, c.vars)
                c = c.sub
            elif is_a (c, c_let):
                self.dprint ('s-solve-let')
                if is_a (c.constraint, c_forall):
                    push (s_let (c.names, c.vars, c.constraint.vars, c.body, rank))
                    c = c.constraint.constraint
                else:
                    # let x: T in C == let x: ∀∅[true].T in C
                    push (s_let (c.names, c.vars, (), c.body, rank))
                    c = c_true()
            elif is_a (c, c_true):
                if is_a (sz, s_and):
                    self.dprint ('s-pop-and')
                    pop()
                    c = sz.constraint
                elif is_a (sz, s_let):
                    unname = []
                    for var in sz.vars:
                        # XXX this isn't quite right - we can subst sz.type with var.rep
                        if var.next and not u.is_free (var) and var not in sz.types:
                            unname.append (var)
                    if unname:
                        self.dprint ('s-unname %r' % (unname,))
                        vars = [x for x in sz.vars if x not in unname]
                        self.dprint ('  new vars=%r' % (vars,))
                        pop()
                        push (s_let (sz.names, sz.types, vars, sz.body, sz.rank))
                    else:
                        # the conditions have been met; turn the <let> into an <env>.
                        self.dprint ('s-pop-let')
                        schemes = self.build_type_schemes (sz.types, sz.vars, u.split (sz))
                        pop()
                        push (s_env (sz.names, schemes))
                        # record for posterity
                        for i in range (len (sz.names)):
                            if not is_a (schemes[i], t_var):
                                # if it's a real type, assign it to the node
                                sz.names[i].type = schemes[i]
                            print '%s=%r' % (sz.names[i].name, schemes[i])
                            pvars[sz.names[i]] = schemes[i]
                        c = sz.body
                elif is_a (sz, s_env):
                    self.dprint ('s-pop-env')
                    pop()
                elif is_a (sz, s_empty):
                    # we're done!
                    self.dprint ('exists=%r' % self.exists)
                    self.dprint ('constraint=%r' % orig_c)
                    return pvars, u
                else:
                    raise ValueError ("unexpected")
            else:
                raise ValueError ("no rule applies")

    def build_type_schemes (self, vars, qvs, eqs):
        # I'm trying to do the 'obvious' thing here - use U2 to compute the type scheme
        #   that should be attached to each pvar's tvar.  I think I may be throwing away
        #   too much info, though.  e.g., this may work for computing a straightforward
        #   type signature but will fail to propagate the kinds of extra info that a
        #   constraint solver can give us.  [thought: we could maybe fix this by including
        #   in the scheme/constraint any equation that references the variable as well].
        # [one of the reasons I did this step was to cut down on possibly huge constraints
        #  caused by a big fix full of tens of functions.  This is less likely to happen
        #  with the partitioning/reordering code since it will collect only truly mutually
        #  recursive functions together, most funs will each get their own <let>]
        def p (t):
            if is_a (t, t_var):
                if eqs.has_key (t):
                    return p (eqs[t])
                else:
                    used.add (t)
                    return t
            elif is_a (t, t_predicate):
                return t_predicate (t.name, [p(x) for x in t.args])
            else:
                return t
        if not eqs:
            return vars
        else:
            result = []
            qvs = set (qvs)
            for var in vars:
                used = set()
                scheme = p (var)
                used = used.intersection (qvs)
                if used:
                    result.append (c_forall (tuple(used), scheme))
                else:
                    result.append (scheme)
            return result

    def move_exists (self, s, vars):
        # this implements the various S-EX-? rules that attach a set of tvars to
        #   the nearest <let> on the stack.
        n = len (s)
        for i in range (-1, -n, -1):
            if is_a (s[i], s_let):
                s[i].add_vars (vars)
                break
        else:
            self.exists.extend (vars)

    def instantiate (self, scheme):
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
                if is_a (c, c_equals):
                    return c_equals (f (c.args[0]), f (c.args[1]))
                elif is_a (c, c_and):
                    return c_and (f (c.args[0]), f (c.args[1]))
                elif is_a (c, t_predicate):
                    return t_predicate (c.name, [f(x) for x in c.args])
                elif is_a (c, t_var) or is_a (c, int):
                    if c in vars:
                        return map[c]
                    else:
                        return c
                elif is_a (c, t_base):
                    return c
                elif is_a (c, c_true):
                    return c
                elif is_a (c, str):
                    # XXX record labels
                    return c
                else:
                    # what other objects can we expect here? exists/forall??
                    raise ValueError
            return c_forall (nvars, f (scheme.constraint))

    def lookup (self, x, s):
        n = -1
        while 1:
            f = s[n]
            n -= 1
            if is_a (f, s_and):
                continue
            elif is_a (f, s_exists):
                continue
            elif is_a (f, s_let):
                continue
            elif is_a (f, s_env):
                for i in range (len (f.names)):
                    if f.names[i].name == x:
                        return f.types[i]
            elif is_a (f, s_empty):
                break
            else:
                raise ValueError ("I'm confused")
        return self.lookup_special_names (x)

    def lookup_special_names (self, name):
        if name == '%rmake':
            return c_forall ((), arrow (product (rdefault (abs()))))
        elif name == '%vmake':
            return c_forall ((), arrow (sum (rdefault (abs()))))
        elif name.startswith ('%rextend/'):
            what, label = name.split ('/')
            # ∀XYZ.(Π(l:X;Y), Z) → Π(l:pre(Z);Y)
            return c_forall (
                (0,1,2),
                arrow (
                    product (rlabel (label, pre(2), 1)),
                    product (rlabel (label, 0, 1)),
                    2
                    )
                )
        elif name.startswith ('%raccess/'):
            what, label = name.split ('/')
            # ∀XY.Π(l:pre(X);Y) → X
            return c_forall ((0,1), arrow (0, product (rlabel (label, pre(0), 1))))
        elif name.startswith ('%vextend/'):
            what, label = name.split ('/')
            # ∀XY.X → Σ(l:pre X;Y)
            return c_forall ((0,1), arrow (sum (rlabel (label, pre(0), 1)), 0))
        elif name.startswith ('%vcase/'):
            what, label = name.split ('/')
            # ∀XYX'Y'.(X → Y) → (Σ(l:X';Y') → Y) → Σ(l:pre X;Y') → Y
            # ∀XYX'Y'.f0 → f1 → s1 → Y
            f0 = arrow (1, 0)
            f1 = arrow (1, sum (rlabel (label, 2, 3)))
            s1 = sum (rlabel (label, pre (0), 3))
            return c_forall ((0,1,2,3), arrow (1, f0, f1, s1))
        else:
            raise UnboundVariable (name)

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
                W ('let %s: forall%r[[]] in %r' % (names, si.vars, si.body))
            elif is_a (si, s_env):
                names = ';'.join (['%s:%r' % (si.names[i].name, si.types[i]) for i in range (len (si.names))])
                W ('env %s: in []' % (names,))
            else:
                raise NotImplementedError
            W ('\n')

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

def ftv (s, t):
    # accumulate free type variables into the set <s>
    if is_a (t, t_var):
        s.add (t)
    elif is_a (t, t_predicate):
        for arg in t.args:
            ftv (s, arg)
    elif is_a (t, t_base):
        pass
    elif is_a (t, str):
        pass
    elif t is None:
        pass
    else:
        raise ValueError ("unknown type object")

def print_solution (pvars, u, top_tv):
    from pprint import pprint as pp
    pp (pvars.keys())
    u.pprint()
    print top_tv
    print 'program: %r' % u.decode (top_tv)

def read_string (s):
    import cStringIO
    import lisp_reader
    sf = cStringIO.StringIO (s)
    r = lisp_reader.reader (sf)
    return r.read()

class typer:

    def __init__ (self, context, verbose):
        self.context = context
        self.verbose = verbose

    def go (self, exp):
        cg = constraint_generator (self.context.scc_graph)
        c, top_tv = cg.go (exp)
        pprint_constraint (c)
        self.verbose = False
        m, u = solver (self.context.datatypes, self.verbose).solve (c)
        # I *think* that any remaining 'unsolved' types (i.e., ones for which we assigned
        # a tvar as the type rather than a scheme) are now solved with <u>.  I also think
        # that any unclosed types (for example an unclosed row type) should throw a
        # compile error here - we do not know statically what fields it might hold.
        for key, val in m.iteritems():
            if is_a (val, t_var):
                key.type = u.decode (val)
            else:
                key.type = val
        #u.renumber()
        # one last pass of decoding...
        for eq in u.eqs:
            decoded = u.decode (eq.rep)
            for v in eq.vars:
                if hasattr (v, 'node'):
                    v.node.type = decoded
        self.find_records (m, u)
        print_solution (m, u, top_tv)

    def find_records (self, m, u):
        all = []
        def p (t, a):
            if is_pred (t, 'rlabel'):
                label, type, rest = t.args
                p (type, [])    # records within records...
                p (rest, [label]+a)
            elif is_pred (t, 'rdefault'):
                all.append (a)
            elif is_a (t, t_predicate):
                for arg in t.args:
                    p (arg, a)
            elif is_a (t, c_forall):
                p (t.constraint, [])
            else:
                pass
        for key, val in m.iteritems():
            p (val, [])
            if is_pred (val, 'product'):
                key.sig = get_record_sig (val)
            elif is_a (val, c_forall) and is_pred (val.constraint, 'product'):
                key.sig = get_record_sig (val.constraint)
        labels = {}
        all2 = {}
        for rec in all:
            rec.sort()
            rec = tuple(rec)
            if not all2.has_key (rec):
                all2[rec] = len(all2)
            for label in rec:
                if not labels.has_key (label):
                    labels[label] = len(labels)
        print 'record types', all2
        self.context.record_types = all2
        self.context.record_labels = labels

def record_signature (t):
    # given a full record, return its 'signature'
    assert (t.name == 'product')
    return result

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
