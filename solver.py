# -*- Mode:Python; coding: utf-8 -*-

# Having a go at the constraint-based inference algorithm described by
#  Pottier and Rémy in "Advanced Topics in Types and Programming
#  Languages", chapter 10: "The Essence of ML Type Inference".
#
# Another great reference is a somewhat simplified presentation of the
#  same material, but (thankfully) with some context, by Pottier: "A
#  modern eye on ML type inference - Old techniques and recent
#  developments", available from his home page:
#  http://cristal.inria.fr/~fpottier/
#
# NOTE: this is a work in progress.
# http://www.nightmare.com/rushing/irken
#

# now with n-ary args
# TODO: s-letall (maybe?), kind-checking

import nodes
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

tvar_counter = 0

def fresh():
    global tvar_counter
    result = tvar_counter
    tvar_counter += 1
    return result

class _type:
    pass

class t_base (_type):
    def __cmp__ (self, other):
        return cmp (self.__class__, other.__class__)

class t_int (t_base):
    def __repr__ (self):
        return 'int'

class t_char (t_base):
    def __repr__ (self):
        return 'char'

class t_str (t_base):
    def __repr__ (self):
        return 'str'

# XXX consider using a true/false variant, then implementing 'if' as a filter.
class t_bool (t_base):
    def __repr__ (self):
        return 'bool'

class t_var (_type):
    next = None
    rank = -1
    letters = 'abcdefghijklmnopqrstuvwxyz'
    eq = None
    def __init__ (self):
        self.id = fresh()
    def __repr__ (self):
        return base_n (self.id, len(self.letters), self.letters)

class t_predicate (_type):
    def __init__ (self, name, args):
        self.name = name
        self.args = tuple (args)
    def __repr__ (self):
        # special case
        if self.name == 'arrow':
            if len(self.args) == 2:
                return '%r->%r' % (self.args[1], self.args[0])
            else:
                return '%r->%r' % (self.args[1:], self.args[0])
        else:
            return '%s%r' % (self.name, self.args)

def is_pred (t, *p):
    # is this a predicate from the set <p>?
    return is_a (t, t_predicate) and t.name in p

def arrow (*sig):
    # sig = (<result_type>, <arg0_type>, <arg1_type>, ...)
    # XXX this might be more clear as (<arg0>, <arg1>, ... <result>)
    return t_predicate ('arrow', sig)
    
# row types
def product (row):
    # a.k.a. 'Π'
    # XXX kind-check that args[0] is a row?
    return t_predicate ('product', (row,))

def sum (row):
    # a.k.a. 'Σ'
    return t_predicate ('sum', (row,))

def rdefault (arg):
    # a.k.a. 'δ'
    return t_predicate ('rdefault', (arg,))

def rlabel (name, type, rest):
    return t_predicate ('rlabel', (name, type, rest))

def abs():
    return t_predicate ('abs', ())

def pre (x):
    return t_predicate ('pre', (x,))

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
    def __init__ (self, formal, init, body):
        self.args = (formal, init, body)
        self.formal = formal
        self.init = init
        self.body = body

class c_forall (constraint):
    kind = 'forall'
    def __init__ (self, vars, constraint, type):
        self.args = (vars, constraint, type)
        self.vars = vars
        self.constraint = constraint
        self.type = type

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
    def __init__ (self, formal, vars, type, body, rank):
        self.formal = formal
        self.vars = vars
        self.type = type
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
    #   binds the scheme to the formal.
    def __init__ (self, formal, type):
        self.formal = formal
        self.type = type

# this is a two-phase algorithm
# 1) constraint generation
# 2) constraint solving

class constraint_generator:

    def go (self, exp):
        t = t_var()
        return self.gen (exp, t), t

    def gen (self, exp, t):
        if is_a (exp, nodes.varref):
            return c_is (exp.name, t)
        elif is_a (exp, nodes.function):
            if is_pred (t, 'arrow'):
                # lemma 10.4.7
                rtv, arg_tvs = t.args[0], list(t.args[1:])
            else:
                arg_tvs = [t_var() for x in exp.names]
                rtv = t_var()
            bod0 = self.gen (exp.body, rtv)
            for i in range (len (exp.names)):
                bod0 = c_let (exp.names[i].name, arg_tvs[i], bod0)
            # XXX: in ATTPL, this is a c_supertype relation
            sub1 = c_equals (t, arrow (rtv, *arg_tvs))
            if is_pred (t, 'arrow'):
                return c_and (bod0, sub1)
            else:
                return c_exists ([rtv] + arg_tvs, c_and (bod0, sub1))
        elif is_a (exp, nodes.application):
            arg_tvs = [t_var() for x in exp.rands]
            sub0 = self.gen (exp.rator, arrow (t, *arg_tvs))
            for i in range (len(exp.rands)):
                sub0 = c_and (sub0, self.gen (exp.rands[i], arg_tvs[i]))
            return c_exists (arg_tvs, sub0)
        elif is_a (exp, nodes.let):
            # XXX make this an n-ary let
            assert (len(exp.names) == 1)
            x = t_var()
            init0 = self.gen (exp.inits[0], x)
            body0 = self.gen (exp.body, t)
            return c_let (exp.names[0].name, c_forall ((x,), init0, x), body0)
        elif is_a (exp, nodes.letrec):
            # XXX make this an n-ary letrec
            assert (len(exp.names) == 1)
            name = exp.names[0].name
            y = t_var()
            fun = exp.inits[0]
            arg_tvs = [t_var() for x in fun.names]
            bod0 = self.gen (fun.body, y)
            bod1 = self.gen (exp.body, t)
            # each arg in a let
            for i in range (len (fun.names)):
                bod0 = c_let (fun.names[i].name, arg_tvs[i], bod0)
            sig = arrow (y, *arg_tvs)
            # inner binding
            bod0 = c_let (name, sig, bod0)
            # outer binding
            return c_let (name, c_forall ((y,) + tuple(arg_tvs), bod0, sig), bod1)
        elif is_a (exp, nodes.conditional):
            test_exp = self.gen (exp.test_exp, t_bool())
            x = t_var()
            then_exp = self.gen (exp.then_exp, x)
            else_exp = self.gen (exp.else_exp, x)
            return c_exists ((x,), c_and (test_exp, c_and (then_exp, else_exp)))
        elif is_a (exp, nodes.literal):
            if exp.kind == 'int':
                return c_equals (t, t_int())
            elif exp.kind == 'char':
                return c_equals (t, t_char())
            else:
                raise ValueError ("unsupported literal type")
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
        r = '='.join (['%r' % v for v in self.vars])
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
        u2 = []
        forget = []
        for eq in self.eqs:
            if eq.rep in young or eq.free.intersection (eq.vars):
                u2.append (c_equals (* list(eq.vars) + [eq.type]))
                forget.append (eq)
        for eq in forget:
            self.forget (eq)
        return list_to_conj (u2)

    def dprint (self, msg):
        if self.step:
            sys.stderr.write ('*** ')
            sys.stderr.write (msg)
            sys.stderr.write ('\n')
            self.pprint()

    def simplify (self):
        # d=c=b=a=x => a=x
        def compress (t):
            if is_a (t, t_var):
                return t.next or t
            elif is_a (t, t_predicate):
                return t_predicate (t.name, [compress(x) for x in t.args])
            else:
                return t
        for eq in self.eqs:
            eq.type = compress (eq.type)
            eq.vars = set ([eq.rep])

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

    def __init__ (self, step=True):
        self.step = step
        self.prim_env = self.make_prim_env()

    def dprint (self, msg):
        if self.step:
            sys.stderr.write (msg)
            sys.stderr.write ('\n')

    def solve (self, c):

        self.dprint ('\nHit <return> at each pause (or "t<return>" to enter the debugger)')

        pvars = []
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
                scheme, type = self.lookup (c.x, s), c.t
                # assert that scheme.type is a tvar
                # "Recall that if σ is of the form ∀X0..XN[U].X
                # where X0..XN#ftv(T), then c_is(σ, T) stands for ∃X0..XN.(U ^ X=T)."
                self.dprint ('scheme= %r' % scheme)
                self.dprint ('type=%r' % type)
                if is_a (scheme, c_forall):
                    if not scheme.vars and is_a (scheme.constraint, c_true):
                        c = c_equals (scheme.type, type)
                    else:
                        c = c_exists (scheme.vars, c_and (scheme.constraint, c_equals (scheme.type, type)))
                else:
                    c = c_equals (scheme, type)
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
                if is_a (c.init, c_forall):
                    vars = c.init.vars
                    push (s_let (c.formal, c.init.vars, c.init.type, c.body, rank))
                    rank += 1
                    c = c.init.constraint
                else:
                    # let x: T in C == let x: ∀∅[true].T in C
                    push (s_let (c.formal, (), c.init, c.body, rank))
                    c = c_true()
            elif is_a (c, c_true):
                if is_a (sz, s_and):
                    self.dprint ('s-pop-and')
                    pop()
                    c = sz.constraint
                elif is_a (sz, s_let) and not is_a (sz.type, t_var):
                    self.dprint ('s-name-2')
                    x = t_var()
                    pop()
                    push (s_let (sz.formal, sz.vars + (x,), x, sz.body, rank))
                    u.add (set([x]), sz.type)
                elif is_a (sz, s_let):
                    unname = []
                    for var in sz.vars:
                        # XXX this isn't quite right - we can subst sz.type with var.rep
                        if var.next and not u.is_free (var) and sz.type is not var:
                            unname.append (var)
                    if unname:
                        self.dprint ('s-unname %r' % (unname,))
                        vars = [x for x in sz.vars if x not in unname]
                        self.dprint ('  new vars=%r' % (vars,))
                        pop()
                        push (s_let (sz.formal, vars, sz.type, sz.body, sz.rank))
                    else:
                        # **** S-LETALL here ****
                        # s-letall will simplify the current scheme by removing some of
                        #   the quantified tvars.
                        # pop-let is a fall-through - after all
                        # the above conditions have been met it turns the <let>
                        # into an <env>.
                        self.dprint ('s-pop-let')
                        pop()
                        push (s_env (sz.formal, c_forall (sz.vars, u.split (sz), sz.type)))
                        c = sz.body
                elif is_a (sz, s_env):
                    # record the type scheme associated with this program variable
                    pvars.append ((sz.formal, sz.type.type))
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
                if f.formal == x:
                    raise ValueError ("shouldn't happen?")
                continue
            elif is_a (f, s_env):
                if f.formal != x:
                    continue
                else:
                    return f.type
            elif is_a (f, s_empty):
                break
        return self.instantiate (self.lookup_special_names (x))

    def instantiate (self, scheme):
        map = {}
        def walk (t):
            if is_a (t, int):
                if not map.has_key (t):
                    map[t] = t_var()
                return map[t]
            elif is_a (t, t_predicate):
                return t_predicate (t.name, [walk(x) for x in t.args])
            else:
                return t
        scheme = walk (scheme)
        tvars = tuple (map.values())
        return c_forall (tvars, c_true(), scheme)

    def make_prim_env (self):
        # build type schemes for builtins/primitives
        # these are written in an unusual fashion, using integers as place holders
        #   for type variables that will be instantiated fresh with each lookup.
        # XXX I think that is probably overkill, just using unique tvars within this
        #  environment is probably enough.
        int = t_int()
        char = t_char()
        bool = t_bool()
        e = {
            '%+' : arrow (int, int, int),
            '%-' : arrow (int, int, int),
            '%*' : arrow (int, int, int),
            '%=' : arrow (bool, int, int),
            '%make-pair' : arrow (t_predicate ('pair', (0, 1)), 0, 1),
            '%pair/first' : arrow (t_predicate ('pair', (0, 1)), 0),
            '%pair/second' : arrow (t_predicate ('pair', (0, 1)), 1),
            '%char->int' : arrow (int, char),
            '%int->char' : arrow (char, int),
            '%zed' : arrow (int),
            # rows
            '%abs' : arrow (t_predicate ('abs', ())), # arg discarded,
            '%pre' : arrow (t_predicate ('pre', (0,)), 0),
            # make a defaulted record
            '%rmake' : arrow (product (rdefault (0)), 0),
            # make an empty variant
            '%vmake' : arrow (sum (rdefault (abs())), 0),
            }
        # label-related row types are in the 'lookup_special' method
        return e

    def lookup_special_names (self, name):
        # we need this hack for row-related label lookups, since the label
        #  is part of the name.
        if self.prim_env.has_key (name):
            # the normal path
            return self.prim_env[name]
        elif name.startswith ('%rextend/'):
            what, label = name.split ('/')
            # XXX pre(X)
            # ∀XYZ.Π(l:X;Y) → Z → Π(l:Z;Y)
            return arrow (
                product (rlabel (label, 2, 1)),
                product (rlabel (label, 0, 1)),
                2
                )
        elif name.startswith ('%raccess/'):
            what, label = name.split ('/')
            # XXX pre(X)
            # ∀XY.Π(l:X;Y) → X
            return arrow (0, product (rlabel (label, 0, 1)))
        elif name.startswith ('%vextend/'):
            what, label = name.split ('/')
            # ∀XY.X → Σ(l:pre X;Y)
            return arrow (sum (rlabel (label, pre(0), 1)), 0)
        elif name.startswith ('%vcase/'):
            what, label = name.split ('/')
            # ∀XYX'Y'.(X → Y) → (Σ(l:X';Y') → Y) → Σ(l:pre X;Y') → Y
            # ∀XYX'Y'.f0 → f1 → s1 → Y
            f0 = arrow (1, 0)
            f1 = arrow (1, sum (rlabel (label, 2, 3)))
            s1 = sum (rlabel (label, pre (0), 3))
            return arrow (1, f0, f1, s1)
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
                W ('let %s: forall%r[[]].%r in %r' % (si.formal, si.vars, si.type, si.body))
            elif is_a (si, s_env):
                W ('env %s: %r in []' % (si.formal, si.type))
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

    print pvars
    u.pprint()
    print top_tv

    def lookup (x):
        # XXX this can probably be simplified because of u.simplify
        if is_a (x, t_var):
            if x.eq:
                if x.eq.type:
                    return lookup (x.eq.type)
                else:
                    return x.eq.rep
            else:
                return x
        elif is_a (x, t_predicate):
            return t_predicate (x.name, [lookup(y) for y in x.args])
        elif is_a (x, t_base):
            return x
        elif is_a (x, str):
            # XXX row labels
            return x
        else:
            raise ValueError ("unknown type object")

    for pvar, tvar in pvars:
        print '%r: %r' % (pvar, lookup (tvar))

    print 'program: %r' % lookup (top_tv)

def base_n (n, base, digits):
    # return a string representation of <n> in <base>, using <digits>
    s = []
    while 1:
        n, r = divmod (n, base)
        s.insert (0, digits[r])
        if not n:
            break
    return ''.join (s)

def read_string (s):
    import cStringIO
    import lisp_reader
    sf = cStringIO.StringIO (s)
    r = lisp_reader.reader (sf)
    return r.read()

def test (s, step=True):
    import nodes
    global tvar_counter
    tvar_counter = 0
    # wrap everything in a top-level <let>
    # if we omit this, s-compress won't work on top-level exists
    print 'expression=', s
    s = "(let ((top %s)) top)" % s
    exp = read_string (s)
    w = nodes.walker()
    exp2 = w.go (exp)
    # alpha conversion
    nodes.rename_variables (exp2)
    cg = constraint_generator()
    c, top_tv = cg.go (exp2)
    print 'constraint=', c
    m, u = solver(step).solve(c)
    u.simplify()
    u.renumber()
    print_solution (m, u, top_tv)

tests = [
    "5",
    "(lambda (x) 3)",
    "(lambda (x) x)",
    "(let ((f (lambda (x) x))) (f 5))",
    "(lambda (x) (lambda (y) x))",
    "(let ((f (lambda (x) x))) f)",
    "((lambda (x) x) (lambda (x) x))",
    "(%+ 3 4)",
    "(%- (%+ 3 4) 2)",
    "(%make-pair 3 4)",
    "(%make-pair 3 #\\A)",
    "(%pair/first (%make-pair 3 #\\A))",
    "(%pair/second (%make-pair 3 #\\A))",
    "(%char->int #\\A)",
    "(%int->char 65)",
    # zero-argument function
    "(%zed)",
    # this causes a cycle, breaks print-solution
    #"(let ((f (lambda (x) 3))) (%int->char (f f)))",
    # row tests
    # make a default row - maps all symbols to a function: δ(a->a)
    "(%rmake (lambda (x) x))",
    # lookup via a random field name on a row of type δ(int)
    "(%raccess/field (%rmake 0))",
    # extend a row: (label:char;δ(int))
    "(%rextend/label (%rmake 0) #\\A)",
    # dereference via the defined label
    "(%raccess/label (%rextend/label (%rmake 0) #\\A))",
    # dereference via an undefined label
    "(%raccess/fnord (%rextend/label (%rmake 0) #\\A))",
    # extend a row twice: (other:a->a; label:char; δ(int))
    "(%rextend/other (%rextend/label (%rmake 0) #\\A) (lambda (x) x))",
    # deref one defined label
    "(%raccess/label (%rextend/other (%rextend/label (%rmake 0) #\\A) (lambda (x) x)))",
    # deref the other
    "(%raccess/other (%rextend/other (%rextend/label (%rmake 0) #\\A) (lambda (x) x)))",
    # deref an undefined label
    "(%raccess/fnord (%rextend/other (%rextend/label (%rmake 0) #\\A) (lambda (x) x)))",
    # build an extended record, store it in a variable, then deref to pull the identity function out and use it.
"""(let ((rec0 (%rextend/other (%rextend/label (%rmake 0) #\\A) (lambda (x) x))))
     (let ((f0 (%raccess/other rec0)))
       (f0 9)))""",
    # abs/pre
    "(%abs)",
    "(%rmake (%abs))",
    "(%rextend/l0 (%rmake (%abs)) (%pre #\\A))",
    # variants
    "(%vextend/l0 3)",
    # three args: f0 is X->Y, f1 is sum->Y, s0 is sum. returns Y.
    "(%vcase/l0 (lambda (x) 3) (lambda (y) 4) (%vextend/l0 9))",
    # now with the wrong variant (note the difference in the type of <y>)
    "(%vcase/l0 (lambda (x) 3) (lambda (y) 4) (%vextend/l1 9))",
    # letrec
    "(letrec ((fact (lambda (n a) (if (%= n 0) a (fact (%- n 1) (%* n a)))))) (fact 5 1))",
    "(letrec ((fact (lambda (n) (if (%= n 0) 1 (%* n (fact (%- n 1))))))) (fact 5))",
    ]

if __name__ == '__main__':
    if '-t' in sys.argv:
        for t in tests:
            test (t, step=False)
    elif '-p' in sys.argv:
        import profile
        profile.run ("test (tests[-1], step=False)")
    elif '-l' in sys.argv:
        # try out the very last test
        test (tests[-1], step=True)
    elif '-i' in sys.argv:
        # interactive
        while 1:
            sys.stdout.write ('> ')
            line = raw_input()
            if not line:
                break
            else:
                #test (line, step=False)
                test (line, step=True)
    else:
        test ("(lambda (x) x)")
