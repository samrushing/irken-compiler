# -*- Mode: Python -*-

#
# NOTE: currently unused
#
# possible replacement for analyze.py that uses the algorithm from Waddell & Dybvig's "Fast and Effective Procedure Inlining".
#
# problems:
#   in general, as described the algorithm will underperform my own because it makes no attempt to inline 'simple'
#   arguments at all - it basically transforms any inline to a let (presumably these are simply arguments on a stack
#   in Chez Scheme).  With enough work I could probably figure this out, but for now I'm putting this on the back burner.
# 
#   Their approach to correctly handling side effects, call/cc, etc... is more robust than my own.  For the most part my
#     answer to this has been "don't do that".  I'll probably have to fix that eventually. 8^)
#
# ideas:
#   We don't have nearly as much to gain from their simplification of conditionals, since our conditional is strongly typed.
#   However, exactly the same technique would apply to vcase, and would make even bigger gains - for example the ad-hoc
#   simplification that they mention "(member e1 '())" - would be automatically handled with variant datatypes.
#   That would be my primary motivation for revisiting this code.
#
# note: this code is not nearly finished/debugged, so if you want to
#   use it as an aid to understanding the algorithm please be careful.

import nodes
import sys

from pprint import pprint as pp
from pdb import set_trace as trace

is_a = isinstance

# from the paper:
# e ::= const | ref | primref | if | seq | assign | lambda | letrec | call

class operand:
    def __init__ (self, exp, env, loc):
        self.exp = exp
        self.env = env
        self.loc = loc

class var:
    def __init__ (self, name, op, sflags, rflags):
        self.name = name
        self.op = op
        self.sflags = sflags
        self.rflags = rflags
    def __repr__ (self):
        return '<%s:%r:%r:%r>' % (self.name, self.op, self.sflags, self.rflags)

class context:
    pass

# to eventually handle vcase as well, hopefully one day
#  'if' will be expressed in terms of vcase?

class c_test (context):
    pass

class c_effect (context):
    pass

class c_value (context):
    pass

class c_app (context):
    def __init__ (self, rands, ctx, flags):
        self.rands = rands
        self.ctx = ctx
        self.flags = flags

def lit_eq (a, b):
    return a.is_a ('literal') and b.is_a ('literal') and a.ltype == b.ltype and a.value == b.value

lit_true  = nodes.literal ('bool', 'true')
lit_false = nodes.literal ('bool', 'false')
lit_undef = nodes.literal ('undefined', 'undefined')

class inliner:
    def __init__ (self):
        pass

    def go (self, exp):
        env = {}
        return self.I (exp, c_value(), env)

    def result (self, exp):
        # helper function
        if exp.is_a ('sequence'):
            return exp.subs[-1]
        else:
            return exp

    def visit (self, rand, ctx):
        if rand.loc == 'unvisited':
            rand0 = self.I (rand.exp, ctx, rand.env)
            rand.loc = rand0
            return rand0
        else:
            print 'VISITING %r' % (rand,)
            return rand.loc

    def copy (self, var, exp, ctx):
        print 'copy!'
        if exp.is_a ('literal'):
            return self.I (exp, ctx, {})
        elif exp.is_a ('varref') and 'assign' not in self.get_var (exp.var).sflags:
            return exp
        elif is_a (ctx, c_app) and exp.is_a ('function'): # XXX or primapp
            return self.fold_lambda (exp, ctx, None)
        elif is_a (ctx, c_value) and exp.is_a ('primapp'):
            trace()
        elif is_a (ctx, c_test) and exp.one_of ('primapp', 'varset', 'function'):
            return lit_true
        else:
            var.loc.add ('ref')
            return nodes.varref (var.name)

    def seq (self, subs):
        subs = [x for x in subs if not lit_eq (x, lit_undef)]
        if len(subs) == 1:
            return subs[0]
        else:
            trace()
            return nodes.sequence (subs)

    def I (self, exp, ctx, env):
        W ("%r %r %r\n" % (exp.kind, ctx, env))
        #trace()
        # i.e., <const>
        if exp.is_a ('literal'):
            if is_a (ctx, c_effect):
                return nodes.literal ('undefined', 'undefined')
            elif is_a (ctx, c_test) and (exp.kind != 'bool' or exp.value == 'true'):
                return nodes.literal ('bool', 'true')
            else:
                return exp
        elif exp.is_a ('sequence'):
            n = len (exp.subs)
            subs = []
            for i in range (n):
                if i == n-1:
                    ctx0 = ctx
                else:
                    ctx0 = c_effect()
                subs.append (self.I (exp.subs[i], ctx0, env))
            return self.seq (subs)
        elif exp.is_a ('conditional'):
            # XXX collapse with vcase?
            if is_a (ctx, c_app):
                ctx1 = c_value()
            else:
                ctx1 = ctx
            test_exp = self.I (exp.test_exp, c_test(), env)
            rtest = self.result (test_exp)
            if rtest.is_a ('literal') and rtest.ltype == 'bool':
                if rtest.value == 'true':
                    then_exp = self.I (exp.then_exp, ctx1, env)
                    return self.seq ([test_exp, then_exp])
                else:
                    else_exp = self.I (exp.else_exp, ctx1, env)
                    return self.seq ([test_exp, else_exp])
            else:
                then_exp = self.I (exp.then_exp, ctx1, env)
                else_exp = self.I (exp.else_exp, ctx1, env)
                if lit_eq (then_exp, else_exp):
                    # they both happen to be the same constant
                    return self.seq ([test_exp, then_exp])
                else:
                    return nodes.conditional (test_exp, then_exp, else_exp)
        elif exp.is_a ('application'):
            rands = [ operand (x, env, 'unvisited') for x in exp.rands ]
            ctx1 = c_app (rands, ctx, set())
            rator = self.I (exp.rator, ctx1, env)
            if 'inlined' in ctx1.flags:
                return rator
            else:
                rands = [self.visit (x, c_value()) for x in rands]
                return nodes.application (rator, rands)
        elif exp.is_a ('cexp'):
            # XXX someday, support constant folding with %%cexp
            #     (maybe by allowing the expression of patterns?)
            rands = [ self.I (rand, c_value(), env) for rand in exp.args ]
            return nodes.cexp (exp.form, exp.type_sig, rands)
        elif exp.is_a ('primapp'):
            rands = [ self.I (rand, c_value(), env) for rand in exp.args ]
            return nodes.primapp (exp.name, rands)
        elif exp.is_a ('let_splat'):
            rator = nodes.function ('<inlined_let>', exp.names, exp.body)
            rands = exp.inits
            return self.I (nodes.application (rator, rands), ctx, env)
        elif exp.is_a ('function'):
            if is_a (ctx, c_test):
                return lit_true
            elif is_a (ctx, c_effect):
                return lit_undef
            elif is_a (ctx, c_value):
                # extend env with mappings of the formals
                formals = []
                rib = []
                for f in exp.formals:
                    x0 = self.get_var (f)
                    renamed = gensym (f.name)
                    x1 = var (renamed, None, set(), set())
                    rib.append ((x0, x1))
                    formals.append (x1)
                body = self.I (exp.body, ctx, (rib, env))
                return nodes.function (exp.name, formals, body)
            elif is_a (ctx, c_app):
                return self.fold_lambda (exp, ctx, env)
            else:
                raise ValueError
        elif exp.is_a ('varref'):
            if is_a (ctx, c_effect):
                return lit_undef
            else:
                v = self.lookup (env, exp.var)
                if v.op is None or 'assign' in v.sflags:
                    v.rflags.add ('ref')
                    return nodes.varref (v.name)
                else:
                    e = self.visit (v.op, c_value())
                    return self.copy (v, self.result (e))
        elif exp.is_a ('fix'):
            rands = [ operand (x, env, 'unvisited') for x in exp.inits ]
            formals = []
            rib = []
            for f in exp.names:
                x0 = self.get_var (f)
                renamed = gensym (f.name)
                x1 = var (renamed, None, set(), set())
                rib.append ((x0, x1))
                formals.append (x1)
            ctx1 = c_app (rands, ctx, set())
            body = self.I (exp.body, ctx1, (rib, env))
            trace()
            if 'inlined' in ctx1.flags:
                return body
            else:
                rands = [self.visit (x, c_value()) for x in rands]
                return nodes.fix (formals, rands, body)
        else:
            raise ValueError

    # (letrec ((id (lambda (x) x)) (ignored (lambda (z) 3))) (id 9))

    def get_var (self, v):
        assert (is_a (v, nodes.vardef))
        if v.inline:
            return v.inline
        else:
            v.inline = var (v, None, set(), set())
            return v.inline

    def lookup (self, env, v):
        print 'LOOKUP %r' % (v,)
        while env:
            rib, env = env
            for x0, x1 in rib:
                if x0.name.name == v.name:
                    print 'RETURNING %r' % (x1,)
                    return x1
        print 'DEFAULTING LOOKUP to source var %r' % (v.inline,)
        trace()
        return v.inline

    def fold_lambda (self, exp, ctx, env):
        formals = []
        rib = []
        for f in exp.formals:
            name = f.name
            x0 = self.get_var (f)
            renamed = gensym (name)
            x1 = var (renamed, None, set(), set())
            rib.append ((x0, x1))
            formals.append (x1)
        body = self.I (exp.body, ctx.ctx, (rib, env))
        # now see how the variables were used
        rands = []
        untouched = []
        for i in range (len (formals)):
            f = formals[i]
            if 'ref' not in f.rflags and 'assign' not in f.rflags:
                untouched.append ((f, self.visit (ctx.rands[i], c_effect())))
            elif 'ref' not in f.rflags and 'assign' in f.rflags:
                rands.append ((f, self.visit (ctx.rands[i], c_effect())))
            else:
                rands.append ((f, self.visit (ctx.rands[i], c_value())))
        # flag it as inlined
        print "FLAGGING %r as inlined" % (ctx,)
        ctx.flags.add ('inlined')
        # (begin <unused_arg0> <unused_arg1> ... [(<app> <used_arg>, ...)])
        if len(rands):
            app = nodes.let_splat (
                [f.name for f,v in rands],
                [v for f,v in rands],
                body
                )
        else:
            app = body
        if len(untouched):
            items = [ v for f,v in untouched ]
            items.append (app)
            return self.seq (items)
        else:
            return app

gensym_counter = 0
def gensym (name):
    global gensym_counter
    r = gensym_counter
    gensym_counter += 1
    return nodes.vardef ('%s_%d' % (name, r))

def read_string (s):
    import cStringIO
    import lisp_reader
    sf = cStringIO.StringIO (s)
    r = lisp_reader.reader (sf)
    return r.read()

def test (s):
    import compile
    import transform
    import nodes
    from pprint import pprint as pp
    global tvar_counter
    c = compile.context()
    tvar_counter = -1
    exp = read_string (s)
    t = transform.transformer (1, c)
    exp2 = t.go ([exp])
    w = nodes.walker()
    exp3 = w.go (exp2)
    # alpha conversion
    var_dict = nodes.rename_variables (exp3, None)
    i = inliner()
    exp4 = i.go (exp3)
    exp4.pprint()

W = sys.stderr.write

if __name__ == '__main__':
    # interactive test mode
    while 1:
        sys.stdout.write ('> ')
        line = raw_input()
        if not line:
            break
        else:
            test (line)
