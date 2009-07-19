# -*- Python -*-

# Having a go at the constraint-based inference algorithm described
#  by Pottier and Remy in ATTPL.

# the simply typed lambda calculus:
#   e ::= x | \x.e | e e
# 
# expressions:
# x    : <varref>
# \x.e : <function>
# e e  : <application>

# types
# t ::= a | (arrow t t)
# (where a = <tvar>)

# constraints:
# C ::= (equals t t) | (and C C) | (exists a C)

# types

class type:
    pass

tvar_counter = -1

def fresh():
    global tvar_counter
    result = tvar_counter
    tvar_counter -= 1
    return result


class tvar:
    def __init__ (self):
        self.id = fresh()

    def __repr__ (self):
        return 'T%d' % (-self.id,)

class arrow:
    def __init__ (self, t0, t1):
        self.t0 = t0
        self.t1 = t1

    def __repr__ (self):
        return '%r -> %r' % (self.t0, self.t1)
    
# constraints

class constraint:
    kind = 'abstract'
    args = ()

    def __repr__ (self):
        return '(%s %s)' % (self.kind, ' '.join ([repr(x) for x in self.args]))

class c_equals (constraint):
    def __init__ (self, t0, t1):
        self.kind = '='
        self.args = (t0, t1)

class c_and (constraint):
    def __init__ (self, c0, c1):
        self.kind = 'and'
        self.args = (c0, c1)
        
class c_exists (constraint):
    def __init__ (self, vars, sub):
        self.kind = 'exists'
        self.args = (vars, sub)

# this is a two-phase algorithm
# 1) constraint generation
# 2) constraint solving

class typer:

    def __init__ (self, verbose=False):
        pass

    def go (self, exp):
        env = {}
        t = tvar()
        x = self.generate (env, exp, t)
        return x

    def generate (self, env, exp, t):
        if exp.is_a ('varref'):
            return c_equals (env[exp.name], t)
        elif exp.is_a ('function'):
            a1 = tvar()
            a2 = tvar()
            # a dict will suffice because we've alpha-converted <exp>
            env[exp.formals[0].name] = a1
            sub0 = self.generate (env, exp.body, a2)
            sub1 = c_equals (arrow (a1, a2), t)
            return c_exists ((a1, a2), c_and (sub0, sub1))
        elif exp.is_a ('application'):
            e1 = exp.rator
            e2 = exp.rands[0]
            a = tvar()
            sub0 = self.generate (env, e1, arrow (a, t))
            sub1 = self.generate (env, e2, a)
            return c_exists ((a,), c_and (sub0, sub1))
        elif exp.is_a ('literal'):
            return c_equals (exp.type, t)
        else:
            raise ValueError

def read_string (s):
    import cStringIO
    import lisp_reader
    sf = cStringIO.StringIO (s)
    r = lisp_reader.reader (sf)
    return r.read()

def test (s):
    import transform
    import lambda_tree
    from pprint import pprint as pp
    global tvar_counter
    tvar_counter = -1
    exp = read_string (s)
    t = transform.transformer (1)
    exp2 = t.go ([exp])
    w = lambda_tree.walker()
    exp3 = w.go (exp2)
    # alpha conversion
    var_dict = lambda_tree.rename_variables (exp3)
    t = typer()
    pp (t.go (exp3))

test ("(lambda (x) x)")
test ("((lambda (x) x) (lambda (x) x))")
test ("((lambda (x) x) (lambda (y) y))")
test ("((lambda (x) x) 3)")

