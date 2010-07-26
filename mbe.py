# -*- Mode: Python -*-

# a quick translation of Dorai Sitaram's CL code.
# Note: there is no hygiene here.

is_a = isinstance
from pdb import set_trace as trace
from pprint import pprint as pp

def is_symbol (x):
    return is_a (x, str)

def is_list (x):
    # DS' code checks that the list has a proper end.
    return is_a (x, list)

def is_ellipsis (x):
    # [<pattern>, '...']
    return is_a (x, list) and len(x) >= 2 and x[1] == '...'

class MatchError (Exception):
    pass

def matches_pattern (p, e, k):
    if is_ellipsis (p):
        if len (p) != 2:
            raise MatchError (p)
        if is_list (e):
            p0 = p[0]
            for e_i in e:
                if not matches_pattern (p0, e_i, k):
                    return False
            else:
                return True
        return False
    elif is_a (p, list) and len(p): # consp
        return (
            is_a (e, list)
            and matches_pattern (p[0], e[0], k)
            and matches_pattern (p[1:], e[1:], k)
            )
    elif is_symbol (p):
        if p in k:
            return p == e
        else:
            return True
    else:
        return p == e

def get_ellipsis_nestings (p, k):
    def sub (p):
        if is_ellipsis (p):
            return [sub(p[0])] + sub(p[2:])
        elif is_list (p) and len(p):
            return sub(p[0]) + sub(p[1:])
        elif is_symbol (p):
            if p in k:
                return []
            else:
                return [p]
        else:
            return []
    return sub(p)

def ellipsis_sub_envs (nestings, r):
    for k,v in r:
        if intersect (nestings, k):
            return v
    else:
        return []

def intersect (v, y):
    if is_symbol (v) or is_symbol (y):
        return v == y
    else:
        for v_i in v:
            for y_j in y:
                if intersect (v_i, y_j):
                    return True
        return False

# this could probably be done with a dictionary, I'm just not 100% certain that the ability
#  to extend it piecemeal is not needed.  As written it returns a lisp-style 'association list',
#  like this: [(<key0>, <val0>), (<key1>, <val1>), ...]

def get_bindings (p, e, k):
    if is_ellipsis (p):
        return [(get_ellipsis_nestings (p[0], k), [ get_bindings (p[0], e_i, k) for e_i in e ])]
    elif is_a (p, list) and len(p):
        return get_bindings (p[0], e[0], k) + get_bindings (p[1:], e[1:], k)
    elif is_symbol (p):
        if p in k:
            return []
        else:
            return [(p, e)] # extend binding environment
    else:
        return []

def assoc (key, pairs):
    for k, v in pairs:
        if key == k:
            return v
    return False

def expand_pattern (p, r, k):
    # p = pattern
    # r = var bindings
    # k = keywords
    if is_ellipsis (p):
        p0 = p[0]
        nestings = get_ellipsis_nestings (p0, k)
        rr = ellipsis_sub_envs (nestings, r)
        rr = [expand_pattern (p0, r_i + r, k) for r_i in rr]
        return rr + expand_pattern (p[2:], r, k)
    elif is_a (p, list) and len(p):
        return [expand_pattern (p[0], r, k)] + expand_pattern (p[1:], r, k)
    elif is_symbol (p):
        if p in k:
            return p
        else:
            probe = assoc (p, r)
            if probe:
                return probe
            else:
                return p
    else:
        return p

class macro:
    def __init__ (self, name, patterns):
        self.name = name
        self.patterns = patterns

    def apply (self, exp):
        k = []
        for in_pat, out_pat in self.patterns:
            if matches_pattern (in_pat, exp, k):
                r = get_bindings (in_pat, exp, k)
                r = expand_pattern (out_pat, r, k)
                #pp (r)
                #trace()
                return r
        else:
            raise MatchError ("no matching clause", exp)

def t0():
    if False:
        print matches_pattern ([1, 'x', 2], [1, 23, 2], [])
        print get_bindings ([1, 'x', ['y']], [1, 23, [2]], [])
        print get_bindings ([1, 'x', ['y']], [1, 23, ['bibble']], [])
        print get_bindings ([1, 'x', ['y']], [1, 23, [['a', 'b', 'c']]], [])
        print matches_pattern ([1, '...'], [1, 1, 1, 1], [])
        print get_bindings (['x', '...'], [1, 2, 3, 4], [])
        print get_bindings (['thing', ['x', '...'], ['y']], ['thing', [1, 2, 3, 4], [100]], [])
        print get_bindings (['thing', ['x', 'y'], '...'], ['thing', [1, 2], [3, 4], [5, 6]], [])

    # => ((x ...) (y ...))
    # ((THING . THING) ((X Y) ((X . 1) (Y . 2)) ((X . 3) (Y . 4)) ((X . 5) (Y . 6))))
    # [('thing', 'thing'), [
    #print expand_pattern ([['x', '...'], ['y', '...']], [([['x'], ['y']], [[('x', 1), ('y', 2)], [('x', 3), ('y', 4)], [('x', 5), ('y', 6)]])], [])
    print expand_pattern (['thing', ['x', '...'], ['y', '...']], [('thing', 'thing'), (['x', 'y'], [[('x', 1), ('y', 2)], [('x', 3), ('y', 4)], [('x', 5), ('y', 6)]])], [])
    # (1, x, 2) => (1, x, x, 2)
    #print expand_pattern ([1, 'x', 'x', 2], [('x', ['a', 'b'])], [])
    #print expand_pattern (['blort', 'x', '...'], [(['x'], [[('x', 1)], [('x', 2)], [('x', 3)], [('x', 4)]])], [])
    
if __name__ == '__main__':
    t0()
