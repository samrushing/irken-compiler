# -*- Mode: Python -*-

# a quick translation of Dorai Sitaram's CL code.
# Note: there is no hygiene here.

is_a = isinstance

def is_symbol (x):
    return is_a (x, str)

def is_list (x):
    # Dorai's code checks that the list has a proper end.
    return is_a (x, list)

def is_ellipsis (x):
    # [<pattern>, '...']
    return is_a (x, list) and len(x) >= 2 and x[1] == '...'

class MatchError (Exception):
    pass

def matches_pattern (p, e):
    if is_ellipsis (p):
        if len (p) != 2:
            raise MatchError (p)
        if is_list (e):
            p0 = p[0]
            for e_i in e:
                if not matches_pattern (p0, e_i):
                    return False
            else:
                return True
        return False
    elif is_a (p, list) and len(p): # consp
        return (
            is_a (e, list)
            and matches_pattern (p[0], e[0])
            and matches_pattern (p[1:], e[1:])
            )
    elif is_symbol (p):
        if p[0] == '<' and p[-1] == '>':
            return e == p[1:-1]
        else:
            return True
    else:
        return p == e

def get_ellipsis_nestings (p):
    def sub (p):
        if is_ellipsis (p):
            # XXX I think the second call to sub here is pointless since '... should
            #     always be the last element of any list.
            return [sub(p[0])] + sub(p[2:])
        elif is_list (p) and len(p):
            return sub(p[0]) + sub(p[1:])
        elif is_symbol (p):
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
    # XXX I think this should be <and>, not <or>.
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

def get_bindings (p, e):
    if is_ellipsis (p):
        return [(get_ellipsis_nestings (p[0]), [ get_bindings (p[0], e_i) for e_i in e ])]
    elif is_a (p, list) and len(p):
        return get_bindings (p[0], e[0]) + get_bindings (p[1:], e[1:])
    elif is_symbol (p):
        return [(p, e)] # extend binding environment
    else:
        return []

def assoc (key, pairs):
    for k, v in pairs:
        if key == k:
            return v
    return False

gensym_counter = 0

def gensym (prefix='g'):
    global gensym_counter
    r = gensym_counter
    gensym_counter += 1
    return '%s%d' % (prefix, r)

def expand_pattern (p, r):
    # p = pattern
    # r = var bindings
    newsyms = {}
    if is_ellipsis (p):
        p0 = p[0]
        nestings = get_ellipsis_nestings (p0)
        rr = ellipsis_sub_envs (nestings, r)
        rr = [expand_pattern (p0, r_i + r) for r_i in rr]
        return rr + expand_pattern (p[2:], r)
    elif is_a (p, list) and len(p):
        return [expand_pattern (p[0], r)] + expand_pattern (p[1:], r)
    elif is_symbol (p):
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

    def gen_syms (self, out_pat):
        "replace all $var in output using gensym"
        newsyms = {}
        def p (exp):
            if is_a (exp, list):
                return [ p(x) for x in exp ]
            elif is_a (exp, str):
                if exp.startswith ('$'):
                    if newsyms.has_key (exp):
                        r = newsyms[exp]
                    else:
                        r = gensym ('mbe_' + exp[1:] + '_')
                        newsyms[exp] = r
                    return r
                else:
                    return exp
            else:
                return exp
        return p (out_pat)

    def apply (self, exp):
        for in_pat, out_pat in self.patterns:
            if matches_pattern (in_pat, exp):
                r = get_bindings (in_pat, exp)
                return expand_pattern (self.gen_syms (out_pat), r)
        else:
            raise MatchError ("no matching clause", exp)

def t0():
    print get_ellipsis_nestings (['a', ['c', 'd'], 'd', ['b', ['x', '...']]])
    print get_ellipsis_nestings ([[['a', 'b'], '...'], '...'])
    print matches_pattern ([1, 'x', 2], [1, 23, 2])
    print get_bindings ([1, 'x', ['y']], [1, 23, [2]])
    print get_bindings ([1, 'x', ['y']], [1, 23, ['bibble']])
    print get_bindings ([1, 'x', ['y']], [1, 23, [['a', 'b', 'c']]])
    print matches_pattern ([1, '...'], [1, 1, 1, 1])
    print get_bindings (['x', '...'], [1, 2, 3, 4])
    print get_bindings (['thing', ['x', '...'], ['y']], ['thing', [1, 2, 3, 4], [100]])
    print get_bindings (['thing', ['x', 'y'], '...'], ['thing', [1, 2], [3, 4], [5, 6]])
    print get_bindings ([[['x', 'y'], '...'], '...'], [[[1,2],[3,4]],[[5,6],[7,8]]])
    # => ((x ...) (y ...))
    # ((THING . THING) ((X Y) ((X . 1) (Y . 2)) ((X . 3) (Y . 4)) ((X . 5) (Y . 6))))
    # [('thing', 'thing'), [
    p = ['thing', ['x', '...'], ['y']]
    n = get_ellipsis_nestings (p)
    print n
    b = get_bindings (p, ['thing', [1, 2, 3, 4], [100]])
    print b
    rr = ellipsis_sub_envs (n, b)
    print rr
    print expand_pattern (['zorble', 'x', '...'], b)
    return
    print expand_pattern (['thing', ['x', '...'], ['y', '...']], [('thing', 'thing'), (['x', 'y'], [[('x', 1), ('y', 2)], [('x', 3), ('y', 4)], [('x', 5), ('y', 6)]])])
    # (1, x, 2) => (1, x, x, 2)
    print expand_pattern ([1, 'x', 'x', 2], [('x', ['a', 'b'])])
    print expand_pattern (['blort', 'x', '...'], [(['x'], [[('x', 1)], [('x', 2)], [('x', 3)], [('x', 4)]])])
    
if __name__ == '__main__':
    t0()
