# -*- Mode: Python; coding: utf-8 -*-

is_a = isinstance

# 'itypes' since 'types' is a standard python module

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

base_types = {
    'int' : t_int(),
    'bool' : t_bool(),
    'char' : t_char(),
    'str' : t_str(),
    }

def base_n (n, base, digits):
    # return a string representation of <n> in <base>, using <digits>
    s = []
    while 1:
        n, r = divmod (n, base)
        s.insert (0, digits[r])
        if not n:
            break
    return ''.join (s)

class t_var (_type):
    next = None
    rank = -1
    letters = 'abcdefghijklmnopqrstuvwxyz'
    eq = None
    counter = 0
    def __init__ (self):
        self.id = t_var.counter
        t_var.counter += 1
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

def parse_cexp_type (t):
    if is_a (t, tuple):
        result_type, arg_types = t
        return arrow (parse_cexp_type (result_type), *[parse_cexp_type (x) for x in arg_types])
    elif is_a (t, str):
        return base_types[t]
    else:
        raise ValueError (t)
        
