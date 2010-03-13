# -*- Mode: Python; coding: utf-8 -*-

is_a = isinstance

# 'itypes' since 'types' is a standard python module

class _type:
    pass

class t_base (_type):
    name = 'base'
    def __cmp__ (self, other):
        return cmp (self.__class__, other.__class__)
    def __repr__ (self):
        return self.name
    def __hash__ (self):
        return hash (self.name)

class t_int (t_base):
    name = 'int'

class t_char (t_base):
    name = 'char'

class t_string (t_base):
    name = 'string'

class t_symbol (t_base):
    name = 'symbol'

# XXX consider using a true/false variant, then implementing 'if' as a filter.
class t_bool (t_base):
    name = 'bool'

class t_undefined (t_base):
    name = 'undefined'

class t_continuation (t_base):
    name = 'continuation'

# XXX may use product() instead...
class t_unit (t_base):
    name = 'unit'

base_types = {
    'int' : t_int(),
    'bool' : t_bool(),
    'char' : t_char(),
    'string' : t_string(),
    'undefined' : t_undefined(),
    'unit': t_unit(),
    'continuation' : t_continuation(),
    'symbol' : t_symbol(),
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
    in_u = False
    def __init__ (self):
        self.id = t_var.counter
        t_var.counter += 1
    def __repr__ (self):
        return base_n (self.id, len(self.letters), self.letters)
        #return '%s.%d' % (r, self.rank)

class t_predicate (_type):
    def __init__ (self, name, args):
        self.name = name
        self.args = tuple (args)
    def __repr__ (self):
        # special case
        if self.name == 'arrow':
            if len(self.args) == 2:
                return '(%r->%r)' % (self.args[1], self.args[0])
            else:
                return '(%r->%r)' % (self.args[1:], self.args[0])
        else:
            return '%s(%s)' % (self.name, ', '.join ([repr(x) for x in self.args]))

def is_pred (t, *p):
    # is this a predicate from the set <p>?
    return is_a (t, t_predicate) and t.name in p

def arrow (*sig):
    # sig = (<result_type>, <arg0_type>, <arg1_type>, ...)
    # XXX this might be more clear as (<arg0>, <arg1>, ... <result>)
    return t_predicate ('arrow', sig)
    
def vector (type):
    return t_predicate ('vector', (type,))

def product (*args):
    # a.k.a. 'Π'
    return t_predicate ('product', args)

def sum (*args):
    # a.k.a. 'Σ'
    # args = ((<tag>, <type>), ...)
    args.sort()
    return t_predicate ('sum', args)

# row types
def rproduct (*args):
    return t_predicate ('rproduct', args)

def rsum (row):
    return t_predicate ('rsum', (row,))

def rdefault (arg):
    # a.k.a. 'δ'
    return t_predicate ('rdefault', (arg,))

def rlabel (name, type, rest):
    return t_predicate ('rlabel', (name, type, rest))

def abs():
    return t_predicate ('abs', ())

def pre (x):
    return t_predicate ('pre', (x,))

# place holder for information about datatypes
class datatype:

    def __init__ (self, context, name, alts, tvars):
        self.context = context
        self.name = name
        self.alts = alts
        self.tvars = tvars.values()
        self.scheme = t_predicate (name, self.tvars)
        self.constructors = {}
        self.tags = {}
        for i in range (len (alts)):
            # assign runtime tags in the order they're defined.
            # [other choices would be to sort alphabetically, and/or
            #  immediate vs tuple]
            tag, prod = alts[i]
            self.constructors[tag] = prod
            self.tags[tag] = i
            
    def order_alts (self, alts):
        r = [None] * len (alts)
        for i in range (len (alts)):
            tag, alt = alts[i]
            r[self.tags[tag]] = alt
        return r
