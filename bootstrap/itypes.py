# -*- Mode: Python; coding: utf-8 -*-

is_a = isinstance

class TypeError (Exception):
    pass

# 'itypes' since 'types' is a standard python module

# The base class includes support for the union-find algorithm
class _type:
    pending = False
    mv = None

    def __init__ (self):
        self.parent = self
        self.rank = 0

    def __iter__ (self):
        return walk_type (self)

    # for union-find
    def find (self):
        if self.parent is self:
            return self
        else:
            self.parent = self.parent.find()
            return self.parent

    # for union-find
    def union (x, y):
        xroot = x.find()
        yroot = y.find()
        if xroot.rank > yroot.rank:
            yroot.parent = xroot
        elif xroot is not yroot:
            xroot.parent = yroot
            if xroot.rank == yroot.rank:
                yroot.rank = yroot.rank + 1
        return xroot, yroot

    # subtyping (used?)
    def sub (self, other):
        return False

# XXX unification would be simpler if all base types were done as no-arg predicates.

class t_base (_type):
    name = 'base'
    code = 'b'
    def __init__ (self):
        _type.__init__ (self)
        self.rank = 1000
    def __cmp__ (self, other):
        return cmp (self.__class__, other.__class__)
    def __repr__ (self):
        return self.name
    def __hash__ (self):
        return hash (self.name)
    def sub (self, other):
        # cheap implementation of the subtyping relationship
        return issubclass (self.__class__, other.__class__)

class t_int (t_base):
    name = 'int'

class t_char (t_base):
    name = 'char'

class t_string (t_base):
    name = 'string'

class t_undefined (t_base):
    name = 'undefined'

# XXX may use product() instead...
class t_unit (t_base):
    name = 'unit'

# meant only for (vector int16)
class t_int16 (t_int):
    name = 'int16'

base_types = {
    'int' : t_int(),
    # bool now has a proper datatypes
    #'bool' : t_bool(),
    'char' : t_char(),
    'string' : t_string(),
    'undefined' : t_undefined(),
    'unit': t_unit(),
    # now a proper datatype
    #'symbol' : t_symbol(),
    'int16' : t_int16(),
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
    counter = 1
    in_u = False
    node = None
    code = 'v'
    mv = None
    def __init__ (self):
        _type.__init__ (self)
        self.id = t_var.counter
        t_var.counter += 1
    def __repr__ (self):
        return base_n (self.id, len(self.letters), self.letters)

class t_predicate (_type):
    code = 'p'
    def __init__ (self, name, args):
        _type.__init__(self)
        self.rank = 500
        self.name = name
        if self.name in ('rlabel', 'rdefault'):
            self.code = 'R'
        self.args = tuple (args)
    def __repr__ (self):
        # special cases
        if self.name == 'arrow':
            if len(self.args) == 2:
                return '(%r->%r)' % (self.args[1], self.args[0])
            else:
                return '(%r->%r)' % (self.args[1:], self.args[0])
        elif self.name == 'rproduct' and len(self.args) == 1:
            return '{%s}' % (rlabels_repr (self.args[0]),)
        elif len(self.args) == 0:
            return self.name
        else:
            return '%s(%s)' % (self.name, ', '.join ([repr(x) for x in self.args]))

def walk_type (t):
    yield t
    if is_a (t, t_predicate):
        for arg in t.args:
            for x in walk_type (arg):
                yield x


# this still doesn't do a very good job...
def rlabels_repr (t):
    r = []
    etc = False
    while 1:
        if is_pred (t, 'rlabel'):
            lname, ltype, t = t.args
            if is_pred (ltype, 'pre'):
                # normal case
                r.append ('%s=%r' % (lname, ltype.args[0]))
            elif is_pred (ltype, 'abs'):
                r.append ('%s=#f' % (lname,))
            else:
                etc = True
                break
        elif is_pred (t, 'rdefault'):
            if is_pred (t.args[0], 'abs'):
                # normal case
                break
            else:
                r.append (repr (t))
                break
        else:
            etc = True
            break
    r.sort()
    if etc:
        r.append ('...')
    return ' '.join (r)

def is_pred (t, *p):
    # is this a predicate from the set <p>?
    return is_a (t, t_predicate) and t.name in p

def arrow (*sig):
    # sig = (<result_type>, <arg0_type>, <arg1_type>, ...)
    # XXX this might be more clear as (<arg0>, <arg1>, ... <result>)
    return t_predicate ('arrow', sig)
    
def continuation (*sig):
    return t_predicate ('continuation', sig)

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

# used to represent equirecursive types ('moo' is a pun on the 'μ' notation).
def moo (tv, x):
    return t_predicate ('moo', (tv, x))

def get_record_sig (t):
    if is_pred (t, 'moo'):
        t0 = t
        t = t.args[1]
    # rproduct (rlabel (...))
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
        elif is_pred (t, 'moo'):
            # follow the moo!
            t = t.args[1]
        elif is_a (t, t_var):
            labels.append ('...')
            break
        else:
            return None
    labels.sort()
    return tuple (labels)

# an algebraic datatype
class datatype:

    # this is a hack to get the predefined tags for certain datatypes.
    # not sure what else would go in here...
    # XXX need a way to stop a user from defining a 'whacky' list type.
    builtin_tags = {
        'list': {'cons':'TC_PAIR', 'nil':'TC_NIL'},
        'bool': {'true':'PXLL_TRUE', 'false':'PXLL_FALSE'},
        'symbol': {'t':'TC_SYMBOL'},
        }

    def __init__ (self, context, name, alts, tvars):
        self.context = context
        self.name = name
        self.alts = alts
        self.tvars = tvars.values()
        self.scheme = t_predicate (name, self.tvars)
        self.constructors = {}
        self.tags = {}
        tags = self.builtin_tags.get (name, None)
        for i in range (len (alts)):
            # assign runtime tags in the order they're defined.
            # [other choices would be to sort alphabetically, and/or
            #  immediate vs tuple]
            altname, prod = alts[i]
            self.constructors[altname] = prod
            if tags:
                self.tags[altname] = tags[altname]
            else:
                self.tags[altname] = i
        self.uimm = self.optimize()
            
    def order_alts (self, alts):
        r = [None] * len (alts)
        for i in range (len (alts)):
            tag, alt = alts[i]
            r[self.tags[tag]] = alt
        return r

    def arity (self, alt):
        return len (self.constructors[alt])

    def optimize (self):
        # scan for the single-immediate optimization.
        # identify all single-item alternatives that hold
        #   an immediate type that we can discern with a
        #   run-time tag.
        # for example:
        #
        # (datatype thing (:number int) (:letter char))
        #
        # can be represented with no runtime overhead, because
        # both alternatives can be a simple immediate.

        if self.name == 'symbol':
            # XXX In this *particular* case, we want to avoid the UIMM
            #   hack, because the runtime knows about symbols already.
            return {}

        good = {}
        bad = set()
        for tag, prod in self.alts:
            if len(prod) == 0:
                # enums break this optimization, because it makes it
                #  impossible for the typecode function to distinguish
                #  between user immediates and builtin immediates.
                return {}
            elif len(prod) == 1 and is_a (prod[0], t_base):
                ty = prod[0]
                if good.has_key (ty):
                    bad.add (ty)
                else:
                    good[ty] = tag
        # take out the bad apples
        for ty in bad:
            del good[ty]
        uimm = {}
        for k, v in good.iteritems():
            uimm[v] = k
        return uimm
