# -*- Mode: Python -*-

# lambda language

from pdb import set_trace as trace

# used to generate a unique identifier for each node.

class serial_counter:

    def __init__ (self):
        self.counter = 0

    def next (self):
        result = self.counter
        self.counter += 1
        return result

serial = serial_counter()

# node types

class node:

    # The initial implementation of the node class did the 'right' thing,
    #   by subclassing <node> to get each of the individual node types.
    # Then of course I started using all these nice attributes.  For example,
    #   I would have a 'calls' attribute on each function that would track
    #   every time it was called.
    # Once I started writing code that transformed this tree of nodes, however,
    #   this all came back to bite me.  The problem is that you need to be able
    #   to safely *copy* trees of these objects. Otherwise you get dangling
    #   pointers and lists, etc... even simple transformations were coming out
    #   completely mangled.  It's *really* made me appreciate the ideas of pure
    #   functional data structures!
    # So, this was rewritten with <kind>, <params>, and <subs>.  Clumsy, but easy
    #   to copy and rewrite.  Accessing these attributes is a real pain, but I
    #   don't have to worry about losing things, or surprises.  Also, walking the
    #   tree of nodes is much simpler.
    # However.  The rest of the compiler doesn't want to be rewritten in this way,
    #   so the last thing we do before handing off to cps.py is to add a bunch of
    #   attributes to each node, in fix_attribute_names().
    # I plan to fix this when it becomes either too clumsy or too embarrassing.
    #
    # XXX Pyrex solves this interestingly... it uses a special attribute to list
    #     the *names* of attributes that refer to sub-expressions.  I think I
    #     considered this and discarded it because the 'set of all
    #     sub-expressions' is still difficult to synthesize, in cases where one
    #     attribute might hold a single expression, and others might hold sets
    #     of them.

    # generic flag
    flag = False
    leaf = False
    constructor = False
    escapes = False

    def __init__ (self, kind, params=(), subs=(), type=None):
        self.kind = kind
        self.params = params
        # XXX consider making this tuple(subs)
        self.subs = subs
        self.serial = serial.next()
        size = 1
        for sub in subs:
            size += sub.size
        self.size = size
        self.type = type
        self.fix_attribute_names()

    def pprint (self, depth=0):
        if self.leaf:
            leaf = 'L'
        else:
            leaf = ' '
        print '%3d %s' % (self.serial, leaf),
        print '  ' * depth, self.kind,
        print '[%d]' % (self.size,),
        if self.type:
            print '%s ' % (self.type,),
        else:
            print '? ',
        if self.params:
            print self.params
        else:
            print
        for sub in self.subs:
            sub.pprint (depth+1)

    def __repr__ (self):
        if self.params:
            return '<%s %r %d>' % (self.kind, self.params, self.serial)
        else:
            return '<%s %d>' % (self.kind, self.serial)

    def __iter__ (self):
        return walk_node (self)

    def is_a (self, kind):
        return self.kind == kind

    def one_of (self, *kinds):
        return self.kind in kinds

    def is_var (self, name):
        return self.kind == 'varref' and self.params == name

    def copy (self):
        return node (self.kind, self.params, self.subs, self.type)

    def deep_copy (self):
        # XXX ugliness.  because self.params is sometimes a list, it would behoove
        #   us to use a copy of that list!  However it's not always a list.
        if is_a (self.params, list):
            params = self.params[:]
        else:
            params = self.params

        r = node (self.kind, params, [ x.deep_copy() for x in self.subs ], self.type)
        
        # special-case: binding positions are not nodes or sub-expressions, but
        #   we want fresh copies of them as well...
        if r.binds():
            binds = [ vardef (x.name, x.type) for x in r.get_names() ]
            if self.is_a ('let_splat'):
                r.params = binds
            elif self.is_a ('fix'):
                r.params = binds
                # update function attributes
                for i in range (len (binds)):
                    if r.subs[i].is_a ('function'):
                        binds[i].function = r.subs[i]
            elif self.is_a ('function'):
                # function
                r.params[1] = binds
            else:
                raise ValueError ("new binding construct?")
        return r

    def binds (self):
        return self.kind in ('let_splat', 'function', 'fix')

    def get_names (self):
        if self.kind == 'function':
            return self.params[1]
        elif self.kind in ('let_splat', 'fix'):
            return self.params
        else:
            raise ValueError ("get_names() not valid for this node")

    def get_body (self):
        # get the body of an expression
        if self.one_of ('let_splat', 'fix', 'function'):
            return self.subs[-1]
        else:
            return self

    def get_rator (self):
        assert (self.kind == 'application')
        return self.subs[0]

    def get_rands (self):
        assert (self.kind == 'application')
        return self.subs[1:]

    def fix_attribute_names (self):
        if self.kind == 'varref':
            self.name = self.params
        elif self.kind == 'varset':
            self.name = self.params
            self.value = self.subs[0]
        elif self.kind == 'literal':
            self.ltype, self.value = self.params
        elif self.kind == 'constructed':
            self.value = self.params
        elif self.kind == 'primapp':
            self.name = self.params
            self.args = self.subs
        elif self.kind == 'sequence':
            self.exprs = self.subs
        elif self.kind == 'cexp':
            self.form, self.type_sig = self.params
            self.args = self.subs
        elif self.kind == 'verify':
            self.tc, self.safety = self.params
            self.arg = self.subs[0]
        elif self.kind == 'conditional':
            [self.test_exp, self.then_exp, self.else_exp] = self.subs
        elif self.kind == 'function':
            self.name, self.formals, self.recursive, self.type = self.params
            self.body = self.subs[0]
        elif self.kind == 'fix':
            self.names = self.params
            self.inits = self.subs[:-1]
            self.body = self.subs[-1]
        elif self.kind == 'let_splat':
            self.names = self.params
            self.inits = self.subs[:-1]
            self.body = self.subs[-1]
        elif self.kind == 'let_subst':
            self.vars = self.params
            self.body = self.subs[0]
        elif self.kind == 'application':
            self.recursive = self.params
            self.rator = self.subs[0]
            self.rands = self.subs[1:]
        elif self.kind == 'make_tuple':
            (self.ttype, self.tag) = self.params
            self.args = self.subs
        elif self.kind == 'pvcase':
            self.alt_formals = self.params
            self.value = self.subs[0]
            self.alts = self.subs[1:]
        elif self.kind == 'nvcase':
            self.vtype, self.tags = self.params
            self.value = self.subs[0]
            self.alts = self.subs[1:-1]
            self.else_clause = self.subs[-1]
        else:
            raise ValueError (self.kind)

def walk_node (n):
    yield n
    for sub in n.subs:
        for x in walk_node (sub):
            yield x

def walk_up (n):
    for sub in n.subs:
        for x in walk_node (sub):
            yield x
    yield n

# this is *not* a node!
class vardef:
    tvar = False
    def __init__ (self, name, type=None):
        if is_a (name, str):
            self.name = name
            self.type = type
        elif is_a (name, list) and len(name) == 3 and name[0] == 'colon':
            # infix colon syntax for type declaration
            assert (type is None)
            self.name = name[1]
            self.type = parse_type (name[2])
        elif is_a (name, list) and len(name) == 2 and name[0] == 'quote':
            # type variable argument
            self.tvar = True
            self.name = name[1]
            self.type = None
        self.assigns = []
        self.refs = []
        self.function = None
        self.serial = serial.next()
        self.escapes = False
        self.inline = None
        self.alias = None
        
    def __repr__ (self):
        #return '{%s.%d}' % (self.name, self.serial)
        if self.type:
            return '{%s:%s}' % (self.name, self.type)
        else:
            return '{%s}' % (self.name,)
            #return '{%s.%d}' % (self.name, len(self.assigns))

def varref (name):
    return node ('varref', name)

def varset (name, value):
    return node ('varset', name, [value])

def literal (kind, value):
    return node ('literal', (kind, value))

# a literal built by constructors & immediates
def constructed (value):
    return node ('constructed', value)

def primapp (name, args):
    return node ('primapp', name, args)

def sequence (exprs):
    if not exprs:
        exprs = [literal ('undefined', 'undefined')]
    return node ('sequence', (), exprs, type=exprs[-1].type)

def cexp (form, type_sig, args):
    return node ('cexp', (form, type_sig), args)

def make_tuple (type, tag, args):
    return node ('make_tuple', (type, tag), args)

def conditional (test_exp, then_exp, else_exp):
    return node ('conditional', (), [test_exp, then_exp, else_exp])

def function (name, formals, body, type=None):
    return node ('function', [name, formals, False, type], [body])

def fix (names, inits, body, type=None):
    n = node ('fix', names, inits + [body], type)
    for i in range (len (names)):
        if inits[i].is_a ('function'):
            names[i].function = inits[i]
    return n

def let_splat (names, inits, body, type=None):
    return node ('let_splat', names, inits + [body], type)

def let_subst (vars, body):
    return node ('let_subst', vars, [body])

def application (rator, rands):
    return node ('application', False, [rator] + rands)
    
def pvcase (value, alt_formals, alts):
    return node ('pvcase', alt_formals, [value] + alts)

def nvcase (vtype, value, tags, alts, else_clause):
    return node ('nvcase', (vtype, tags), [value] + alts + [else_clause])

# ================================================================================

class ConfusedError (Exception):
    pass

import itypes

# should this be moved to transform.py?

def parse_type (exp, tvars=None):

    if tvars is None:
        tvars = {}

    def get_tvar (name):
        if not tvars.has_key (name):
            tvars[name] = itypes.t_var()
        return tvars[name]

    def pfun (x):
        if is_a (x, list):
            if len(x) and x[0] == 'quote':
                # a type variable
                return get_tvar (x[1])
            elif len(x) >= 2 and x[-2] == '->':
                # an arrow type
                result_type = pfun (x[-1])
                arg_types = tuple ([pfun(y) for y in x[:-2]])
                return itypes.arrow (result_type, *arg_types)
            elif len(x) > 0 and is_a (x[0], str):
                # a predicate
                arg_types = tuple ([pfun(y) for y in x[1:]])
                return itypes.t_predicate (x[0], arg_types)
            else:
                raise ValueError ("malformed type: %r" % (x,))
        elif is_a (x, str):
            if itypes.base_types.has_key (x):
                return itypes.base_types[x]
            else:
                raise ValueError ("unknown type: %r" % (x,))
        else:
            return x

    #print 'pfun (%r) => %r' % (exp, pfun (exp))
    return pfun (exp)

from lisp_reader import atom

is_a = isinstance

class walker:

    """The walker converts from 's-expression' => 'node tree' representation"""

    def __init__ (self, context):
        self.context = context

    def walk_exp (self, exp):
        WALK = self.walk_exp
        if is_a (exp, str):
            return varref (exp)
        elif is_a (exp, atom):
            return literal (exp.kind, exp.value)
        elif is_a (exp, list):
            rator = exp[0]
            simple = is_a (rator, str)
            if simple:
                if rator == '%%cexp':
                    assert (is_a (exp[2], atom))
                    assert (exp[2].kind == 'string')
                    tvars = {}
                    type_sig = parse_type (exp[1], tvars)
                    form = exp[2].value
                    return cexp (form, (tvars.values(), type_sig), [ WALK (x) for x in exp[3:]])
                elif rator == '%%make-tuple':
                    type = exp[1]
                    tag = exp[2]
                    args = exp[3:]
                    return make_tuple (type, tag, [ WALK (x) for x in args ] )
                elif rator.startswith ('%'):
                    return primapp (rator, [WALK (x) for x in exp[1:]])
                elif rator == 'begin':
                    return sequence ([WALK (x) for x in exp[1:]])
                elif rator == 'set_bang':
                    ignore, name, val = exp
                    return varset (name, WALK (val))
                elif rator == 'quote':
                    return literal (exp[1].kind, exp[1].value)
                elif rator == 'constructed':
                    return constructed (WALK (exp[1]))
                elif rator == 'if':
                    return conditional (WALK (exp[1]), WALK (exp[2]), WALK (exp[3]))
                elif rator == 'function':
                    fun_name, fun_type = exp[1]
                    formals = exp[2]
                    formals = [vardef (x) for x in formals]
                    return function (fun_name, formals, WALK (exp[3]), fun_type)
                elif rator == 'let_splat':
                    ignore, vars, body = exp
                    names = [vardef(x[0]) for x in vars]
                    inits = [WALK (x[1])  for x in vars]
                    return let_splat (names, inits, WALK (body))
                elif rator == 'let_subst':
                    ignore, vars, body = exp
                    return let_subst (vars, WALK (body))
                elif rator == 'fix':
                    ignore, names, inits, body = exp
                    names = [vardef (x) for x in names]
                    inits = [WALK (x)   for x in inits]
                    return fix (names, inits, WALK (body))
                elif rator == 'pvcase':
                    ignore, value, alt_formals, alts = exp
                    alt_formals = [ (selector, type, [vardef (name) for name in formals]) for selector, type, formals in alt_formals ]
                    return pvcase (WALK(value), alt_formals, [WALK (x) for x in alts])
                elif rator == 'nvcase':
                    ignore, vtype, value, alts, ealt = exp
                    dt = self.context.datatypes[vtype]
                    tags = [x[0] for x in alts]
                    alts = [x[1] for x in alts]
                    return nvcase (vtype, WALK(value), tags, [WALK (x) for x in alts], WALK(ealt))
                else:
                    # a varref application
                    return application (WALK (rator), [WALK (x) for x in exp[1:]])
            else:
                # a non-simple application
                return application (WALK (rator), [ WALK (x) for x in exp[1:]])
        else:
            raise ValueError, exp

    def go (self, exp):
        exp = self.walk_exp (exp)
        for node in exp:
            node.fix_attribute_names()
        return exp

# walk the node tree, applying subst nodes.
def apply_substs (exp):
    
    def shadow (names, lenv):
        if lenv is None:
            return lenv
        else:
            rib, tail = lenv
            rib = [(x[0], x[1]) for x in rib if x[0] not in names]
            return (rib, shadow (names, tail))

    def lookup (name, lenv):
        while lenv:
            rib, lenv = lenv
            for xf, xt in rib:
                if xf == name:
                    return xt
        return name

    def walk (exp, lenv):
        if exp.binds():
            names = [x.name for x in exp.get_names()]
            lenv = shadow (names, lenv)
        elif exp.is_a ('let_subst'):
            # filter out wildcards from match expressions
            names = [ (f, t) for (f, t) in exp.params if f != '_' ]
            lenv = (names, lenv)
            return walk (exp.subs[0], lenv)
        elif exp.one_of ('varref', 'varset'):
            exp.params = lookup (exp.params, lenv)
        # XXX urgh, destructive update bad, bad, bad.
        exp.subs = [walk (sub, lenv) for sub in exp.subs ]
        return exp
    
    return walk (exp, None)

# alpha conversion

def rename_variables (exp, context):
    vars = []
    datatypes = context.datatypes

    def lookup_var (name, lenv):
        while lenv:
            rib, lenv = lenv
            # walk rib backwards for the sake of <let*>
            #   (e.g., (let ((x 1) (x 2)) ...))
            for x in reversed (rib):
                if x.name == name:
                    return x
        if datatypes.has_key (name):
            return None
        elif name.startswith ('&'):
            return None
        else:
            raise ValueError ("unbound variable: %r" % (name,))

    # walk <exp>, inventing a new name for each <vardef>,
    #   renaming varref/varset as we go...
    def rename (exp, lenv):
        if exp.binds():
            defs = exp.get_names()
            for vd in defs:
                vd.alpha = len (vars)
                vars.append (vd)
            if exp.is_a ('let_splat'):
                # this one is tricky
                names = []
                lenv = (names, lenv)
                for i in range (len (defs)):
                    # add each name only after its init
                    init = exp.subs[i]
                    rename (init, lenv)
                    names.append (defs[i])
                # now all the inits are done, rename body
                rename (exp.subs[-1], lenv)
                # ugh, non-local exit
                return
            else:
                # normal binding behavior
                lenv = (defs, lenv)
            if exp.is_a ('fix'):
                # rename functions
                for i in range (len (defs)):
                    if exp.subs[i].is_a ('function'):
                        if not defs[i].name.startswith ('&'):
                            exp.subs[i].params[0] = '%s_%d' % (defs[i].name, defs[i].alpha)
            for sub in exp.subs:
                rename (sub, lenv)
        elif exp.is_a ('pvcase'):
            # this is a strangely shaped binding construct
            # note: nvcase uses let internally for binding, and does not need to be here.
            # XXX I think this is probably true of pvcase too, now.
            rename (exp.value, lenv)
            n = len (exp.alts)
            for i in range (n):
                selector, defs = exp.alt_formals[i]
                alt = exp.alts[i]
                for vd in defs:
                    vd.alpha = len (vars)
                    vars.append (vd)
                lenv = (defs, lenv)
                rename (alt, lenv)
        elif exp.one_of ('varref', 'varset'):
            name = exp.params
            probe = lookup_var (name, lenv)
            if probe:
                exp.var = probe
                if exp.is_a ('varset'):
                    probe.assigns.append (exp)
                else:
                    probe.refs.append (exp)
                exp.params = exp.name = '%s_%d' % (name, exp.var.alpha)
            for sub in exp.subs:
                rename (sub, lenv)
        else:
            for sub in exp.subs:
                rename (sub, lenv)

    # first, apply any pending substs
    exp = apply_substs (exp)
    # because of the destructive update we gotta redo this
    for node in exp:
        node.fix_attribute_names()
    
    #exp.pprint()
    rename (exp, None)
    # now go back and change the names of the vardefs
    for vd in vars:
        if vd.name.startswith ('&'):
            vd.name = vd.name[1:]
        else:
            vd.name = '%s_%d' % (vd.name, vd.alpha)

    result = {}
    for vd in vars:
        result[vd.name] = vd
    return result

# leave this here for tests/t_lex.scm
42

