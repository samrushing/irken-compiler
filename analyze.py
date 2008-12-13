# -*- Mode: Python -*-

#
# analysis on the lambda tree.
#

import lambda_tree as tree
import typing

is_a = isinstance

class UnboundVariableError (Exception):
    pass

# something to think about.
# fix and let* are *very* close now.  In fact, once the node tree leaves
#  this file they become identical.  So now the question is, can we push
#  that transformation even earlier?  It might simplify some of this code,
#  and may open up some interesting transformations that could flatten
#  the lexical depth of the output...

class analyzer:

    """identify the definition and use of variables (and functions)."""

    def __init__ (self, safety=1, noinline=False, verbose=False):
        self.node_counter = 0
        self.safety=safety
        self.vars = {}
        self.constants = {}
        self.inline = not noinline
        self.verbose = verbose
        self.pending_inlines = []

    def analyze (self, root):
        # add constructors
        if len(typing.classes) or len (typing.datatypes):
            root = self.add_constructors (root)
        # transforms *before* alpha conversion
        root = self.transform (root, 0)
        # perform alpha conversion and resolve varref/varset
        self.alpha_convert (root)
        # perform simple transformations
        root = self.transform (root, 1)
        # find recursive functions/applications
        self.find_recursion (root)
        if self.verbose:
            print 'calls:'
            self.print_calls (root)
        self.find_applications (root)
        
        if self.inline:

            self.call_graph = self.build_call_graph (root)
            root = self.find_inlines (root)
            # transform again
            root = self.transform (root, 1)
            # trim again
            self.find_applications (root)
            if self.verbose:
                print 'after inlining, then pruning again'
            root = self.prune_fixes (root)
            # repeat this with new nodes...
            self.find_recursion (root)

        for node in root:
            node.fix_attribute_names()
            if node.is_a ('function'):
                node.calls = self.get_fun_calls (node)

        self.escape_analysis (root)

        if self.verbose:
            print ' --- as scheme ---'
            print tree.as_sexp (tree.to_scheme (root))

        return root

    def add_constructors (self, root):
        names = []
        inits = []
        for name, c in typing.classes.items():
            names.append (tree.vardef (c.name))
            inits.append (c.gen_constructor())
        for name, dt in typing.datatypes.items():
            if is_a (dt, typing.union):
                for sname, stype in dt.alts:
                    fname, fun = dt.gen_constructor (sname)
                    names.append (tree.vardef (fname))
                    inits.append (fun)
            else:
                raise ValueError ("unknown datatype")
        return tree.fix (names, inits, root)

    def alpha_convert (self, exp):
        vars = []

        def lookup_var (name, lenv):
            while lenv:
                rib, lenv = lenv
                # walk rib backwards for the sake of <let*>
                #   (e.g., (let ((x 1) (x 2)) ...))
                for i in range (len(rib)-1, -1, -1):
                    x = rib[i]
                    if x.name == name:
                        return x
            raise ValueError ("unbound variable: %r" % (name,))

        # XXX consider that this could be simplified by assigning
        #   a serial number to each vardef, and just using that.
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
                            exp.subs[i].params[0] = '%s_%d' % (defs[i].name, defs[i].alpha)
            elif exp.one_of ('varref', 'varset'):
                name = exp.params
                exp.var = lookup_var (name, lenv)
                if exp.is_a ('varset'):
                    if exp.var.nary:
                        raise ValueError ("can't assign to a varargs argument")
                exp.params = '%s_%d' % (name, exp.var.alpha)
            for sub in exp.subs:
                rename (sub, lenv)

        rename (exp, None)
        # now go back and change the names of the vardefs
        for vd in vars:
            vd.name = '%s_%d' % (vd.name, vd.alpha)

        # global variable table
        for vd in vars:
            self.vars[vd.name] = vd

    def transform (self, node, stage):
        name = 'transform_%d_%s' % (stage, node.kind)
        probe = getattr (self, name, None)
        if probe:
            node = probe (node)
        new_subs = [self.transform (sub, stage) for sub in node.subs]
        node = tree.node (node.kind, node.params, new_subs, node.type)
        if node.is_a ('fix'):
            # update function slots in every vardef
            names = node.get_names()
            inits  = node.subs[:-1]
            for i in range (len (names)):
                if inits[i].is_a ('function'):
                    names[i].function = inits[i]
        return node

    def transform_0_typecase (self, node):
        # (typecase type x 
        #    ((<select0> <formal0> <formal1> ...) <body0>)
        #    ((<select1> <formal0> <formal1> ...) <body1>)
        #    ...)
        # =>
        # (typecase type x
        #    ((let ((f0 x.0) (f1 x.1) (f2 x.2)) <body0>) ...))
        #
        alts = []
        for i in range (len (node.alts)):
            formals = []
            inits = []
            alt_formals = node.alt_formals[i][1:]
            for j in range (len (alt_formals)):
                formal = alt_formals[j]
                if formal != '_':
                    formals.append (tree.vardef (formal))
                    inits.append (tree.cexp ("UOBJ_GET(%%s,%d)" % (j,), ('?', ('?')), [node.value]))
            if len(formals):
                alts.append (tree.let_splat (formals, inits, node.alts[i]))
            else:
                alts.append (node.alts[i])
        assert (len(alts) == len(node.alts))
        node.alts = alts
        node.params = node.vtype, node.alts
        node.subs = [node.value] + node.alts
        return node

    def transform_1_conditional (self, node):
        # (if #t x y) => x
        [test_exp, then_exp, else_exp] = node.subs
        if test_exp.is_a ('literal') and test_exp.params[0] == 'bool':
            if test_exp.params[1] == 'true':
                return then_exp
            else:
                return else_exp
        else:
            return node

    opt_apply_lambda_to_let = True

    def transform_1_application (self, node):
        rator = node.get_rator()
        if self.opt_apply_lambda_to_let and rator.is_a ('function'):
            # ((lambda (var0 var1 ... ) <body>) <arg0>) => (let* ((var0 arg0) ...) <body>)
            # [it's ok to use let* because we alpha-converted everything]
            rands = node.get_rands()
            name, formals, recursive, type = rator.params
            body = rator.get_body()
            return self.transform (tree.let_splat (formals, rands, body), 1)
        else:
            return node

    # XXX any reason the same wouldn't work for <fix>?
    def transform_1_let_splat (self, node):
        # coalesce cascading let*
        names = node.params
        inits = node.subs[:-1]
        body = node.subs[-1]
        # this is generated often by typecase: (let (x <init>) x)
        if len(names) == 1 and body.is_a ('varref') and body.params == names[0].name:
            return inits[0]
        elif body.is_a ('let_splat'):
            names2 = body.params
            inits2 = body.subs[:-1]
            body2  = body.subs[-1]
            return tree.let_splat (
                names + names2,
                [self.transform (x, 1) for x in inits + inits2],
                self.transform (body2, 1),
                type=body2.type
                )
        else:
            # search for any let* in the inits...
            #   (let* ((x (let* ((a ...) (b ...)) <body2>))) <body1>)
            #     => (let* ((a ...) (b ...) (x <body2>)) <body1>) ???
            for i in range (len (inits)):
                if inits[i].is_a ('let_splat'):
                    n2 = inits[i]
                    # insert this let* above this variable
                    names2 = n2.params
                    inits2 = n2.subs[:-1]
                    body2  = n2.subs[-1]
                    return self.transform (
                        tree.let_splat (
                            names[:i] + names2 + names[i:],
                            inits[:i] + inits2 + [body2] + inits[i+1:],
                            body,
                            type=body.type
                            ),
                        1
                        )
            else:
                return node

    def transform_1_sequence (self, node):
        if len (node.subs) == 1:
            # (begin x) => x
            return node.subs[0]
        else:
            # (begin a0 a1 a2 ... (begin b1 b2) ...)
            # => (begin a0 a1 a2 ... b1 b2 ...)
            # this has no real effect, but feels good, doesn't it?
            subs = []
            for sub in node.subs:
                if sub.is_a ('sequence'):
                    subs.extend (sub.subs)
                else:
                    subs.append (sub)
            return tree.sequence (subs)

    # Unfortunately, these two expose the C backend up here
    #  where they shouldn't.  Should probably just make these
    #  user functions?
    def transform_1_get (self, node):
        [ob] = node.subs
        field_name = node.params
        c = typing.classes[ob.type.name]
        offset = c.get_field_offset (field_name)
        return tree.cexp ("UOBJ_GET(%%s,%d)" % (offset,), ('?', ('?')), [ob])

    def transform_1_set (self, node):
        #(%%cexp "((pxll_vector*)(%s))->val[%s] = %s" ob offset x))
        [ob, val] = node.subs
        field_name = node.params
        c = typing.classes[ob.type]
        offset = c.get_field_offset (field_name)
        return tree.cexp ("UOBJ_SET(%%s,%d,%%s)" % (offset,), ('undefined', ('?', '?')), [ob, val])

    def replace (self, orig_node, fun):
        # apply replacement-fun() to all of <node>
        node = fun (orig_node)
        new_subs = []
        size = 1
        for sub in node.subs:
            new_sub = self.replace (sub, fun)
            new_subs.append (new_sub)
            size += new_sub.size
        node.subs = new_subs
        # update the size
        node.size = size
        # catch updates to extra meta-data
        # XXX disgusting, all of it
        if node.is_a ('fix'):
            names = node.get_names()
            inits  = node.subs[:-1]
            for i in range (len (names)):
                if inits[i].is_a ('function') and names[i].function.serial != inits[i].serial:
                    old = names[i].function
                    new = inits[i]
                    names[i].function = new
        return node

    def note_funcall (self, name):
        if self.calls.has_key (name):
            self.calls[name] = self.calls[name] + 1
        else:
            self.calls[name] = 1

    def get_fun_calls (self, name):
        return self.calls.get (name, 0)

    def check_args (self, app, fun):
        # check the number of args for known functions
        nargs = len (app.subs) - 1
        formals = fun.params[1]
        if len(formals) != nargs and (len(formals) > 0 and not formals[0].nary):
            print "Wong number of args to fun %r in" % fun
            app.pprint()
            raise ValueError ("wrong number of args")

    def find_recursion (self, exp):

        self.calls = {}

        def lookup_fun (fun, fenv):
            while fenv:
                entry, fenv = fenv
                if fun is entry:
                    return True
            return False

        def search (exp, fenv):
            if exp.is_a ('function'):
                fenv = (exp, fenv)
            elif exp.is_a ('application'):
                if exp.get_rator().is_a ('varref'):
                    ref = exp.get_rator()
                    name = ref.params
                    var = self.vars[name]
                    if var.function:
                        fun = var.function
                        self.check_args (exp, fun)
                        if lookup_fun (fun, fenv):
                            # mark both the function and the application as recursive
                            fun.params[2] = True
                            exp.params = True
                        else:
                            exp.params = False
                        exp.function = fun
                        self.note_funcall (name)
                    else:
                        exp.function = None
                else:
                    exp.function = None
            for sub in exp.subs:
                search (sub, fenv)
        search (exp, None)

    def build_call_graph (self, root):
        call_graph = {}
        def search (exp, this_fun):
            if exp.is_a ('application') and exp.get_rator().is_a ('varref'):
                ref = exp.get_rator()
                name = ref.params
                this_fun.add (name)
            elif exp.is_a ('function') and exp.params[0]:
                name = exp.params[0]
                # i.e., a named function
                this_fun = set()
                call_graph[name] = this_fun
            for sub in exp.subs:
                search (sub, this_fun)
        call_graph['top'] = set()
        search (root, call_graph['top'])
        return call_graph

    def is_recursive (self, name):
        # this is used by the inliner to decide whether to inline a small
        #   function - rather than computing the full transitive closure,
        #   we'll check only candidate functions...
        class FoundIt:
            pass

        def search (name, needle, seen):
            seen.add (name)
            try:
                for callee in self.call_graph[name]:
                    if callee == needle:
                        raise FoundIt
                    if callee not in seen:
                        search (callee, needle, seen)
            except KeyError:
                # XXX for now, ignore unknown functions
                #  [i.e., pretend they can't be recursive...]
                pass

        try:
            search (name, name, set())
        except FoundIt:
            return True
        else:
            return False

    def print_calls (self, root):
        from pprint import pprint as pp
        pp (self.calls)

    def lookup_var (self, node):
        name = node.params
        return self.vars[name]

    # ad-hoc 'tree shaker'
    # we only want to descend into code that's actually called.
    # so rather than walk every function in a <fix>, we start
    # from the outermost body, and follow every chain of funcalls
    # from there.

    def get_initial_expressions (self, node):
        # collect all the expressions that will execute when this node is evaluated.
        # specifically, this is the first step of the tree shaker.
        nodes = []
        if node.one_of ('fix', 'let_splat'):
            # initial expressions consist of the body, and any non-lambda <inits>
            nodes.append (node.get_body())
            inits = node.subs[:-1]
            for init in inits:
                if not init.is_a ('function'):
                    nodes.append (init)
            return nodes
        else:
            return [node]

    def find_applications (self, root):
        # XXX this method really needs a different name - it's more like
        #   'walk applications for tree shaking..'
        to_scan = {}
        # look at the body of root - find all referenced (named) functions
        for exp in self.get_initial_expressions (root):
            for node in exp:
                if node.one_of ('varref', 'varset'):
                    var = self.lookup_var (node)
                    if var.function:
                        fun = var.function
                        fun.params[0] = var.name # alpha conversion
                        to_scan[var] = fun
                    else:
                        to_scan[var] = None
        #print 'find_applications, to_scan=', to_scan
        # find all (named) functions referenced by those in <to_scan>
        seen = to_scan.copy()
        pass_num = 1
        while len(to_scan):
            to_scan_2 = {}
            #print 'pass #%d: %r' % (pass_num, to_scan.keys())
            for name, fun in to_scan.iteritems():
                if fun:
                    for node in fun.get_body():
                        if node.one_of ('varref', 'varset'):
                            var = self.lookup_var (node)
                            if var.function:
                                fun = var.function
                                fun.params[0] = var.name # alpha conversion
                                if not seen.has_key (var):
                                    to_scan_2[var] = fun
                                    seen[var] = fun
                            else:
                                seen[var] = None
                else:
                    seen[name] = None
            pass_num += 1
            to_scan = to_scan_2
        # ok, now <seen> has every (named) called function?
        # we can now start removing uncalled funs from <fix>
        pruned = []
        for node in root:
            if node.is_a ('fix'):
                # Warning: this edits the node in place
                keep = []
                names = node.params
                funs = node.subs[:-1]
                body = node.subs[-1]
                for i in range (len (names)):
                    if seen.has_key (names[i]):
                        keep.append ((names[i], funs[i]))
                    else:
                        pruned.append (names[i])
                node.params = [x[0] for x in keep]
                node.subs   = [x[1] for x in keep] + [body]
        if self.verbose:
            print 'pruned: ', pruned
            print 'kept:   ', seen.keys()
        # trim the global variable map
        for prune in pruned:
            del self.vars[prune.name]

    def prune_fixes (self, root):
        # now prune empty fixes
        def prune_fix (node):
            if node.one_of ('fix', 'let_splat') and not node.get_names():
                return prune_fix (node.get_body())
            else:
                return node
        return self.replace (root, prune_fix)

    def is_varargs (self, fun):
        # is this an nary function?
        if fun:
            formals = fun.params[1]
            # XXX fix this when we support non-optional args
            return len(formals) and formals[0].nary
        else:
            return False

    inline_threshold = 18

    def find_inlines (self, root):

        def replacer (node):
            if node.is_a ('application'):
                rator = node.get_rator()
                if rator.is_a ('varref'):
                    var = self.lookup_var (rator)
                    name = rator.params
                    fun = var.function
                    # (<varref xxx> ...) doesn't always refer to a known
                    #  fun, in this case calls == 0...
                    calls = self.get_fun_calls (name)
                    # don't inline functions starting with magical '^' character
                    # XXX eventually this will be replaced with some
                    #     kind of compile-time-environment mechanism
                    if (not name.startswith ('^')
                        and calls > 0
                        and (not self.is_varargs (fun))
                        and ((fun.size <= self.inline_threshold or calls == 1)
                             and not self.is_recursive (name))
                        ):
                        node.function = fun
                        result = self.inline_application (node)
                        if result.is_a ('application'):
                            # sneaky!
                            return replacer (result)
                        else:
                            return result
                    else:
                        return node
                else:
                    return node
            else:
                return node

        # now call the replacer
        return self.replace (root, replacer)

    def count_refs (self, name, body):
        count = 0
        for node in body:
            if node.is_var (name):
                count += 1
        return count

    rename_counter = 0

    def inline_application (self, node):
        # ok, we've decided to inline this node.
        # now we pick which of the two kinds of inlining we'll use.
        # 1) if the arguments are all simple (lit or varref), then we inline textually.
        # 2) if any of the arguments are complex, then we translate to let*.
        # 3) if a complex argument is only referred to once, treat it like a simple arg.
        simple = []
        complex = []
        rator = node.get_rator()
        rands = node.get_rands()
        fun = node.function
        # alpha convert a copy of the function
        body = self.instantiate (fun)
        name, formals, recursive, type = fun.params
        for i in range (len (rands)):
            arg = rands[i]
            if arg.one_of ('varref', 'literal'):
                simple.append (i)
            elif self.count_refs (formals[i].name, fun) == 1:
                # XXX here's the problem.  it's a complex arg, referred to only once.
                #   hence it gets re-substituted, but without the benefit of inlining.
                #   somehow making it a sequence fixes things????
                simple.append (i)
            else:
                complex.append (i)
        if self.verbose:
            print 'inline: size=%3d name=%r simple=%r complex=%r calls=%d' % (fun.size, name, simple, complex, self.get_fun_calls (fun))
        # substitute each simple arg in the body
        if simple:
            substs = [ (formals[i], rands[i]) for i in simple ]
        else:
            substs = []
        
        if not complex:
            result = self.substitute (body, substs)
        else:
            # generate new names for the complex args
            names = []
            inits = []
            for i in complex:
                name = '%s_i%d' % (formals[i].name, analyzer.rename_counter)
                var = tree.vardef (name)
                var.type = formals[i].type
                self.vars[name] = var
                names.append (var)
                inits.append (rands[i])
                varref = tree.varref (names[-1].name)
                varref.type = names[-1].type
                substs.append ((formals[i], varref))
                analyzer.rename_counter += 1
            body = self.substitute (body, substs)
            result = tree.let_splat (names, inits, body)
            result.type = body.type
        return result

    def substitute (self, body, substs):
        def replacer (node):
            # XXX consider this - set! will work when replacing with a
            #     variable, but what if it's a constant?  can this happen?
            if node.one_of ('varref', 'varset'):
                for k, v in substs:
                    if k.name == node.params:
                        # a match
                        return v
                else:
                    return node
            else:
                return node
        return self.replace (body, replacer)

    inline_counter = 0

    def instantiate (self, fun):
        # give the body of a function, return a new copy with all fresh, unique
        #   bindings in order to preserve the alpha-converted state of the whole program
        # first, get all fresh new nodes.  This is a somewhat simpler task than full
        #   alpha conversion - mostly because we know the bindings are already unique.
        fun = fun.deep_copy()
        body = fun.get_body()
        # now, append a unique modifier to every locally bound variable
        vars = []
        suffix = '_i%d' % (analyzer.inline_counter,)
        analyzer.inline_counter += 1

        def lookup_var (name, lenv):
            while lenv:
                rib, lenv = lenv
                for x in rib:
                    if x.name == name:
                        return x
            return False

        def rename (exp, lenv):
            if exp.binds():
                defs = exp.get_names()
                vars.extend (defs)
                lenv = (defs, lenv)
            elif exp.one_of ('varref', 'varset'):
                name = exp.params
                if lookup_var (name, lenv):
                    exp.params += suffix
            for sub in exp.subs:
                rename (sub, lenv)

        rename (body, None)
        # go back and rename all the vardefs
        for vd in vars:
            vd.name += suffix

        # add the new names to the global table
        for vd in vars:
            self.vars[vd.name] = vd

        return body
        
    def escape_analysis (self, root):
        
        # for each variable, we need to know if it might potentially
        #  escape.  a variable 'escapes' when it is referenced while free
        #  inside a function that escapes (i.e., any function that is
        #  varref'd outside of the operator position).

        escapes = set()

        def find_escaping_functions (node, parent):
            if node.is_a ('function'):
                # any function outside a fix (i.e., a lambda) is by
                #  definition an escaping one (because we reduce
                #    ((lambda () ...) ...)) => (let* ...)
                if not parent or not parent.is_a ('fix'):
                    escapes.add (node)
                    node.escapes = True
            elif node.is_a ('varref'):
                if not (parent.is_a ('application') and parent.subs[0] is node):
                    # function referenced in non-rator position
                    var = self.lookup_var (node)
                    if var.function:
                        var.function.escapes = True
                        escapes.add (var.function)
            for sub in node.subs:
                find_escaping_functions (sub, node)
                
        find_escaping_functions (root, None)

        # now we've found all the escaping functions.  now grep through them
        #   for escaping variables.  we do this by building an environment
        #   only below that function, anything that fails lookup is free.

        def lookup (node, lenv):
            while lenv:
                rib, lenv = lenv
                for v in rib:
                    if v is node:
                        return v
            return False

        def find_escaping_variables (node, lenv):
            if node.binds():
                names = node.get_names()
                lenv = (names, lenv)
            elif node.one_of ('varref', 'varset'):
                if not lookup (node, lenv):
                    # reference to a free variable.  flag it as escaping.
                    var = self.lookup_var (node)
                    var.escapes = True
                    if self.verbose:
                        print '%r escapes' % (var,)
            for sub in node.subs:
                find_escaping_variables (sub, lenv)

        for fun in escapes:
            find_escaping_variables (fun, None)
            
            
