# -*- Mode: Python -*-

#
# analysis on the lambda tree - inlining, simplification, etc...
#

import nodes
import itypes

from pdb import set_trace as trace

is_a = isinstance

class UnboundVariableError (Exception):
    pass

# XXX this file needs a lot of work now.  much of the nastier stuff in here
#  has now been obsoleted by the typing phase.

# something to think about.
# fix and let* are *very* close now.  In fact, once the node tree leaves
#  this file they become identical.  So now the question is, can we push
#  that transformation even earlier?  It might simplify some of this code,
#  and may open up some interesting transformations that could flatten
#  the lexical depth of the output...

class analyzer:

    """identify the definition and use of variables (and functions)."""

    def __init__ (self, context, noinline=False, verbose=False):
        self.node_counter = 0
        self.context = context
        self.vars = context.var_dict
        self.constants = {}
        self.inline = not noinline
        self.verbose = verbose
        self.inline_multiplier = {}

    def analyze (self, root):
        # find aliases
        self.find_aliases (root)
        # perform simple transformations
        root = self.transform (root, 0)
        root = self.transform (root, 1)
        self.find_recursion (root)
        if self.verbose:
            print 'calls:'
            self.print_calls (root)
        self.find_applications (root)
        
        if self.inline:
            # XXX this is already being done in typing, let's combine them.
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

        # mark leaf expressions
        self.find_leaves (root)

        for node in root:
            node.fix_attribute_names()
            if node.is_a ('function'):
                node.calls = self.get_fun_calls (node)

        self.escape_analysis (root)

        return root

    def transform (self, node, stage):
        name = 'transform_%d_%s' % (stage, node.kind)
        probe = getattr (self, name, None)
        if probe:
            node = probe (node)
        new_subs = [self.transform (sub, stage) for sub in node.subs]
        node = nodes.node (node.kind, node.params, new_subs, node.type)
        if node.is_a ('fix'):
            # update function slots in every vardef
            names = node.get_names()
            inits  = node.subs[:-1]
            for i in range (len (names)):
                if inits[i].is_a ('function'):
                    names[i].function = inits[i]
        return node

    def transform_0_primapp (self, node):
        if node.name.startswith ('%vcase/'):
            return self.transform_pvcase (node)
        else:
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

    # XXX any reason the same wouldn't work for <fix>?
    def transform_1_let_splat (self, node):
        # coalesce cascading let*
        names = node.params
        inits = node.subs[:-1]
        body = node.subs[-1]
        # this is generated often by vcase: (let (x <init>) x)
        # XXX probably not true any more XXX
        if len(names) == 1 and body.is_a ('varref') and body.params == names[0].name:
            return inits[0]
        elif body.is_a ('let_splat'):
            names2 = body.params
            inits2 = body.subs[:-1]
            body2  = body.subs[-1]
            return nodes.let_splat (
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
                        nodes.let_splat (
                            names[:i] + names2 + names[i:],
                            inits[:i] + inits2 + [body2] + inits[i+1:],
                            body,
                            type=body.type
                            ),
                        1
                        )
            else:
                return node

    def find_aliases (self, root):
        # find aliases - e.g., "(define THING 0)" or "(define PLUS +)"
        # this is a bit of a hack - this should probably be folded into
        #   a more general mechanism (like the inliner, duh).
        for node in root:
            if node.is_a ('fix'):
                for i in range (len (node.names)):
                    name = node.names[i]
                    init = node.inits[i]
                    if init.is_a ('literal'):
                        # strings are a special case - they're not 'simple' literals.
                        if not name.assigns and init.ltype != 'string':
                            name.alias = init
                    elif init.is_a ('varref'):
                        # neither is assigned to, should be safe
                        if not name.assigns and not init.var.assigns:
                            name.alias = init

    def transform_0_varref (self, node):
        var = self.vars[node.name]
        if var.alias is not None:
            #print 'alias %r => %r' % (var, var.alias)
            return var.alias
        else:
            return node

    def transform_1_fix (self, node):
        # coalesce cascading <fix>
        # (fix (a b c) (fix (d e f) ...))
        # => (fix (a b c d e f) ...)
        names = node.params
        inits = node.subs[:-1]
        body = node.subs[-1]
        if body.is_a ('fix'):
            names2 = body.params
            inits2 = body.subs[:-1]
            body2  = body.subs[-1]
            result = nodes.fix (
                names + names2,
                [self.transform (x, 1) for x in inits + inits2],
                self.transform (body2, 1),
                type=body2.type
                )
            result.fix_attribute_names()
            return result
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
            return nodes.sequence (subs)

    def transform_pvcase (self, node, val=None):
        ignore, label, arity = node.name.split ('/')
        arity = int (arity)
        success, failure, value = node.subs
        if val is None:
            val = value
        if failure.body.is_a ('primapp') and failure.body.name.startswith ('%vcase/'):
            vcase = self.transform_pvcase (failure.body, val)
        elif failure.body.is_a ('primapp') and failure.body.name.startswith ('%vfail'):
            vcase = nodes.pvcase (val, [], [])
        else:
            # since <failure> cannot bind any variables, we just beta reduce it here.
            vcase = nodes.pvcase (val, [], [failure.body])
        # filter out don't-care variable bindings
        n = len (success.formals)
        formals = []
        kept = []
        for i in range (n):
            f = success.formals[i]
            if not f.name.startswith ('_'):
                formals.append (f)
                kept.append (i)
        # ugh, always a bad idea to edit nodes in place.
        success.formals = formals
        success.params[1] = formals
        alt_formals = (label, n, success.formals)
        # don't trigger this for variant records!
        inits = []
        if arity > 1:
            for i in kept:
                inits.append (nodes.primapp ('%%vget/%s/%d/%d' % (label, arity, i), [val]))
        elif arity == 0:
            inits = []
        else:
            if 0 in kept:
                inits.append (nodes.primapp ('%%vget/%s/%d/0' % (label, arity,), [val]))
            else:
                inits = []
        for x in inits:
            x.fix_attribute_names()
        clause = nodes.application (success, inits)
        vcase.params.insert (0, alt_formals)
        vcase.subs.insert (1, clause)
        return vcase

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
        mult = self.inline_multiplier.get (name, 1)
        return mult * self.calls.get (name, 0)

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

    # XXX shouldn't be needed, use <context.dep_graph> instead.
    # YYY not necessarily - dep_graph records dependencies on things other than funcalls.
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

    # XXX use context.dep_graph, or context.scc_graph, which have all the cycles for us already.
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
                    # XXX we should really check that this variable is actually *used*
                    nodes.append (init)
            return nodes
        else:
            return [node]

    def find_applications (self, root):
        # XXX this method really needs a different name - it's more like
        #   'walk applications for tree shaking..'
        to_scan = {}
        # look at the body of root - find all referenced (named) functions
        initial_expressions = self.get_initial_expressions (root)
        for exp in initial_expressions:
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

    inline_threshold = 13

    def find_inlines (self, root):

        def replacer (node):
            if node.is_a ('application'):
                rator = node.get_rator()
                if rator.is_a ('varref'):
                    name = rator.params
                    var = self.lookup_var (rator)
                    fun = var.function
                    # (<varref xxx> ...) doesn't always refer to a known
                    #  fun, in this case calls == 0...
                    calls = self.get_fun_calls (name)
                    # don't inline functions starting with magical '^' character
                    # XXX eventually this will be replaced with some
                    #     kind of compile-time-environment mechanism
                    if (not name.startswith ('^')
                        and calls > 0
                        and ((fun.size <= self.inline_threshold or calls == 1)
                             and not self.is_recursive (name))
                        ):
                        if calls > 1:
                            # set the inline multiplier for funs called by this one.
                            self.set_multiplier (name, calls)
                        node.function = fun
                        result = self.inline_application (node)
                        if result.is_a ('application'):
                            # sneaky!
                            return replacer (result)
                        else:
                            return result
                    else:
                        return node
                elif rator.is_a ('function'):
                    node.function = rator
                    result = self.inline_application (node)
                    if result.is_a ('application'):
                        #print 'inlining lambda...'
                        #node.pprint()
                        return replacer (result)
                    else:
                        return result
                else:
                    return node
            else:
                return node

        # now call the replacer
        return self.replace (root, replacer)

    def set_multiplier (self, name, calls):
        # when we inline <name>, each function that it calls must have its call-count
        #  raised by a factor of <calls>.
        for callee in self.call_graph[name]:
            # only record the multiplier the first time <name> is inlined.
            if not self.inline_multiplier.has_key (callee):
                self.inline_multiplier[callee] = calls

    def assigned (self, var):
        return len (self.context.var_dict[var.name].assigns)

    def safe_nvget_inline (self, rands):
        r0 = rands[0]
        if r0.is_a ('primapp') and r0.name.startswith ('%nvget/'):
            # make sure the variable is not assigned to...
            if self.assigned (r0.args[0]) == 0:
                return True
        return False

    rename_counter = 0

    def inline_application (self, node):
        # ok, we've decided to inline this node.
        # now we pick which of the two kinds of inlining we'll use.
        # 1) if the arguments are all simple (lit or varref), then we inline textually.
        # 2) if any of the arguments are complex, then we translate to let*.
        # 3) if a complex argument is only referred to once, treat it like a simple arg.
        #
        # XXX might we consider a primapp a simple arg? (say, depending on its size?)
        #
        simple = []
        complex = []
        rator = node.get_rator()
        rands = node.get_rands()
        fun = node.function
        # alpha convert a copy of the function
        body = self.instantiate (fun)
        name, formals, recursive, type = fun.params
        assert (len(formals) == len (rands))
        for i in range (len (rands)):
            arg = rands[i]
            formal = formals[i]
            if arg.is_a ('varref'):
                if self.assigned (arg) or self.assigned (formal):
                    complex.append (i)
                else:
                    simple.append (i)
            elif arg.is_a ('literal'):
                simple.append (i)
            # ok, this just fails with the t_stack.scm, because the field selection primapp
            #   hides the reference to an assigned variable.  think about how important this
            #   is and try to get it back?
            #elif len(formal.refs) == 1:
            #    # it's a complex arg, referred to only once.
            #    simple.append (i)
            # XXX because the case of field selection is so important (otherwise *every* vcase
            #     expression will allocate), I'm going to special case it here.
            elif len(formal.refs) == 1 and self.safe_nvget_inline (rands):
                simple.append (i)
            else:
                complex.append (i)
        if self.verbose:
            print 'inline: size=%3d name=%r simple=%r complex=%r calls=%d' % (fun.size, name, simple, complex, self.get_fun_calls (name))
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
                # propagate types as well
                name = '%s_i%d' % (formals[i].name, analyzer.rename_counter)
                var = nodes.vardef (name)
                var.type = formals[i].type
                self.vars[name] = var
                names.append (var)
                inits.append (rands[i])
                varref = nodes.varref (names[-1].name)
                varref.type = names[-1].type
                varref.var = var
                substs.append ((formals[i], varref))
                analyzer.rename_counter += 1
            body = self.substitute (body, substs)
            result = nodes.let_splat (names, inits, body)
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
                        if node.is_a ('varset'):
                            return nodes.varset (v.name, node.value)
                        else:
                            return v
                else:
                    return node
            else:
                return node
        return self.replace (body, replacer)

    inline_counter = 0

    def instantiate (self, fun):
        # give the body of a function, return a new copy with all fresh, unique
        #   bindings in order to preserve the alpha-converted state of the whole program.
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

        def lookup (name, lenv):
            while lenv:
                rib, lenv = lenv
                for v in rib:
                    if v == name:
                        return v
            return False

        def find_escaping_variables (node, lenv):
            if node.binds():
                names = [x.name for x in node.get_names()]
                lenv = (names, lenv)
            elif node.one_of ('varref', 'varset'):
                name = node.params
                if not lookup (name, lenv):
                    # reference to a free variable.  flag it as escaping.
                    var = self.lookup_var (node)
                    var.escapes = True
                    if self.verbose:
                        print '%r escapes' % (var,)
            for sub in node.subs:
                find_escaping_variables (sub, lenv)

        for fun in escapes:
            find_escaping_variables (fun, None)
            
            
    def find_leaves (self, exp):

        # descend the node tree, marking nodes as 'leaf' (or not) on the way up.

        def search (exp):
            if exp.is_a ('application'):
                is_leaf = False
            else:
                is_leaf = True
            for sub in exp.subs:
                sub.leaf = search (sub)
                is_leaf = is_leaf and sub.leaf
            return is_leaf

        exp.leaf = search (exp)
