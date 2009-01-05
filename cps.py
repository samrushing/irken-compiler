# -*- Mode: Python -*-

import lambda_tree as tree

is_a = isinstance

class compiler:

    def __init__ (self, safety=1, verbose=False):
        self.line = 0
        self.safety = safety
        self.verbose = verbose
        self.constants = {}

    def lexical_address (self, lenv, name):
        x = 0
        while lenv:
            rib, lenv = lenv
            for y in range (len (rib)):
                if rib[y].name == name:
                    return rib[y], (x, y), self.use_top and lenv == None
            x += 1
        else:
            raise ValueError, "unbound variable: %r" % (name,)

    # This 'compiler' converts <exp> to CPS, with each continuation representing
    #  a target 'register' for the result of that expression.

    def compile_exp (self, tail_pos, exp, lenv, k):

        #import sys; W = sys.stdout.write
        #W ('compile_exp: [%3d] %r\n' % (exp.serial, exp,))

        if tail_pos:
            k = cont (k[1], self.gen_return)
        
        if exp.is_a ('varref'):
            return self.compile_varref (tail_pos, exp, lenv, k)
        elif exp.is_a ('varset'):
            return self.compile_varset (tail_pos, exp, lenv, k)
        elif exp.is_a ('literal'):
            return self.gen_lit (exp, k)
        elif exp.is_a ('sequence'):
            return self.compile_sequence (tail_pos, exp.subs, lenv, k)
        elif exp.is_a ('conditional'):
            return self.compile_conditional (tail_pos, exp, lenv, k)
        elif exp.is_a ('cexp'):
            return self.compile_primargs (exp.args, ('%cexp', exp.form, exp.type_sig), lenv, k)
        elif exp.is_a ('function'):
            return self.compile_function (tail_pos, exp, lenv, k)
        elif exp.is_a ('application'):
            return self.compile_application (tail_pos, exp, lenv, k)
        elif exp.one_of ('fix', 'let_splat'):
            return self.compile_let_splat (tail_pos, exp, lenv, k)
        elif exp.is_a ('make_tuple'):
            return self.compile_primargs (exp.args, ('%make-tuple', exp.type, exp.tag), lenv, k)
        elif exp.is_a ('primapp'):
            return self.compile_primapp (tail_pos, exp, lenv, k)
        elif exp.is_a ('typecase'):
            return self.compile_typecase (tail_pos, exp, lenv, k)
        else:
            raise NotImplementedError

    def safe_for_tr_call (self, app):
        if app.rator.is_a ('varref') and app.recursive and app.function:
            for vardef in app.function.formals:
                if vardef.escapes:
                    return False
            return True
        else:
            return False

    def compile_application (self, tail_pos, exp, lenv, k):
        if tail_pos:
            gen_invoke = self.gen_invoke_tail
        else:
            gen_invoke = self.gen_invoke
        if tail_pos and self.safe_for_tr_call (exp):
            # special-case tail recursion to avoid consing environments
            var, addr, is_top = self.lexical_address (lenv, exp.rator.name)
            # <tr_call> needs to know how many levels of lenv to pop
            exp.depth, index = addr
            return self.compile_tr_call (exp.rands, exp, lenv, k)
        else:
            def make_application (args_reg):
                return self.compile_exp (
                    False, exp.rator, lenv, cont (
                        [args_reg] + k[1],
                        lambda closure_reg: gen_invoke (exp.function, closure_reg, args_reg, k)
                        )
                    )
            return self.compile_rands (exp.rands, lenv, cont (k[1], make_application))

    def compile_varref (self, tail_pos, exp, lenv, k):
        var, addr, is_top = self.lexical_address (lenv, exp.name)
        assert (var.name == exp.name)
        return self.gen_varref (addr, is_top, var, k)

    def compile_varset (self, tail_pos, exp, lenv, k):
        var, addr, is_top = self.lexical_address (lenv, exp.name)
        assert (var.name == exp.name)
        return self.compile_exp (
            False, exp.value, lenv, cont (
                k[1], lambda reg: self.gen_assign (addr, is_top, var, reg, k)
                )
            )

    # collect_primargs is used by primops, simple_conditional, and tr_call.
    #   in order to avoid the needless consumption of registers, we re-arrange
    #   the eval order of these args - by placing the complex args first.

    def collect_primargs (self, args, regs, lenv, k, ck):
        # sort args by size/complexity
        args = [(args[i], i) for i in range (len (args))]
        args.sort (lambda x,y: cmp (y[0].size, x[0].size))
        perm = [x[1] for x in args]
        args = [x[0] for x in args]
        #print 'collect_primargs, len(args)=', len(args)
        return self._collect_primargs (args, regs, perm, lenv, k, ck)

    def _collect_primargs (self, args, regs, perm, lenv, k, ck):
        # collect a set of arguments into registers, pass that into compiler-continuation <ck>
        if len(args) == 0:
            # undo the permutation of the args
            perm_regs = [regs[perm.index (i)] for i in range (len (perm))]
            return ck (perm_regs)
        else:
            return self.compile_exp (
                False, args[0], lenv, cont (
                    regs + k[1],
                    lambda reg: self._collect_primargs (args[1:], regs + [reg], perm, lenv, k, ck)
                    )
                )

    def compile_tr_call (self, args, node, lenv, k):
        return self.collect_primargs (args, [], lenv, k, lambda regs: self.gen_tr_call (node, regs))

    def compile_primargs (self, args, op, lenv, k):
        return self.collect_primargs (args, [], lenv, k, lambda regs: self.gen_primop (op, regs, k))

    def compile_primapp (self, tail_pos, exp, lenv, k):
        if exp.name == '%%vector-literal':
            if len (exp.args) < 5:
                return self.compile_primargs (exp.args, ('%make-tuple', exp.type, 'vector'), lenv, k)
            else:
                return self.compile_vector_literal (exp.args, lenv, k)
        elif exp.name == '%%array-ref':
            # XXX need two different insns, to handle constant index
            # XXX could support strings as character arrays by passing down a hint?
            return self.compile_primargs (exp.args, ('%array-ref',), lenv, k)
        elif exp.name == '%%array-set':
            return self.compile_primargs (exp.args, ('%array-set',), lenv, k)
        else:
            raise ValueError ("Unknown primop: %r" % (exp.name,))

    def compile_sequence (self, tail_pos, exps, lenv, k):
        if len(exps) == 0:
            raise ValueError ("illegal sequence")
        elif len(exps) == 1:
            # last expression may be in tail position
            return self.compile_exp (tail_pos, exps[0], lenv, k)
        else:
            # more than one expression
            return self.compile_exp (
                False, exps[0], lenv,
                dead_cont (k[1], self.compile_sequence (tail_pos, exps[1:], lenv, k))
                )

    # XXX this needs to be fixed
    simple_conditionals = set (('%==', '%eq?', '%zero?', '%ge?', '%gt?', '%lt?', '%le?'))

    def compile_conditional (self, tail_pos, exp, lenv, k):
        if exp.test_exp.is_a ('primapp') and exp.test_exp.name in self.simple_conditionals:
            return self.compile_simple_conditional (tail_pos, exp, lenv, k)
        else:
            return self.compile_exp (
                False, exp.test_exp, lenv, cont (
                    k[1],
                    lambda test_reg: self.gen_test (
                        test_reg, 
                        self.compile_exp (tail_pos, exp.then_exp, lenv, cont (k[1], lambda reg: self.gen_jump (reg, k))),
                        self.compile_exp (tail_pos, exp.else_exp, lenv, cont (k[1], lambda reg: self.gen_jump (reg, k))),
                        k
                        )
                    )
                )

    def compile_simple_conditional (self, tail_pos, exp, lenv, k):
        def finish (regs):
            return self.gen_simple_test (
                exp.test_exp.name,
                regs,
                self.compile_exp (tail_pos, exp.then_exp, lenv, cont (k[1], lambda reg: self.gen_jump (reg, k))),
                self.compile_exp (tail_pos, exp.else_exp, lenv, cont (k[1], lambda reg: self.gen_jump (reg, k))),
                k
                )
        return self.collect_primargs (exp.test_exp.args, [], lenv, k, finish)

    def compile_typecase (self, tail_pos, exp, lenv, k):
        def finish (test_reg):
            jump_k = cont (k[1], lambda reg: self.gen_jump (reg, k))
            alts = [self.compile_exp (tail_pos, alt, lenv, jump_k) for alt in exp.alts]
            return self.gen_typecase (test_reg, exp.vtype, alts, k)
        return self.compile_exp (False, exp.value, lenv, cont (k[1], finish))

    def compile_function (self, tail_pos, exp, lenv, k):
        lenv = (exp.formals, lenv)
        return self.gen_closure (
            exp,
            self.compile_exp (True, exp.body, lenv, cont ([], self.gen_return)),
            k
            )

    def compile_let_splat (self, tail_pos, exp, lenv, k):
        # becomes this sequence:
        #   (new_env, push_env, store_env0, ..., <body>, pop_env)
        k_body = dead_cont (k[1], self.compile_exp (tail_pos, exp.body, (exp.names, lenv), cont (k[1], lambda reg: self.gen_pop_env (reg, k))))
        return self.gen_new_env (
            len (exp.names),
            cont (
                k[1],
                lambda tuple_reg: self.gen_push_env (
                    tuple_reg,
                    dead_cont (k[1], self.compile_tuple_rands (0, exp.inits, tuple_reg, [tuple_reg] + k[1], (exp.names, lenv), k_body))
                    )
                )
            )

    opt_collect_args_in_regs = False

    if opt_collect_args_in_regs:
        # simply collect the args into registers, then use a <build_env> insn to populate the rib.
        # Note that collect_primargs will re-order the args...
        def compile_rands (self, rands, lenv, k):
            return self.collect_primargs (rands, [], lenv, k, lambda regs: self.gen_build_env (regs, k))
    else:
        # allocate the env rib, then place each arg in turn.
        # NOTE:
        #   to change the order of evaluation to right-to-left, you need to:
        #   1) pass i+1 to compile_tuple_rands
        #   2) make "i>0" the test, and
        #   3) i-1 the iter
        # then beware of callers expecting the other behavior (like let*)
        def compile_rands (self, rands, lenv, k):
            if not rands:
                return self.gen_new_env (0, k)
            else:
                return self.gen_new_env (
                    len (rands),
                    cont (k[1], lambda tuple_reg: self.compile_tuple_rands (0, rands, tuple_reg, [tuple_reg] + k[1], lenv, k))
                    )

        def compile_tuple_rands (self, i, rands, tuple_reg, free_regs, lenv, k):
            return self.compile_exp (
                False, rands[i], lenv, cont (
                    free_regs,
                    lambda arg_reg: self.gen_store_env (
                        arg_reg, tuple_reg, i, len(rands),
                        (dead_cont (free_regs, self.compile_tuple_rands (i+1, rands, tuple_reg, free_regs, lenv, k)) if i+1 < len(rands) else k)
                        )
                    )
                )

    # ugh, this nearly completely duplicates <compile_rands>/<compile_tuple_rands>
    def compile_vector_literal (self, rands, lenv, k):
        return self.gen_new_vector (
            len (rands),
            cont (k[1], lambda vec_reg: self.compile_vector_rands (0, rands, vec_reg, [vec_reg] + k[1], lenv, k))
            )

    def compile_vector_rands (self, i, rands, vec_reg, free_regs, lenv, k):
        return self.compile_exp (
            False, rands[i], lenv, cont (
                free_regs,
                lambda arg_reg: self.gen_store_vec (
                    arg_reg, vec_reg, i, len(rands),
                    (dead_cont (free_regs, self.compile_vector_rands (i+1, rands, vec_reg, free_regs, lenv, k)) if i+1 < len(rands) else k)
                    )
                )
            )


# the allocator and cont stuff *should* be in the compiler instance...

class register_allocator:

    def __init__ (self):
        self.max_reg = -1

    def allocate (self, free_regs):
        i = 0
        while 1:
            if i not in free_regs:
                self.max_reg = max (self.max_reg, i)
                return i
            else:
                i += 1
        
the_register_allocator = register_allocator()

def cont (free_regs, generator):
    reg = the_register_allocator.allocate (free_regs)
    return (reg, free_regs, generator (reg))

def dead_cont (free_regs, k):
    return ('dead', free_regs, k)

def box (n):
    return (n<<1)|1

class INSN:

    typecheck = None
    allocates = 0

    def __init__ (self, name, regs, params, k):
        self.name = name
        self.regs = regs
        self.params = params
        self.k = k
        self.subs = ()

    def print_info (self):
        if self.name == 'test':
            return '%s %r %r' % (self.name, self.regs, self.params[0])
        elif self.name == 'close':
            return '%s %r %r' % (self.name, self.regs, self.params[0].name)
        elif self.name == 'typecase':
            return '%s %r %r' % (self.name, self.params[0], self.regs)
        else:
            return '%s %r %r' % (self.name, self.regs, self.params)

    def __repr__ (self):
        return '<INSN %s>' % (self.print_info())

class irken_compiler (compiler):

    """generates 'register' CPS"""


    def gen_lit (self, lit, k):
        if lit.type == 'int':
            return INSN ('lit', [], box (lit.value), k)
        elif lit.type == 'bool':
            if lit.value == 'true':
                n = 0x106
            else:
                n = 0x6
            return INSN ('lit', [], n, k)
        elif lit.type == 'char':
            return INSN ('lit', [], (ord(lit.value)<<8)|0x02, k)
        elif lit.type == 'string':
            return INSN ('make_string', [], lit.value, k)
        elif lit.type == 'undefined':
            return INSN ('lit', [], 0x0e, k)
        elif lit.type == 'nil':
            return INSN ('lit', [], 0x0a, k)
        else:
            raise SyntaxError

    def gen_primop (self, primop, regs, k):
        return INSN ('primop', regs, primop, k)

    def gen_jump (self, reg, k):
        # k[0] is the target for the whole conditional
        return INSN ('jump', [reg, k[0]], None, None)

    def gen_new_env (self, size, k):
        return INSN ('new_env', [], size, k)

    def gen_store_env (self, arg_reg, tuple_reg, i, n, k):
        return INSN ('store_env', [arg_reg, tuple_reg], (i, n), k)

    def gen_build_env (self, regs, k):
        return INSN ('build_env', regs, None, k)

    def gen_push_env (self, reg, k):
        return INSN ('push_env', [reg], None, k)

    def gen_pop_env (self, reg, k):
        return INSN ('pop_env', [reg], None, k)

    def gen_new_vector (self, size, k):
        return INSN ('new_vector', [], size, k)

    def gen_store_vec (self, arg_reg, vec_reg, i, n, k):
        return INSN ('store_vec', [arg_reg, vec_reg], (i, n), k)

    def gen_varref (self, addr, is_top, var, k):
        return INSN ('varref', [], (addr, is_top, var), k)
    
    def gen_assign (self, addr, is_top, var, reg, k):
        return INSN ('varset', [reg], (addr, is_top, var), k)

    def gen_move (self, reg_var, reg_src, name, k):
        return INSN ('move', [reg_var, reg_src], name, k)

    def gen_save (self, free_regs, k):
        return INSN ('save', [free_regs], None, k)

    def gen_closure (self, fun, body, k):
        return INSN ('close', [], (fun, body, k[1]), k)

    def gen_test (self, test_reg, then_code, else_code, k):
        return INSN ('test', [test_reg], (None, then_code, else_code), k)

    def gen_simple_test (self, name, regs, then_code, else_code, k):
        return INSN ('test', regs, (name, then_code, else_code), k)

    def gen_typecase (self, test_reg, type, alts, k):
        return INSN ('typecase', [test_reg], (type, alts), k)

    def gen_invoke_tail (self, fun, closure_reg, args_reg, k):
        return INSN ('invoke_tail', [closure_reg, args_reg], fun, None)

    def gen_invoke (self, fun, closure_reg, args_reg, k):
        return INSN ('invoke', [closure_reg, args_reg], (k[1], fun), k)

    def gen_tr_call (self, app_node, regs):
        return INSN ('tr_call', regs, (app_node.depth, app_node.function), None)

    def gen_return (self, val_reg):
        return INSN ('return', [val_reg], None, None)

    def go (self, exp):
        lenv = None
        # only enable the 'top lenv' hack if the top level is a fix
        self.use_top = exp.is_a ('fix')
        result = self.compile_exp (True, exp, lenv, cont ([], self.gen_return))
        result = flatten (result)
        find_allocation (result, self.verbose)
        return result

def flatten (exp):
    r = []
    while exp:
        #print exp
        if exp.k:
            target, free_regs, next = exp.k
        else:
            next = None
            target = None
            free_regs = []
        exp.k = None
        exp.target = target
        exp.free_regs = free_regs
        if exp.name == 'test':
            name, then_code, else_code = exp.params
            exp.params = name, flatten (then_code), flatten (else_code)
        elif exp.name == 'close':
            node, body, free = exp.params
            exp.params = node, flatten (body), free
        elif exp.name == 'typecase':
            type, alts = exp.params
            exp.params = type, [flatten (x) for x in alts]
        r.append (exp)
        exp = next
    return r

import sys
W = sys.stdout.write

def pretty_print (insns, depth=0): 
   for insn in insns:
        W ('%s' % ('    ' * depth))
        if insn.target == 'dead':
            W ('   -   ')
        elif insn.target is None:
            W ('       ')
        else:
            W ('%4d = ' % (insn.target,))
        W ('%s\n' % (insn.print_info(),))
        # special case prints
        if insn.name == 'test':
            name, then_code, else_code = insn.params
            pretty_print (then_code, depth+1)
            pretty_print (else_code, depth+1)
        elif insn.name == 'close':
            node, body, free = insn.params
            pretty_print (body, depth+1)
        elif insn.name == 'typecase':
            type, alts = insn.params
            for alt in alts:
                pretty_print (alt, depth+1)

def walk (insns):
    "iterate the entire tree of insns"
    # XXX what about <fix> and <let*>?
    for insn in insns:
        yield (insn)
        if insn.name == 'test':
            name, then_code, else_code = insn.params
            for x in walk (then_code):
                yield x
            for x in walk (else_code):
                yield x
        elif insn.name == 'close':
            node, body, free = insn.params
            for x in walk (body):
                yield x

def walk_function (insns):
    "iterate only the insns in this function body"
    for insn in insns:
        yield (insn)
        if insn.name == 'test':
            name, then_code, else_code = insn.params
            for x in walk_function (then_code):
                yield x
            for x in walk_function (else_code):
                yield x
        elif insn.name == 'typecase':
            type, alts = insn.params
            for alt in alts:
                for x in walk_function (alt):
                    yield x

def find_allocation (insns, verbose):
    funs = [ x for x in walk (insns) if x.name == 'close' ]
    # examine each fun to see if it performs allocation
    for fun in funs:
        node, body, free = fun.params
        fun.allocates = 0
        for insn in walk_function (body):
            if insn.name == 'primop' and insn.params[0] == '%make-tuple' and len(insn.regs):
                # we're looking for non-immediate constructors (i.e., list/cons but not list/nil)
                fun.allocates += 1
            elif insn.name in ('new_env', 'build_env', 'new_vector'):
                fun.allocates += 1
        if verbose:
            print 'allocates %d %s' % (fun.allocates, fun.params[0].name)
