# -*- Mode: Python -*-

from pdb import set_trace as trace

import solver

is_a = isinstance

class register_rib:
    def __init__ (self, formals, regs):
        self.formals = formals
        self.regs = regs
        assert (len (formals) == len (regs))
    def lookup (self, name):
        lf = len (self.formals)
        for i in range (lf):
            if name == self.formals[i].name:
                return self.formals[i], self.regs[i]
        return None

class compiler:

    def __init__ (self, context, safety=1, verbose=False):
        self.context = context
        self.safety = safety
        self.verbose = verbose
        self.constants = {}

    def lexical_address (self, lenv, name):
        x = 0
        while lenv:
            rib, lenv = lenv
            if isinstance (rib, register_rib):
                probe = rib.lookup (name)
                if probe is not None:
                    var, reg = probe
                    return var, (None, reg), False
            else:
                for y in range (len (rib)):
                    if rib[y].name == name:
                        return rib[y], (x, y), self.use_top and lenv == None
                # only real 'ribs' increase lexical depth
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
        elif exp.is_a ('fix'):
            return self.compile_let_splat (tail_pos, exp, lenv, k)
        elif exp.is_a ('let_splat'):
            if self.safe_for_let_reg (exp):
                return self.compile_let_reg (tail_pos, exp, lenv, k)
            else:
                return self.compile_let_splat (tail_pos, exp, lenv, k)
        elif exp.is_a ('make_tuple'):
            return self.compile_primargs (exp.args, ('%make-tuple', exp.type, exp.tag), lenv, k)
        elif exp.is_a ('primapp'):
            return self.compile_primapp (tail_pos, exp, lenv, k)
        elif exp.is_a ('pvcase'):
            return self.compile_pvcase (tail_pos, exp, lenv, k)            
        elif exp.is_a ('nvcase'):
            return self.compile_nvcase (tail_pos, exp, lenv, k)            
        else:
            raise NotImplementedError

    def safe_for_let_reg (self, exp):
        # we only want to use registers for bindings when
        #  1) we're in a leaf position (to avoid consuming registers
        #     too high on the stack, and to avoid escaping variables),
        #  2) there's not too many bindings (again, avoid consuming regs)
        #  3) none of the variables escape (storing a binding in a reg
        #     defeats the idea of a closure)
        if exp.leaf and len(exp.names) < 4:
            for name in exp.names:
                if name.escapes:
                    return False
            else:
                return True
        else:
            return False

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

    def compile_varref (self, tail_pos, exp, lenv, k):
        var, addr, is_top = self.lexical_address (lenv, exp.name)
        if addr[0] is None:
            # register variable
            return self.gen_move (addr[1], None, k)
        else:
            return self.gen_varref (addr, is_top, var, k)

    def compile_varset (self, tail_pos, exp, lenv, k):
        var, addr, is_top = self.lexical_address (lenv, exp.name)
        assert (var.name == exp.name)
        if addr[0] is None:
            # register variable
            fun = lambda reg: self.gen_move (addr[1], reg, k)
        else:
            fun = lambda reg: self.gen_assign (addr, is_top, var, reg, k)
        return self.compile_exp (False, exp.value, lenv, cont (k[1], fun))

    # collect_primargs is used by primops, simple_conditional, and tr_call.
    #   in order to avoid the needless consumption of registers, we re-arrange
    #   the eval order of these args - by placing the complex args first.

    def collect_primargs (self, args, regs, lenv, k, ck, reorder=True):
        args = [(args[i], i) for i in range (len (args))]
        if reorder:
            # sort args by size/complexity
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
        if exp.name.startswith ('%raccess/') or exp.name.startswith ('%rset/'):
            prim, field = exp.name.split ('/')
            # try to get constant-time field access...
            sig = solver.get_record_sig (exp.args[0].type)
            if prim == '%raccess':
                return self.compile_primargs (exp.args, ('%record-get', field, sig), lenv, k)
            else:
                return self.compile_primargs (exp.args, ('%record-set', field, sig), lenv, k)                
        elif exp.name.startswith ('%rextend/'):
            return self.compile_record_literal (exp, lenv, k)
        elif exp.name.startswith ('%vector-literal/'):
            if len (exp.args) < 5:
                return self.compile_primargs (exp.args, ('%make-tuple', exp.type, 'vector'), lenv, k)
            else:
                return self.compile_vector_literal (exp.args, lenv, k)
        elif exp.name.startswith ('%make-vector'):
            return self.compile_primargs (exp.args, ('%make-vector',), lenv, k)
        elif exp.name in ('%%array-ref', '%%product-ref'):
            # XXX need two different insns, to handle constant index
            # XXX could support strings as character arrays by passing down a hint?
            return self.compile_primargs (exp.args, ('%array-ref',), lenv, k)
        elif exp.name == '%%array-set':
            return self.compile_primargs (exp.args, ('%array-set',), lenv, k)
        elif exp.name.startswith ('%vcon/'):
            ignore, label, arity = exp.name.split ('/')
            tag = self.context.variant_labels[label]
            return self.compile_primargs (exp.args, ('%make-tuple', label, tag), lenv, k)
        elif exp.name.startswith ('%vget/'):
            ignore, label, arity, index = exp.name.split ('/')
            return self.compile_primargs (exp.args, ('%vget', index), lenv, k)
        elif exp.name.startswith ('%nvget/'):
            ignore, dtype, label, index = exp.name.split ('/')
            return self.compile_primargs (exp.args, ('%vget', index), lenv, k)
        elif exp.name.startswith ('%dtcon/'):
            ignore, dtname, label = exp.name.split ('/')
            tag = self.context.datatypes[dtname].tags[label]
            return self.compile_primargs (exp.args, ('%make-tuple', label, tag), lenv, k)
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

    def compile_conditional (self, tail_pos, exp, lenv, k):
        if exp.test_exp.is_a ('cexp'):
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
                exp.test_exp.params,
                regs,
                self.compile_exp (tail_pos, exp.then_exp, lenv, cont (k[1], lambda reg: self.gen_jump (reg, k))),
                self.compile_exp (tail_pos, exp.else_exp, lenv, cont (k[1], lambda reg: self.gen_jump (reg, k))),
                k
                )
        return self.collect_primargs (exp.test_exp.args, [], lenv, k, finish)

    def compile_pvcase (self, tail_pos, exp, lenv, k):
        def finish (test_reg):
            jump_k = cont (k[1], lambda reg: self.gen_jump (reg, k))
            alts = [self.compile_exp (tail_pos, alt, lenv, jump_k) for alt in exp.alts]
            return self.gen_pvcase (test_reg, exp.alt_formals, alts, k)
        return self.compile_exp (False, exp.value, lenv, cont (k[1], finish))

    def compile_nvcase (self, tail_pos, exp, lenv, k):
        def finish (test_reg):
            jump_k = cont (k[1], lambda reg: self.gen_jump (reg, k))
            alts = [self.compile_exp (tail_pos, alt, lenv, jump_k) for alt in exp.alts]
            return self.gen_nvcase (test_reg, exp.vtype, alts, k)
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
                    dead_cont (k[1], self.compile_store_rands (0, 1, exp.inits, tuple_reg, [tuple_reg] + k[1], (exp.names, lenv), k_body))
                    )
                )
            )

    def compile_let_reg (self, tail_pos, exp, lenv, k):

        def loop (names, inits, lenv, regs):
            if len(inits) == 0:
                return self.compile_exp (tail_pos, exp.body, lenv, (k[0], k[1] + regs, k[2]))
            else:
                lenv0 = (register_rib ([names[0]], [inits[0]]), lenv)
                return self.compile_exp (
                    False, inits[0], lenv, cont (
                        regs + k[1],
                        lambda reg: loop (
                            names[1:],
                            inits[1:],
                            (register_rib ([names[0]], [reg]), lenv),
                            regs + [reg]
                            )
                        )
                    )

        return loop (exp.names, exp.inits, lenv, [])

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
                    cont (k[1], lambda tuple_reg: self.compile_store_rands (0, 1, rands, tuple_reg, [tuple_reg] + k[1], lenv, k))
                    )

    # if we use collect_primargs() to populate literal vectors and records, the code
    #   emitted consumes one register for each arg before finally storing all the registers
    #   in one pass.  As the literals become larger, the register usage becomes very wasteful.
    # instead, this function accumulates the args one at a time, and stores them individually
    #   into the tuple.
        
    def compile_store_rands (self, i, offset, rands, tuple_reg, free_regs, lenv, k):
        # offset is an additional offset from the beginning of the tuple - used only
        #  when storing into environment ribs (because of the <next> pointer immediately
        #  after the tag).
        return self.compile_exp (
            False, rands[i], lenv, cont (
                free_regs,
                lambda arg_reg: self.gen_store_tuple (
                    offset, arg_reg, tuple_reg, i, len(rands),
                    (dead_cont (free_regs, self.compile_store_rands (i+1, offset, rands, tuple_reg, free_regs, lenv, k)) if i+1 < len(rands) else k)
                    )
                )
            )

    def compile_vector_literal (self, rands, lenv, k):
        # XXX fixme, this constant doesn't belong here
        PXLL_VECTOR = 0x14
        return self.gen_new_tuple (
            PXLL_VECTOR, len (rands),
            cont (k[1], lambda vec_reg: self.compile_store_rands (0, 0, rands, vec_reg, [vec_reg] + k[1], lenv, k))
            )

    def get_record_tag (self, sig):
        #print 'get record tag', sig
        c = self.context
        if not c.records2.has_key (sig):
            c.records2[sig] = len (c.records2)
            for label in sig:
                if not c.labels2.has_key (label):
                    c.labels2[label] = len (c.labels2)
        return c.records2[sig]

    def compile_record_literal (self, exp, lenv, k):
        # unwind row primops into a record literal
        # (%rextend/field0 (%rextend/field1 (%rmake) ...)) => {field0=x field1=y}
        fields = []
        while 1:
            if exp.name == '%rmake':
                # we're done...
                break
            elif exp.name.startswith ('%rextend/'):
                ignore, field = exp.name.split ('/')
                fields.append ((field, exp.args[1]))
                exp = exp.args[0]
            else:
                return self.compile_record_extension (fields, exp, lenv, k)
        # put the names into canonical order (sorted by label)
        fields.sort (lambda a,b: cmp (a[0],b[0]))
        # lookup the runtime tag for this record
        sig = tuple ([x[0] for x in fields])
        TC_USEROBJ = 0x20
        tag = TC_USEROBJ + (self.get_record_tag (sig) << 2)
        # now compile the expression as a %make-tuple
        args = [x[1] for x in fields]
        return self.gen_new_tuple (
            tag, len (args),
            cont (k[1], lambda rec_reg: self.compile_store_rands (0, 0, args, rec_reg, [rec_reg] + k[1], lenv, k))
            )

    def compile_record_extension (self, fields, exp, lenv, k):
        # ok, we have a source record {a,b} to which we want to add
        #   one or more fields {c,d}.  We'll need to compile a
        #   'make-tuple' with args fetched from the source record
        #   mixed in with new args, all in the correct order.
        sig = solver.get_record_sig (exp.type)
        labels = [x[0] for x in fields]
        labels.sort()
        args = [x[1] for x in fields]
        new_sig = list(set(sig).union (set(labels)))
        new_sig.sort()
        new_sig = tuple (new_sig)
        if sig == new_sig:
            # identical, it's actually an update
            # XXX should consider doing copy+update instead, for functional cred.
            # XXX another option: consider it an error.
            # the last sounds best: principle of least surprise.
            assert (len(fields) == 1)
            return self.compile_primargs ([exp, args[0]], ('%record-set', fields[0][0], sig), lenv, k)
        else:
            new_tag = self.get_record_tag (new_sig)
            return self.compile_primargs ([exp] + args, ('%extend-tuple', labels, sig, new_tag), lenv, k)

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
        elif self.name in ('pvcase', 'nvcase'):
            return '%s %r %r' % (self.name, self.params[0], self.regs)
        else:
            return '%s %r %r' % (self.name, self.regs, self.params)

    def __repr__ (self):
        return '<INSN %s>' % (self.print_info())

class cps (compiler):

    """generates 'register' CPS"""


    def gen_lit (self, lit, k):
        if lit.ltype == 'int':
            return INSN ('lit', [], box (lit.value), k)
        elif lit.ltype == 'bool':
            if lit.value == 'true':
                n = 0x106
            else:
                n = 0x6
            return INSN ('lit', [], n, k)
        elif lit.ltype == 'char':
            if lit.value == 'eof':
                # special case
                val = 257<<8|0x02
            else:
                val = ord(lit.value)<<8|0x02
            return INSN ('lit', [], val, k)
        elif lit.ltype == 'string':
            return INSN ('make_string', [], lit.value, k)
        elif lit.ltype == 'undefined':
            return INSN ('lit', [], 0x0e, k)
        elif lit.ltype == 'nil':
            return INSN ('lit', [], 0x0a, k)
        else:
            raise SyntaxError

    def gen_primop (self, primop, regs, k):
        return INSN ('primop', regs, primop, k)

    def gen_move (self, reg_var, reg_src, k):
        return INSN ('move', [reg_var, reg_src], None, k)

    def gen_jump (self, reg, k):
        # k[0] is the target for the whole conditional
        return INSN ('jump', [reg, k[0]], None, None)

    def gen_new_env (self, size, k):
        return INSN ('new_env', [], size, k)

    def gen_build_env (self, regs, k):
        return INSN ('build_env', regs, None, k)

    def gen_push_env (self, reg, k):
        return INSN ('push_env', [reg], None, k)

    def gen_pop_env (self, reg, k):
        return INSN ('pop_env', [reg], None, k)

    def gen_new_tuple (self, tag, size, k):
        return INSN ('new_tuple', [], (tag, size), k)

    def gen_store_tuple (self, offset, arg_reg, tuple_reg, i, n, k):
        return INSN ('store_tuple', [arg_reg, tuple_reg], (i, offset, n), k)

    def gen_varref (self, addr, is_top, var, k):
        return INSN ('varref', [], (addr, is_top, var), k)
    
    def gen_assign (self, addr, is_top, var, reg, k):
        return INSN ('varset', [reg], (addr, is_top, var), k)

    def gen_closure (self, fun, body, k):
        return INSN ('close', [], (fun, body, k[1]), k)

    def gen_test (self, test_reg, then_code, else_code, k):
        return INSN ('test', [test_reg], (None, then_code, else_code), k)

    def gen_simple_test (self, name, regs, then_code, else_code, k):
        return INSN ('test', regs, (name, then_code, else_code), k)

    def gen_pvcase (self, test_reg, types, alts, k):
        return INSN ('pvcase', [test_reg], (types, alts), k)

    def gen_nvcase (self, test_reg, dtype, alts, k):
        return INSN ('nvcase', [test_reg], (dtype, alts), k)

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
        #pretty_print (result)
        #remove_moves (result)
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
        elif exp.name in ('pvcase', 'nvcase'):
            types, alts = exp.params
            exp.params = types, [flatten (x) for x in alts]
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
        elif insn.name in ('pvcase', 'nvcase'):
            types, alts = insn.params
            for alt in alts:
                pretty_print (alt, depth+1)

# when <let> expressions are in a leaf position, the bindings may be
#  be stored in registers rather than an environment tuple.  due to 
#  the way the CPS algorithm works, there are a lot of redundant move
#  insns generated that we can ignore by remapping the relevant registers.                

# Ok, this doesn't work correctly [yet].  The problem comes up when varset
#   causes regs to get remapped - tests/t_bad_inline.scm fails.

def remove_moves (insns):
    map = {}
    for insn in insns:
        name = insn.name
        if insn.name == 'move':
            # a new entry in map
            src = insn.regs[0]
            # note: <src> may already be in the map!
            while map.has_key (src):
                # follow the chain of references
                src = map[src]
            # src == target sometimes happens, don't go all infinite loop.
            if insn.target != 'dead' and insn.target != src:
                print 'map %d == %d' % (insn.target, src)
                map[insn.target] = src
        # rename any that we can
        insn.regs = [ map.get(x,x) for x in insn.regs ]
        # special case
        if insn.name == 'test':
            name, then_code, else_code = insn.params
            remove_moves (then_code)
            remove_moves (else_code)
        elif insn.name == 'close':
            node, body, free = insn.params
            remove_moves (body)
        elif insn.name in ('pvcase', 'nvcase'):
            types, alts = insn.params
            for alt in alts:
                remove_moves (alt)
        if insn.name != 'move' and map.has_key (insn.target):
            # remove any that are blown away
            del map[insn.target]

def walk (insns):
    "iterate the entire tree of insns"
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
        elif insn.name in ('pvcase', 'nvcase'):
            types, alts = insn.params
            for alt in alts:
                for y in walk (alt):
                    yield y

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
        elif insn.name in ('pvcase', 'nvcase'):
            types, alts = insn.params
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
            if insn.name == 'primop':
                if insn.params[0] == '%make-tuple' and len(insn.regs):
                    # we're looking for non-immediate constructors (i.e., list/cons but not list/nil)
                    fun.allocates += 1
                elif insn.params[0] in ('%make-vector', '%extend-tuple'):
                    fun.allocates += 1
            elif insn.name in ('new_env', 'build_env', 'new_tuple', 'invoke', 'close', 'make_string'):
                fun.allocates += 1
        if verbose:
            print 'allocates %d %s' % (fun.allocates, fun.params[0].name)
