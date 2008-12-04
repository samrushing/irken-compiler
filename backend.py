# -*- Mode: Python -*-

import sys
W = sys.stderr.write

class RegisterError:
    pass

class c_backend:

    def __init__ (self, out, src, num_regs, safety=1, annotate=True, trace=False):
        self.out = out
        self.src = src
        self.num_regs = num_regs
        self.indent = 1
        self.safety = safety
        self.annotate = annotate
        self.trace = trace
        self.toplevel_env = False
        self.write_header()

    def write_header (self):
        header = open ('header.c').read()
        tag = '%%%REGISTER_DECLARATIONS%%%'
        stop = header.find (tag)
        self.out.write (header[:stop])
        self.out.write (self.register_declarations())
        self.out.write (header[stop+len(tag):])

    def register_declarations (self):
        return '\n'.join (["  register object * r%d;" % i for i in range (self.num_regs + 1) ])

    def emit (self, insns):
        for insn in insns:
            name = 'insn_%s' % (insn.name,)
            if self.annotate:
                self.write ('// %s' % (insn.print_info(),))
            fun = getattr (self, name, None)
            fun (insn)
            if insn.typecheck:
                print insn, insn.typecheck
                self.emit_typecheck (insn)

    def emit_typecheck (self, insn):
        if insn.target != 'dead':
            self.write ('verify (r%d, TC_%s);' % (insn.target, insn.typecheck.upper()))

    def done (self):
        # close out the vm() function...
        self.out.write (
            " Lreturn:\n"
            "  return (pxll_int) result;\n"
            "}\n"
            )

    label_counter = 0

    def new_label (self):
        "generate a fresh unused label"
        self.label_counter += 1
        return 'L%d' % (self.label_counter,)

    safe_name_map = {'!':'_bang','*':'_splat','?':'_question','+':'_plus','-':'_'}

    def frob_name (self, name):
        l = []
        for ch in name.lower():
            if self.safe_name_map.has_key (ch):
                r = self.safe_name_map[ch]
            elif ch in 'abcdefghijklmnopqrstuvwxyz_0123456789':
                r = ch
            else:
                r = '_%02x' % (ord (ch),)
            l.append (r)
        r = ''.join (l)
        if r == '_':
            # special case
            r = 'minus'
        return r

    def function_label (self, fun):
        "generate/memoize a unique label for a known function"
        if hasattr (fun, 'label'):
            return fun.label
        else:
            if fun.name:
                fun.label = 'FUN_%d%s' % (fun.serial, self.frob_name (fun.name))
            else:
                fun.label = 'FUN_%d_lambda' % (fun.serial,)
            return fun.label

    def write (self, line):
        "write out a 'line' of indented code, followed by a newline"
        self.out.write ('  ' * self.indent)
        self.out.write (line)
        self.out.write ('\n')

    def verify (self, level, line):
        if self.safety >= level:
            self.write (line)

    def insn_lit (self, insn):
        # for now
        lit = insn.params
        if insn.target is 'dead':
            # why bother with dead literals?
            comment = '//'
        else:
            comment = ''
        self.write ('%sr%r = (object *) %d;' % (comment, insn.target, lit))

    def insn_return (self, insn):
        val_reg = insn.regs[0]
        self.verify (1, 'verify (k, TC_SAVE);')
        #self.write ('result = r%d; goto *k[3];' % val_reg)
        self.write ('PXLL_RETURN(%d);' % (val_reg,))

    def insn_jump (self, insn):
        result, target = insn.regs
        # this is really just a placeholder for a 'fall-off' continuation used by 'if'
        # in a native code compiler it might actually encode a jump, but because we are
        # using the if(){bracketed}else{form} it is unnecessary.
        if (target != 'dead') and result != target:
            self.write ('r%d = r%d;' % (target, result))

    # wrap_in, wrap_out: automatically box/unbox, etc... depending on type.
    def wrap_in (self, types, args):
        result = []
        for i in range (len (types)):
            if types[i] == 'int':
                result.append ('unbox(%s)' % (args[i],))
            else:
                result.append (args[i])
        return tuple (result)

    def wrap_out (self, type, exp):
        if type == 'int':
            return 'box(%s)' % (exp,)
        elif type == 'bool':
            # hmm... this is more like a cast, and should probably be
            # expressed as such.
            return 'PXLL_TEST(%s)' % (exp,)
        else:
            return exp

    def insn_primop (self, insn):
        regs = insn.regs
        primop = insn.params
        if primop[0] == '%cexp':
            ignore, form, sig = primop
            result_type, arg_types = sig
            regs = tuple('r%d' % x for x in regs)
            regs = self.wrap_in (arg_types, regs)
            exp = self.wrap_out (result_type, form % regs)
            if insn.target == 'dead':
                # probably a side-effect expression
                self.write ('%s;' % (exp,))
            else:
                self.write ('r%d = %s;' % (insn.target, exp))
        elif primop[0] == '%make-tuple':
            ignore, type, tag = primop
            regs = insn.regs
            nargs = len (regs)
            tag = 'TC_%s' % (tag.upper(),)
            self.write ('t = alloc_no_clear (%s, %d);' % (tag, nargs))
            for i in range (nargs):
                self.write ('t[%d] = r%d;' % (i+1, regs[i]))
            self.write ('r%d = t;' % (insn.target,))
        elif primop[0] == '%array-ref':
            [base, index] = insn.regs
            self.write ('range_check ((object*)r%d, unbox(r%d));' % (base, index))
            self.write ('r%d = ((pxll_vector*)r%d)->val[unbox(r%d)];' % (insn.target, base, index))
        elif primop[0] == '%array-set':
            [base, index, val] = insn.regs
            self.write ('range_check ((object*)r%d, unbox(r%d));' % (base, index))
            self.write ('((pxll_vector*)r%d)->val[unbox(r%d)] = r%d;' % (base, index, val))
        else:
            raise ValueError ("unknown primop")

    def insn_test (self, insn):
        name, then_code, else_code = insn.params
        if name:
            code, types = primop_types[name]
            regs = tuple ('r%d' % x for x in insn.regs)
            self.write ('if PXLL_IS_TRUE(%s) {' % (code % regs,))
        else:
            # this is a scheme-like definition of test/#t/#f
            self.write ('if PXLL_IS_TRUE(r%d) {' % (insn.regs[0],))
        self.indent += 1
        self.emit (then_code)
        self.indent -= 1
        self.write ('} else {')
        self.indent += 1
        self.emit (else_code)
        self.indent -= 1
        self.write ('}')

    def check_free_regs (self, free_regs):
        "describe the free_regs to new_env() for gc"
        # XXX do we need a bitfield, or an integer?
        # from everything I've seen, they're always in order ,
        # which makes sense because that's how the allocator works!
        # so let's just assume that and check for it here.
        n = len (free_regs)
        while n:
            n = n - 1
            if n not in free_regs:
                raise RegisterError ("Bad assumption about free registers!")

    def insn_new_env (self, insn):
        # val tuple is: <tag next val0 val1 val2 ...>
        size = insn.params
        # XXX our heap check is now done in a way that does not require looking at free regs.
        #self.check_free_regs (insn.regs)
        #self.write ('check_heap (%d); r%d = allocate (TC_TUPLE, %d);' % (len(insn.regs), insn.target, size + 1))
        self.write ('r%d = allocate (TC_TUPLE, %d);' % (insn.target, size + 1))
        if not self.toplevel_env:
            self.toplevel_env = True
            self.write ('top = r%d;' % (insn.target,))

    def c_string (self, s):
        r = repr(s)
        if r[0] == "'":
            # 'thingy"blue"thingy'
            return '"' + r.replace ('"', '\\"')[1:-1] + '"'
        else:
            # "thingy'blue'thingy"
            return r

    def insn_make_string (self, insn):
        s = insn.params
        ls = len(s)
        self.write ('r%d = allocate (TC_STRING, string_tuple_length (%d));' % (insn.target, ls))
        self.write (
            '{ pxll_string * s = (pxll_string *) r%d;'
            ' memcpy (s->data, %s, %d); s->len = %d; }' % (
                insn.target, self.c_string (s), ls, ls)
            )
        
    def insn_store_env (self, insn):
        [arg_reg, tuple_reg] = insn.regs
        i, n = insn.params
        self.verify (2, 'verify (r%d, TC_TUPLE);' % tuple_reg)
        self.write ('r%d[%d] = r%d;' % (tuple_reg, i+2, arg_reg))
        # XXX this is a bit of a hack. Because of the confusing implementation of compile_rands,
        #     we have no way of passing the tuple to its continuation (when it's needed)
        if insn.target != 'dead' and insn.target != tuple_reg:
            self.write ('r%d = r%d;' % (insn.target, tuple_reg))

    def insn_build_env (self, insn):
        regs = insn.regs
        size = len (regs)
        self.write ('t = allocate (TC_TUPLE, %d);' % (size + 1,))
        for i in range (len (regs)):
            self.write ('t[%d] = r%d;' % (i+2, regs[i]))
        self.write ('r%d = t;' % (insn.target,))

    def insn_push_env (self, insn):
        [args_reg] = insn.regs
        if self.trace:
            self.write ('stack_depth_indent(k); fprintf (stderr, ">> [%d] push\\n", __LINE__);')
        self.write ('r%d[1] = lenv; lenv = r%d;' % (args_reg, args_reg))

    def insn_pop_env (self, insn):
        [src] = insn.regs
        if self.trace:
            self.write ('stack_depth_indent(k); fprintf (stderr, "<< [%d] pop\\n", __LINE__);')
        self.write ('lenv = lenv[1];')
        if insn.target != 'dead' and src != insn.target:
            self.write ('r%d = r%d;' % (insn.target, src))

    def insn_varref (self, insn):
        addr, is_top, var = insn.params
        name = var.name
        depth, index = addr
        if insn.target != 'dead':
            if self.safety > 3:
                # a version that checks TC_TUPLE on every rib
                self.write ('r%d = varref_safe (%d, %d);' % (insn.target, depth, index))
            else:
                self.verify (2, 'verify (lenv, TC_TUPLE);')
                if is_top:
                    self.write ('r%d = top[%d];' % (insn.target, index+2))
                elif var.nary:
                    # ref the environment rib, not one particular argument
                    self.write ('r%d = ((object *%s) lenv) %s;' % (insn.target, '*' * depth, '[1]' * depth))
                else:
                    # gcc generates identical code for these, and the latter is cleaner.
                    #self.write ('r%d = ((object *%s) lenv) %s[%d];' % (insn.target, '*' * depth, '[1]' * depth, index+2))
                    self.write ('r%d = varref (%d,%d);' % (insn.target, depth, index))

    def insn_varset (self, insn):
        val_reg = insn.regs[0]
        addr, is_top, var = insn.params
        if insn.target != 'dead':
            print '[set! result used]',
        depth, index = addr
        if self.safety > 3:
            # a version that checks TC_TUPLE on every rib
            self.write ('varset_safe (%d, %d, r%d);' % (depth, index, val_reg))
        else:
            self.verify (2, 'verify (lenv, TC_TUPLE);')
            if is_top:
                self.write ('top[%d] = r%d;' % (index+2, val_reg))
            else:
                # gcc generates identical code for these, and the latter is cleaner.
                #self.write ('((object *%s)lenv)%s[%d] = r%d;' % ('*' * depth, '[1]' * depth, index+2, val_reg))
                self.write ('varset (%d, %d, r%d);' % (depth, index, val_reg))

    def insn_move (self, insn):
        reg_var, reg_src = insn.regs
        if reg_src is not None:
            # this is from varset
            self.write ('r%d = r%d;' % (reg_var, reg_src))
        else:
            # this is from varref
            # XXX not needed because of cps.remove_moves()
            #self.write ('r%d = r%d;' % (insn.target, reg_var))
            #if insn.target == reg_var:
            #    import pdb; pdb.set_trace()
            pass

    def insn_close (self, insn):
        fun, body, free = insn.params
        proc_label = self.function_label (fun)
        jump_label = self.new_label()
        # emit a jump over function definition
        self.write ('// def %r' % (fun.name,))
        self.write ('goto %s;' % (jump_label,))
        # the function definition itself
        self.write ('%s:' % (proc_label,))
        self.indent += 1
        if self.trace:
            self.write ('stack_depth_indent(k); fprintf (stderr, ">> [%%d] %s\\n", __LINE__);' % ((fun.name or 'lambda'),))
        if insn.allocates:
            self.check_free_regs (free)
            #print '*** function %r performs heap allocation ***' % (fun,)
            #self.write ('check_heap (%d);' % (len(free),))
            self.write ('check_heap();')
        self.emit (body)
        self.indent -= 1
        self.write ('%s:' % (jump_label,))
        # create a closure object
        self.write ('r%d = allocate (TC_CLOSURE, 2);' % (insn.target,))
        # unfortunately, this is a gcc extension - '&&' takes the
        #   address of a label.  is there some portable way to this?
        self.write ('r%d[1] = &&%s; r%d[2] = lenv;' % (insn.target, proc_label, insn.target))

    def insn_invoke_tail (self, insn):
        closure_reg, args_reg = insn.regs
        fun = insn.params
        # call
        self.verify (1, 'verify (r%d, TC_CLOSURE);' % (closure_reg,))
        #   extend closure's environment with args, jump
        if fun:
            label = 'goto %s' % (self.function_label (fun),)
        else:
            label = 'goto *r%d[1]' % (closure_reg,)
        self.write ('r%d[1] = r%d[2]; lenv = r%d; %s;' % (args_reg, closure_reg, args_reg, label))

    def insn_invoke (self, insn):
        closure_reg, args_reg = insn.regs
        free_regs, fun = insn.params
        return_label = self.new_label()
        nregs = len (free_regs)
        # save
        self.write ('t = allocate (TC_SAVE, %d);' % (3 + nregs))
        saves = []
        for i in range (nregs):
            saves.append ('t[%d] = r%d;' % (i+4, free_regs[i]))
        saves = ' '.join (saves)
        self.write ('t[1] = k; t[2] = lenv; t[3] = &&%s; %s k = t;' % (return_label, saves))
        # call
        self.verify (1, 'verify (r%d, TC_CLOSURE);' % (closure_reg,))
        #   extend closure's environment with args, jump
        if fun:
            label = 'goto %s' % (self.function_label (fun,),)
        else:
            label = 'goto *r%d[1]' % (closure_reg,)
        self.write ('r%d[1] = r%d[2]; lenv = r%d; %s;' % (args_reg, closure_reg, args_reg, label))
        # label
        self.write ('%s:' % (return_label,))
        if self.trace:
            if fun:
                fun_name = fun.name
            else:
                fun_name = 'lambda'
            self.write ('stack_depth_indent(k); fprintf (stderr, "<< [%%d] %s\\n", __LINE__);' % (fun_name,))
        # restore
        restores = []
        for i in range (nregs):
            restores.append ('r%d = k[%d];' % (free_regs[i], i+4))
        restores = ' '.join (restores)
        self.verify (1, 'verify (k, TC_SAVE);')
        self.write ('%s; lenv = k[2]; k = k[1];' % restores)
        if insn.target is not 'dead':
            self.write ('r%d = result;' % (insn.target,))

    def insn_tr_call (self, insn):
        regs = insn.regs
        depth, fun = insn.params
        # XXX do we not have access to the closure for this?
        if depth > 1:
            npop = depth - 1
            self.write ('lenv = ((object %s)lenv)%s;' % ('*' * npop, '[1]' * npop))
        nargs = len (regs)
        for i in range (nargs):
            self.write ('lenv[%d] = r%d;' % (2+i, regs[i]))
        self.write ('goto %s;' % (self.function_label (fun),))

    def insn_verify (self, insn):
        # XXX should really be a warning - how about 'UnDead'?
        assert (insn.target == 'dead')
        val_reg = insn.regs[0]
        tc, safety = insn.params
        self.verify (safety, 'verify (r%d, %s);' % (val_reg, tc))

    def insn_fetch_const (self, insn):
        index = insn.params
        self.write ('r%d = pxll_constants[%d];' % (insn.target, index))
