# -*- Mode: Python -*-

import os
import nodes
import lisp_reader
import graph
import analyze
import cps
import transform
import context

is_a = isinstance
from pdb import set_trace as trace
from pprint import pprint as pp

# what's the idea?
#
# well, we want to prototype a byte-code compiler that will eventually
#  be written in Irken.  While doing this, we will write the VM for this
#  bytecode, which we can test using this module.
#
# now, what language shall we compile?
#
# We *could* start leaning toward python right now, but a much easier
#  path would be to use the Irken front end.  How about this:
#
# reader => transform => [skip typer] => analyze => cps
#
# Since we plan to implement an untyped language, we skip the typing phase.
#
# [we might also want to skip analyze/inlining?  can we?]

def compile_file (f, name, c):
    base, ext = os.path.splitext (name)
    print 'read...'
    r = lisp_reader.reader (f)
    exp = r.read_all()

    if c.verbose:
        print '--- read ---'
        pp (exp)

    print 'transform...'
    t = transform.transformer (c)
    exp2 = t.go (exp)
    if c.verbose:
        print '--- transform ---'
        pp (exp2)

    w = nodes.walker (c)
    exp3 = w.go (exp2)

    print 'rename...'
    # alpha conversion
    c.var_dict = nodes.rename_variables (exp3, c)
    # find strongly connected components
    print 'call graph...'
    c.dep_graph = graph.build_dependency_graph (exp3)
    c.scc_graph, c.scc_map = graph.strongly (c.dep_graph)

    a = analyze.analyzer (c)
    exp4 = a.analyze (exp3)

    if c.verbose:
        print '--- analyzer ---'
        exp4.pprint()

    ic = byte_cps (c, verbose=c.verbose)
    exp5 = ic.go (exp4)

    if c.verbose:
        print '--- cps ---'
        cps.pretty_print (exp5)

    fo = open ('%s.byc' % base, 'wb')
    num_regs = ic.regalloc.max_reg
    b = compiler (fo, name, num_regs, c)
    b.go (exp5)
    fo.close()

class byte_cps (cps.cps):
    # here, we'll override Irken behavior to get bytecode behavior

    def compile_primapp (self, tail_pos, exp, lenv, k):
        return self.compile_primargs (exp.args, (exp.name,), lenv, k)

    def compile_literal (self, tail_pos, exp, lenv, k):
        lits = self.context.literals
        lt, lv = key = exp.ltype, exp.value
        if lits.has_key (key):
            index = lits[key]
        else:
            index = len(lits)
            lits[key] = index
        return self.gen_lit (index, k)

    def gen_lit (self, index, k):
        return cps.INSN ('lit', [], index, k)

class opcodes:
    lit    =  0
    ret    =  1
    add    =  2
    sub    =  3
    eq     =  4
    tst    =  5
    jmp    =  6
    fun    =  7
    tail   =  8
    tail0   = 9
    env    = 10
    arg    = 11
    ref    = 12
    mov    = 13
    push   = 14
    trcall = 15
    ref0   = 16
    call   = 17
    pop    = 18
    ge     = 19

class label:
    counter = 0
    def __init__ (self):
        self.val = label.counter
        label.counter += 1
    def __repr__ (self):
        return '<L%d>' % (self.val,)

class label_ref:
    def __init__ (self, label):
        self.val = label.val
    def __repr__ (self):
        return '<R%d>' % (self.val)

class compiler:
    # byte-code compiler for vm/vm.scm (irken)
    def __init__ (self, fo, name, nregs, c):
        self.fo = fo
        self.name = name
        self.nregs = nregs
        self.context = c
        self.fun_labels = {}

    def write (self, s):
        self.fo.write (s)

    def done (self):
        self.fo.close()

    def go (self, insns):
        self.emit_literals (self.context.literals)
        # find label offsets
        pc = 0
        labels = {}
        refs = []
        r = []
        for x in self.emit (insns):
            if is_a (x, label):
                labels[x.val] = pc
            elif is_a (x, label_ref):
                refs.append ((x, pc))
                # placeholder
                r.append (None)
                pc += 1
            else:
                r.append (x)
                pc += 1
        # fill in label refs
        for lab, offset in refs:
            assert (r[offset] is None)
            r[offset] = self.encode_int (labels[lab.val])
        # render
        for x in r:
            if is_a (x, int):
                self.fo.write (chr (x))
            else:
                for y in x:
                    self.fo.write (chr (y))
        print r
        for i in range (len (r)):
            print '%3d %r' % (i,r[i])
        self.done()

    def emit (self, insns):
        r = []
        for insn in insns:
            name = 'insn_%s' % (insn.name,)
            fun = getattr (self, name, None)
            r.extend (fun (insn))
        return r

    def emit_literals (self, literals):
        # convert into a list
        l = [None] * len(literals)
        for k, v in literals.iteritems():
            l[v] = k
        # emit the encodings
        for kind, val in l:
            if kind is 'int':
                if val >= 0:
                    bytes = self.int_bytes (val)
                    self.write ('+%s' % (bytes))
                else:
                    bytes = self.int_bytes (-val)
                    self.write ('-%s' % (bytes))
            elif kind is 'bool':
                if val == 'true':
                    self.write ('T')
                else:
                    self.write ('F')
            else:
                raise NotImplementedError
        self.write ('.')
                
    def encode_int (self, n):
        if n < 0:
            raise ValueError
        elif n < 255:
            return [n]
        else:
            bytes = []
            while n:
                bytes.insert (0, n & 0xff)
                n >>= 8
            if len(bytes) > 255:
                raise ValueError ("integer too large")
            else:
                bytes.insert (0, len(bytes))
                bytes.insert (0, 0xff)
                return bytes

    def int_bytes (self, n):
        return ''.join ([chr(x) for x in self.encode_int (n)])

    def insn_lit (self, insn):
        lit_index = insn.params
        return [opcodes.lit, insn.target, self.encode_int (lit_index)]

    def insn_return (self, insn):
        val_reg = insn.regs[0]
        return [opcodes.ret, val_reg]
    
    primops = {
        '%+' : 'add',
        '%-' : 'sub',
        '%=' : 'eq',
        '%>=' : 'ge',
        }

    def insn_primop (self, insn):
        prim, = insn.params
        op = getattr (opcodes, self.primops[prim])
        return [op, insn.target] + insn.regs

    def insn_test (self, insn):
        ignore, then_code, else_code = insn.params
        # TST <reg> L0
        # <then_code>
        # JMP L1
        # L0:
        # <else_code>
        # L1:
        l0 = label()
        l1 = label()
        then_code = self.emit (then_code)
        else_code = self.emit (else_code)
        then_code.extend ([opcodes.jmp, label_ref (l1), l0])
        return [opcodes.tst, insn.regs[0], label_ref (l0)] + then_code + else_code + [l1]

    def insn_close (self, insn):
        fun, body, free = insn.params
        l0 = label()
        l1 = label()
        if fun.name:
            self.fun_labels[fun.name] = l1
        body_code = self.emit (body)
        # FUN <trg> L0 <code> L0:
        return [opcodes.fun, insn.target, label_ref (l0), l1] + body_code + [l0]

    def insn_invoke_tail (self, insn):
        closure_reg, args_reg = insn.regs
        if args_reg is None:
            return [opcodes.tail0, closure_reg]
        else:
            return [opcodes.tail, closure_reg, args_reg]

    def insn_new_env (self, insn):
        size = insn.params
        return [opcodes.env, insn.target, size]

    def insn_store_tuple (self, insn):
        [arg_reg, tuple_reg] = insn.regs
        i, offset, n = insn.params
        r = [opcodes.arg, tuple_reg, arg_reg, i]
        # XXX this is a bit of a hack. Because of the confusing implementation of compile_rands,
        #     we have no way of passing the tuple to its continuation (when it's needed)
        if insn.target != 'dead' and insn.target != tuple_reg:
            print 'tuple move hack'
            trace()
            r.extend ([opcodes.mov, insn.target, tuple_reg])
        return r

    def insn_varref (self, insn):
        addr, is_top, var = insn.params
        depth, index = addr
        if depth == 0:
            return [opcodes.ref0, insn.target, index]
        else:
            return [opcodes.ref, insn.target, depth, index]

    def insn_move (self, insn):
        reg_var, reg_src = insn.regs
        if reg_src is not None:
            # from varset
            return [opcodes.mov, reg_var, reg_src]
        elif insn.target != 'dead':
            return [opcodes.mov, insn.target, reg_var]
        else:
            # dead move
            pass

    def insn_push_env (self, insn):
        [args_reg] = insn.regs
        return [opcodes.push, args_reg]

    def insn_tr_call (self, insn):
        regs = insn.regs
        depth, fun = insn.params
        # XXX track this issue down...
        depth = depth - 1
        # TRCALL <&L0> <depth> <nregs> <reg0> <reg1> ...
        l0 = self.fun_labels[fun.name]
        return [opcodes.trcall, label_ref (l0), depth, len(regs)] + regs

    def check_regs (self, regs):
        # verify that a set of registers is 'complete', e.g., 4,3,2,1,0
        # this should catch any change to register allocation and alert us.
        n = len (regs)
        while n:
            n = n - 1
            if n not in regs:
                raise RegisterError ("register set not complete")

    def insn_invoke (self, insn):
        closure_reg, args_reg = insn.regs
        free_regs, fun = insn.params
        nregs = len (free_regs)
        self.check_regs (free_regs)
        # turn this into: SAVE, INVOKE, RESTORE
        # SAVE/INVOKE can be combined into one insn, 'call'.
        # RESTORE will have to be its own insn, since RETURN needs
        #   to support tail calls.
        return [opcodes.call, closure_reg, args_reg, nregs, opcodes.pop, insn.target]

if __name__ == '__main__':
    import sys

    def argtest (s):
        if s in sys.argv:
            sys.argv.remove (s)
            return True
        else:
            return False

    c = context.context()

    c.verbose  = argtest ('-v')
    c.noinline = argtest ('-ni')
    c.literals = {}

    name = sys.argv[1]
    f = open (name, 'rb')

    compile_file (f, name, c)
