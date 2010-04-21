# -*- Mode: Python -*-

import os
import nodes
import lisp_reader
import graph
import analyze
import cps
import transform

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

class context:
    # maintain some context between passes
    def __init__ (self):
        self.datatypes = {}
        self.literals = {}

def compile_file (f, name, verbose=True):
    base, ext = os.path.splitext (name)
    print 'read...'
    r = lisp_reader.reader (f)
    exp = r.read_all()

    if verbose:
        print '--- read ---'
        pp (exp)

    c = context()

    print 'transform...'
    t = transform.transformer (c)
    exp2 = t.go (exp)
    if verbose:
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

    a = analyze.analyzer (c, True, verbose)
    exp4 = a.analyze (exp3)

    if verbose:
        print '--- analyzer ---'
        exp4.pprint()

    ic = byte_cps (c, verbose=verbose)
    exp5 = ic.go (exp4)

    if verbose:
        print '--- cps ---'
        cps.pretty_print (exp5)

    fo = open ('%s.byc' % base, 'wb')
    num_regs = cps.the_register_allocator.max_reg
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

class compiler:
    # byte-code compiler for vm/vm.scm (irken)
    def __init__ (self, fo, name, nregs, c):
        self.fo = fo
        self.name = name
        self.nregs = nregs
        self.context = c
        self.fun_addrs = {}

    def write (self, s):
        self.fo.write (s)

    def done (self):
        self.fo.close()

    def go (self, insns):
        self.emit_literals (self.context.literals)
        for x in self.emit (insns):
            self.fo.write (chr(x))
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
        return [opcodes.lit, insn.target] + self.encode_int (lit_index)

    def insn_return (self, insn):
        val_reg = insn.regs[0]
        return [opcodes.ret, val_reg]
    
    primops = {
        '%+' : 'add',
        '%-' : 'sub',
        '%=' : 'eq',
        }

    def insn_primop (self, insn):
        prim, = insn.params
        op = getattr (opcodes, self.primops[prim])
        return [op, insn.target] + insn.regs

    def insn_test (self, insn):
        ignore, then_code, else_code = insn.params
        # tst <reg> <then_size>
        # <then_code>
        # <jmp>
        # <else_code>
        then_code = self.emit (then_code)
        else_code = self.emit (else_code)
        then_code.extend ([opcodes.jmp] + self.encode_int (len(else_code)))
        return [opcodes.tst, insn.regs[0]] + self.encode_int (len (then_code)) + then_code + else_code

    def insn_close (self, insn):
        fun, body, free = insn.params
        body_code = self.emit (body)
        return [opcodes.fun, insn.target] + self.encode_int (len (body_code)) + body_code

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
        return [opcodes.arg, tuple_reg, arg_reg, i]

    def insn_varref (self, insn):
        addr, is_top, var = insn.params
        depth, index = addr
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
        return [opcodes.trcall, depth, len(regs)] + regs

if __name__ == '__main__':
    import sys
    name = sys.argv[1]
    f = open (name, 'rb')
    compile_file (f, name)
