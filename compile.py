# -*- Mode: Python -*-

import lisp_reader
import transform
import graph
import nodes
import solver
import analyze
import cps
import backend
import os
import context

def compile_file (f, name, c):
    base, ext = os.path.splitext (name)
    print 'read...'
    r = lisp_reader.reader (f)
    exp = r.read_all()

    if c.verbose:
        print '--- read ---'
        pp (exp)

    # XXX might go easier on memory if we discarded the results of
    #  each pass after using it... 8^)

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

    print 'typing...'
    # run the constraint generator and solver to find types
    t = solver.typer (c, c.verbose, step=c.step_solver)

    if c.typetype:
        t.go (exp3)

    if c.verbose:
        print '--- typing 1 ---'
        exp3.pprint()

    a = analyze.analyzer (c)
    exp4 = a.analyze (exp3)

    if c.verbose:
        print '--- analyzer ---'
        exp4.pprint()

    t2 = solver.typer (c, c.verbose, step=c.step_solver)
    t.go (exp4)
    #import cProfile
    #cProfile.runctx ("t.go (exp4)", globals(), locals())
    if c.verbose:
        print '--- typing 2 ---'
        exp4.pprint()

    ic = cps.cps (c, verbose=c.verbose)
    exp5 = ic.go (exp4)

    if c.verbose:
        print '--- cps ---'
        cps.pretty_print (exp5)

    fo = open ('%s.c' % base, 'wb')
    #num_regs = cps.the_register_allocator.max_reg
    num_regs = ic.regalloc.max_reg
    b = backend.c_backend (fo, name, num_regs, c)
    b.emit (exp5)
    b.done()
    fo.close()
    cc (name, c)

def cc (name, context):
    import os
    base, ext = os.path.splitext (name)
    uname = os.uname()
    machine = uname[-1]
    if machine == 'amd64':
        if context.force_32:
            arch = '-m32'
        else:
            arch = '-m64'
    else:
        arch = ''
    if context.optimize:
        #optimize = '-O3'
        optimize = '-O2'
    else:
        optimize = ''
    if uname[0] == 'Darwin':
        # doesn't work as of os x 10.6 [and won't work until clang supports lexical funs]
        #cc = '/Developer/usr/bin/clang'
        # *does* work as of os x 10.6
        #cc = '/Developer/usr/bin/llvm-gcc'
        cc = '/usr/local/bin/gcc'
        #arch += ' -fnested-functions'
        if not context.force_32:
            arch += ' -m64'
    if context.no_range:
        arch += ' -DNO_RANGE_CHECK '
    cmd = '%s -I. -g %s %s %s.c -o %s' % (cc, arch, optimize, base, base)
    print cmd
    os.system (cmd)

from pprint import pprint as pp

# raise this from the default 1000
import sys
sys.setrecursionlimit (10000)

if __name__ == '__main__':
    import os
    import sys
    import cStringIO

    def argtest (s):
        if s in sys.argv:
            sys.argv.remove (s)
            return True
        else:
            return False

    c = context.context()

    c.optimize = argtest ('-O')
    c.annotate = argtest ('-a')
    c.verbose  = argtest ('-v')
    c.trace    = argtest ('-t')
    c.noinline = argtest ('-ni')
    c.force_32 = argtest ('-f32')
    c.step_solver = argtest ('-ss')
    # run the type solver *before* inlining as well as after.
    c.typetype = argtest ('-tt')
    c.no_range = argtest ('-nrc')

    if '-f' in sys.argv:
        sys.argv.remove ('-f')
        name = sys.argv[1]
        compile_file (open (name, 'rb'), name, c)
