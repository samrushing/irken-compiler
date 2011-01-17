# -*- Mode: Python -*-

import lisp_reader
import transform
import graph
import nodes
import analyze
import cps
import backend
import os
import context

# path through the compiler:
# read -> transform -> node -> type -> analyze -> type -> cps -> backend
#
# transform includes builtin transforms and user macros (i.e., lib/derived.scm).
# typing may be done twice, depending on the '-tt' option.

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

    if c.standard_macros:
        r2 = lisp_reader.reader (open (c.standard_macros, 'rb'))
        macros = r2.read_all()
        exp[:0] = macros

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

    if c.verbose:
        print '--- node tree ---'
        exp3.pprint()

    if c.typetype:
        print 'type 1...'
        type_exp (c, exp3)
        if c.verbose:
            print '--- typing 1 ---'
            exp3.pprint()

    print 'inlining...'
    a = analyze.analyzer (c)
    exp4 = a.analyze (exp3)

    if c.verbose:
        exp4.pprint()

    print 'type 2...'
    type_exp (c, exp4)
        
    if c.verbose:
        exp4.pprint()

    print 'cps...'

    ic = cps.cps (c, verbose=c.verbose)
    exp5 = ic.go (exp4)

    if c.verbose:
        print '--- cps ---'
        cps.pretty_print (exp5)

    fo = open ('%s.c' % base, 'wb')
    #num_regs = cps.the_register_allocator.max_reg
    num_regs = ic.regalloc.max_reg
    print 'codegen...'
    b = backend.c_backend (fo, name, num_regs, c)
    b.go (exp5)
    fo.close()
    cc (name, c)

def type_exp (c, exp):
    # we have two different type solvers, so this fun abstracts that step.
    if False:
        import solver
        t = solver.typer (c, c.verbose, step=c.step_solver)
        return t.go (exp)
    else:
        import typing
        t = typing.typer (c)
        return t.go (exp)

def cc (name, context):
    import os
    base, ext = os.path.splitext (name)
    uname = os.uname()
    machine = uname[-1]
    cc = 'gcc'
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
        #cc = '/usr/local/bin/gcc'
        cc = 'gcc'
        arch += ' -fnested-functions'
        if not context.force_32:
            arch += ' -m64'
    if context.no_range:
        arch += ' -DNO_RANGE_CHECK '
    #cc = '/usr/local/bin/gcc -fplugin=/Users/rushing/src/dragonegg/dragonegg.so '
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
    c.print_types = argtest ('-pt')
    c.profile  = argtest ('-p')

    if len (sys.argv) != 2:
        W = sys.stderr.write
        W ("Usage: %s <irken-source-file>\n" % sys.argv[0])
        W ("Options:\n")
        W ("   -O : optimize\n")
        W ("   -a : annotate\n")
        W ("   -v : verbose\n")
        W ("   -t : emit tracing code\n")
        W ("   -ni : no inline; suppress most inlining\n")
        W ("   -f32 : force 32-bits on a 64-bit machine\n")
        W ("   -ss : single-step the type solver\n")
        W ("   -tt : 'type twice': run the type solver before inlining as well as after.\n")
        W ("   -nrc : no range checks\n")
        #W ("   -pt : print types\n")
    else:
        name = sys.argv[1]
        #import cProfile
        #cProfile.runctx ("compile_file (open (name, 'rb'), name, c)", globals(), locals())
        compile_file (open (name, 'rb'), name, c)
