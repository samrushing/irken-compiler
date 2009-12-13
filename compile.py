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

class context:
    # maintain some context between passes
    def __init__ (self):
        self.datatypes = {}
        self.dep_graph = None
        self.scc_graph = None
        self.scc_map = None
        self.var_dict = None
        self.record_types = None
        self.cincludes = set()
        self.records2 = {}
        self.labels2 = {}
        self.variant_labels = {
            # Hack.  since pxll.h and header.c:dump_object() already
            #   know about the 'builtin' cons and nil constructors, we
            #   hard-code them in here.  You'll note that TC_PAIR is
            #   two entries before TC_USEROBJ, likewise with TC_NIL.
            'cons': -2,
            'nil': -2,
            }
        # don't throw away precious tag space.
        self.nvariant_offset = len(self.variant_labels)

def compile_file (f, name, safety=1, annotate=True, noinline=False, verbose=False, trace=False, step_solver=False, typetype=False):
    base, ext = os.path.splitext (name)
    r = lisp_reader.reader (f)
    exp = r.read_all()

    if verbose:
        print '--- read ---'
        pp (exp)

    # XXX might go easier on memory if we discarded the results of
    #  each pass after using it... 8^)

    c = context()

    t = transform.transformer (safety, c)
    exp2 = t.go (exp)
    if verbose:
        print '--- transform ---'
        pp (exp2)

    w = nodes.walker()
    exp3 = w.go (exp2)

    # alpha conversion
    c.var_dict = nodes.rename_variables (exp3, c.datatypes)
    # find strongly connected components
    c.dep_graph = graph.build_dependency_graph (exp3)
    c.scc_graph, c.scc_map = graph.strongly (c.dep_graph)
    # run the constraint generator and solver to find types
    t = solver.typer (c, verbose, step=step_solver)

    if typetype:
        t.go (exp3)

    if verbose:
        print '--- typing 1 ---'
        exp3.pprint()

    a = analyze.analyzer (c, safety, noinline, verbose)
    exp4 = a.analyze (exp3)

    if verbose:
        print '--- analyzer ---'
        exp4.pprint()

    t2 = solver.typer (c, verbose, step=step_solver)
    t.go (exp4)
    #import cProfile
    #cProfile.runctx ("t.go (exp4)", globals(), locals())
    if verbose:
        print '--- typing 2 ---'
        exp4.pprint()

    ic = cps.irken_compiler (c, safety=safety, verbose=verbose)
    exp5 = ic.go (exp4)

    if verbose:
        print '--- cps ---'
        cps.pretty_print (exp5)

    fo = open ('%s.c' % base, 'wb')
    num_regs = cps.the_register_allocator.max_reg
    b = backend.c_backend (fo, name, num_regs, c, safety=safety, annotate=annotate, trace=trace)
    b.emit (exp5)
    b.done()
    fo.close()

def cc (name, force_32=False, optimize=False, cc='gcc'):
    import os
    base, ext = os.path.splitext (name)
    uname = os.uname()
    machine = uname[-1]
    if machine == 'amd64':
        if force_32:
            arch = '-m32'
        else:
            arch = '-m64'
    else:
        arch = ''
    if optimize:
        #optimize = '-O3'
        optimize = '-O2'
    else:
        optimize = ''
    if uname[0] == 'Darwin':
        # doesn't work as of os x 10.6
        #cc = '/Developer/usr/bin/clang'
        # *does* work as of os x 10.6
        cc = '/Developer/usr/bin/llvm-gcc'
        arch += ' -fnested-functions'
        if not force_32:
            arch += ' -m64'
    cmd = '%s -I. -g %s %s %s.c -o %s' % (cc, arch, optimize, base, base)
    print cmd
    os.system (cmd)

from pprint import pprint as pp

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

    optimize = argtest ('-O')
    annotate = argtest ('-a')
    verbose  = argtest ('-v')
    trace    = argtest ('-t')
    noinline = argtest ('-ni')
    force_32 = argtest ('-f32')
    step_solver = argtest ('-ss')
    # run the type solver *before* inlining as well as after.
    typetype = argtest ('-tt')

    if '-s' in sys.argv:
        i = sys.argv.index ('-s')
        level = int (sys.argv[i+1])
        del sys.argv[i:i+2]
        safety = level
    else:
        safety = 1

    if '-f' in sys.argv:
        sys.argv.remove ('-f')
        name = sys.argv[1]
        compile_file (open (name, 'rb'), name, safety, annotate, noinline, verbose, trace, step_solver, typetype)
        cc (name, optimize=optimize)
