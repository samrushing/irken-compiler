# -*- Mode: Python -*-

import lisp_reader
import transform
import lambda_tree
import typing
import analyze
import cps
import backend
import os

def read_string (s):
    import cStringIO
    import lisp_reader
    sf = cStringIO.StringIO (s)
    r = lisp_reader.reader (sf)
    return r.read()

def compile_file (f, name, safety=1, annotate=True, noinline=False, verbose=False, trace=False):
    base, ext = os.path.splitext (name)
    r = lisp_reader.reader (f)
    exp = r.read_all()

    if verbose:
        print '--- read ---'
        pp (exp)

    # XXX might go easier on memory if we discarded the results of
    #  each pass after using it... 8^)

    t = transform.transformer (safety)
    exp2 = t.go (exp)
    if verbose:
        print '--- transform ---'
        pp (exp2)

    w = lambda_tree.walker()
    exp3 = w.go (exp2)

    # alpha conversion
    var_dict = lambda_tree.rename_variables (exp3)

    typing.verbose = verbose    # hack

    typing.type_program (var_dict, exp3)
    if verbose:
        print '--- typing ---'
        exp3.pprint()

    a = analyze.analyzer (var_dict, safety, noinline, verbose)
    exp4 = a.analyze (exp3)

    if verbose:
        print '--- analyzer ---'
        exp4.pprint()

    c = cps.pxll_compiler (safety=safety, verbose=verbose)
    exp5 = c.go (exp4)

    if verbose:
        print '--- cps ---'
        cps.pretty_print (exp5)
        #import sys; sys.stdout.write ('-'*40 + '\n')
    fo = open ('%s.c' % base, 'wb')
    num_regs = cps.the_register_allocator.max_reg
    b = backend.c_backend (fo, name, num_regs, safety=safety, annotate=annotate, trace=trace)
    b.emit (exp5)
    b.done()
    fo.close()

def gcc (name, force_32=False, optimize=False):
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
        arch += ' -fnested-functions'
        if not force_32:
            arch += ' -m64'
    cmd = 'gcc -I. -g %s %s %s.c -o %s' % (arch, optimize, base, base)
    print cmd
    os.system (cmd)

from pprint import pprint as pp

tests = [
    # <lit>
    ("(begin 123 456)", "456"),
    # primops
    ("(%+ 3 5)", "8"),
    ("(%- (%+ 9 7) 4)", "12"),
    ("(%zero? 0)", "#t"),
    ("(%== 5 4)", "#f"),
    ("(%== 5 5)", "#t"),
    # <if>
    ("#t", "#t"),
    ("(if #t 3 4)", "3"),
    ("(if #f 3 4)", "4"),
    ("(%+ (if #t 10 0) 9)", "19"),
    # <%%cexp>
    ('(%%cexp "box(unbox(%s)+unbox(%s))" 3 4)', "7"),
    # <push_env>
    ("((lambda (x y) (%+ x y)) 3 5)", "8"),
    ("((lambda (x) 1 2 3) 5)", "3"),
    # <varref>
    ("((lambda (x) 1 2 x) 5)", "5"),
    # <varset>
    ("((lambda (x) (set! x 5) x) 3)", "5"),
    # <close>
    ("(lambda (x) x)", None),
    # <invoke_tail>
    #("((lambda2 (x) x) 5)", "5"),
    # <invoke>
    ("(begin (%printn 11) (%+ ((lambda (x) x) 5) ((lambda (x) (%printn 314) (%+ x 3)) 9)))", "11\n314\n17"),
    ("(%+ 11 ((lambda (x) x) 5))", "16"),
    # looping
    ("(let loop ((n 5)) (if (%zero? n) 1 (%* n (loop (%- n 1)))))", "120"),
    # tak
    ("""
  (let tak ((x 18) (y 12) (z 6))
    (if (%ge? y x)
        z
        (tak (tak (%- x 1) y z)
             (tak (%- y 1) z x)
             (tak (%- z 1) x y))))
""", "7"),
    # tak20
    ("""
(let loop ((n 20))
  (let tak ((x 18) (y 12) (z 6))
    (if (%ge? y x)
        z
        (tak (tak (%- x 1) y z)
             (tak (%- y 1) z x)
             (tak (%- z 1) x y))))
  (if (%zero? n)
      99
      (loop (%- n 1))))""", "99"),
    # count down from one million 0.040s on G5, 0.016 on amd64
    ("(let loop ((n 1000000)) (if (%zero? n) n (loop (%- n 1))))", "0"),
    # %getcc
    ("(let ((x 5) (y #f)) 1 2 (set! y (%getcc)) (%print 99) y)", None),
    # XXX I think this test is maybe asking too much of this
    #   simplistic implementation.  It works as long as call/cc
    #   doesn't get inlined, which is almost certainly because <k> at
    #   this point contains Lreturn: perhaps it's a simple as
    #   disallowing call/cc to be inlined, or making it a builtin,
    #   etc...  [hmm... this seems fairly clean - by forcing it to be
    #   an actual function call, <k> should contain the correct
    #   continuation?]
    #     # %putcc
    #      ("""
    #  (define (call/cc p)
    #    (let ((k (%getcc)))
    #      (p (lambda (r) (%putcc k r)))))
    #  (%printn 1)
    #  (%printn 2)
    #  (%printn 3)
    #  (%printn 4)
    #  (call/cc (lambda (exit) (%printn 5) (%printn 6) (exit 3) (%printn 7) (%printn 8)))
    #  (%printn 9)
    #  88
    #  """, "1\n2\n3\n4\n5\n6\n9\n88"),
    # random tests
    ("(let loop ((n 100) (a 0)) (if (%zero? n) a (loop (%- n 1) (%+ a n))))", "5050"),
    # strings
    ('"testing"', '"testing"'),
    ("(let loop ((n 5)) (if (%zero? n) 1 (%* n (loop (%- n 1)))))", "120"),
    # overflows on 32-bit
    #("(let fib ((n 50) (a 0) (b 1)) (if (%zero? n) a (fib (%- n 1) b (%+ a b))))", "12586269025"),
    ("(let fib ((n 42) (a 0) (b 1)) (if (%zero? n) a (fib (%- n 1) b (%+ a b))))", "267914296"),
    ("((lambda (x) 1 2 3) 5)", "3"),
    ("(%+ 3 ((lambda (x) 1 2 3) 5))", "6"),
    ("(let loop ((n 100) (a 0)) (if (%zero? n) a (loop (%- n 1) (%+ a n))))", "5050"),
    ('(let loop ((n 1000000)) (if (%zero? n) "done" (loop (%- n 1))))', '"done"'),

    # type testing
    #("123", "123"),
    #("(%+ 3 4)", "7"),
    #("(%+ (%== 1 2) 3)", None),

    # type equations testing
    #("(lambda (f) (%zero? (f f)))", None),
    #("((lambda (x y) (%+ x y)) 3 5)", None),
    #("(%+ (%== 1 2) 3)", None),
    #("(lambda (f) (f 11))", None),
    #("(lambda (f) (lambda (x) (%- (f 3) (f x))))", None),
    #("(let loop ((n 100) (a 0)) (if (%zero? n) a (loop (%- n 1) (%+ a n))))", "5050"),
    #("(lambda (f) (lambda (x) (%- (f 3) (f x))))", None),
    #('(let loop ((n 1000000)) (if (%zero? n) "done" (loop (%- n 1))))', '"done"'),
    ('(%printn "howdy")', '"howdy"\n#t'),
    ("((lambda (x) 1 2 x) 5)", "5"),
    ("(define (plus x y) (%+ x y)) (plus 3 4)", "7"),
    ("(let loop ((n 1000000)) (if (%zero? n) n (loop (%- n 1))))", "0"),
    #("(define (plus x y) (%+ x y)) (plus (plus (plus 3 4) 5) 8)", "20"),
    #("(define (plus x y) (%+ x y)) (let ((p plus)) (p 3 4))", "7"),
    ("(define (plus x y) (%+ x y))(plus 3 4)", "7"),
    ("(define (id x) x) (define (id2 y) y)", None),
    ("(%+ 3 4)", "7"),
    ("(if (%zero? 0) 21 99)", "21"),
    ("(%+ 3 (if (%zero? 0) 19 34))", "22"),
    ("'()", "()"),
    ("(let* ((x 42)) (%+ x x))", "84"),
    ("(%+ (let* ((x 42) (y (%+ x 3))) (%+ x y)) 3)", "90"),
    ("(let ((x 34)) ((lambda (x y) (%+ x y)) x x))", "68"),
    ("(let fib ((n 42) (a 0) (b 1)) (if (%zero? n) a (fib (%- n 1) b (%+ a b))))", "267914296"),
    ("(let loop ((n 1000000)) (if (%zero? n) n (loop (%- n 1))))", "0"),
    ("""(begin (%printn 11)
               (%+ ((lambda (x) x) 5)
                   ((lambda (x) (%printn 314) (%+ x 3)) 9))
                )""", "11\n314\n17"),
    ("""
  (let tak ((x 18) (y 12) (z 6))
    (if (%ge? y x)
        z
        (tak (tak (%- x 1) y z)
             (tak (%- y 1) z x)
             (tak (%- z 1) x y))))
""", "7"),
    ]


def t0():
    global tests
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
        compile_file (open (name, 'rb'), name, safety, annotate, noinline, verbose, trace)
        gcc (sys.argv[1], optimize=optimize)
        sys.exit (1)

    if '-l' in sys.argv:
        # only run the last test.
        tests = [tests[-1]]
    for e, expected in tests:
        sf = cStringIO.StringIO (e)
        compile_file (sf, 'test', safety, annotate, noinline, verbose, trace)
        gcc ('test', optimize=optimize)
        r = os.popen ('./test', 'r').read().strip()
        if expected is None:
            print r
        elif r != expected:
            print '=============================='
            print 'Expected %r - but got %r' % (expected, r)
            print '=============================='
            raise SystemError
        
if __name__ == '__main__':
    t0()
