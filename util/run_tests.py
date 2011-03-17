#
# run all the regression tests
#
# [or, you can request specific ones by giving their names on the command line]
#

import sys
import os
import subprocess

def system (cmd):
    fo = open ('/dev/null', 'wb')
    p = subprocess.Popen (cmd, shell=True, stdout=fo, stderr=subprocess.STDOUT)
    return os.waitpid (p.pid, 0)[1]

def run_test (cmd, *args):
    cmd = PJ ('tests', cmd)
    p = subprocess.Popen ([cmd] + list(args), stdout=subprocess.PIPE)
    out = p.stdout.read()
    return out

def test_t17():
    lines = run_test ('t17').split ('\n')
    # we can't say anything about the address returned by malloc
    # but we should expect to read this number
    assert (lines[1] == '3141')
    # and the sizeof (pxll_int) is random too, since even on the same
    #  platform we might compile 32 or 64 bit, so just ignore it.

def test_t_dump_image():
    # generate the output
    run_test ('t_dump_image')
    # load the image and run it
    exp0 = run_test ('t_dump_image','-l')
    exp1 = open ('tests/t_dump_image.exp').read()
    assert (exp0 == exp1)

def test_t21():
    out = run_test ('t21')
    exp = open ('gc.c').read()
    # make sure the first part matches the contents of gc.c
    assert (out[:len(exp)] == exp)
    # the chars are too hard to tests for, and unlikely to be wrong.
    # should really make a separate char test.

def test_t22():
    out = run_test ('t22')
    lines = out.split ('\n')
    assert (lines[0].count ('<closure pc=') == 5)
    r6 = [ str(x) for x in range (6) ]
    assert (lines[1:] == (r6 + r6 + ['#u', '']))

def test_t_lex():
    out = run_test ('t_lex')
    assert (out.split('\n')[-4:] == ['{u0 NUMBER "42"}', '{u0 NEWLINE "\\0x0a"}', '"done"', ''])

def test_t_vm():
    out = run_test ('t_vm', 'vm/tests/t11.byc')
    assert (out.split()[-2:] == ['7', '#u'])

PJ = os.path.join

if len(sys.argv) > 1:
    # run only these specific tests
    files = [x + '.scm' for x in sys.argv[1:]]
else:
    files = os.listdir ('tests')

# When looking for things that are broken, I prefer to work with the smallest
#  test that reproduces a problem.  Thus, run the tests in source-size order...
files = [ (os.stat(PJ ('tests', x)).st_size, x) for x in files ]
files.sort()

# tests that need special handling
special = [x[5:] for x in dir() if x.startswith ('test_')]

failed = []

succeeded = 0

for size, file in files:
    if file.endswith ('.scm'):
        base, ext = os.path.splitext (file)
        path = os.path.join ('tests', file)
        print 'compiling', path
        fail = file.startswith ('f')
        code = system ('self/compile %s' % (path,))
        # need to address the return codes from the compiler...
        if code == 3584:
            if fail:
                failed.append ((base, 'compile did not fail like expected'))
            else:
                if base not in special:
                    out = run_test (base)
                    exp_path = PJ ('tests', base + '.exp')
                    if os.path.isfile (exp_path):
                        exp = open (exp_path).read()
                        if out != exp:
                            failed.append ((base, 'did not match expected output'))
                            #raise ValueError ("oops - output didn't match on test '%s'" % (base,))
                        else:
                            succeeded += 1
                    else:
                        succeeded += 1                    
                else:
                    # tests that require special handling for whatever reason.
                    try:
                        eval ('test_%s()' % (base,))
                    except:
                        failed.append ((base, 'assertion failed'))
                    else:
                        succeeded += 1

print '%d tests passed' % succeeded
if len(failed):
    print '%d tests failed!!' % (len(failed))
    for base, reason in failed:
        print base, reason
