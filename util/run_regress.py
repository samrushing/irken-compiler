# Note: not needed any longer! remove me!
#
# compare the output of the stage0 and stage1 compiler
#

import sys
import os
import subprocess

def system (cmd):
    fo = open ('/dev/null', 'wb')
    p = subprocess.Popen (cmd, shell=True, stdout=fo, stderr=subprocess.STDOUT)
    return os.waitpid (p.pid, 0)[1]

PJ = os.path.join

files = os.listdir ('tests')

# When looking for things that are broken, I prefer to work with the smallest
#  test that reproduces a problem.  Thus, run the tests in source-size order...
files = [ (os.stat(PJ ('tests', x)).st_size, x) for x in files ]
files.sort()

missing = []
failed = []
success = []

for size, file in files:
    if file.endswith ('.scm'):
        base, ext = os.path.splitext (file)
        path = os.path.join ('tests', file)
        print 'compiling', path
        fail = file.startswith ('f')
        if not fail:
            system ('self/compilepy %s; mv tests/%s.c tests/%s.0.c' % (path, base, base))
            system ('self/compile   %s; mv tests/%s.c tests/%s.1.c' % (path, base, base))
            code = system ('diff -u tests/%s.0.c tests/%s.1.c' % (base, base))
            if code == 512:
                missing.append (base)
            elif code == 256:
                failed.append (base)
            elif code == 0:
                success.append (base)
            else:
                print 'confused about return code from diff', code
                raw_input()
            #print 'hit return to continue'
            #raw_input()

print 'failed', failed
print 'missing', missing
print 'success', success
