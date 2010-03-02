import sys
import compile
import os

PJ = os.path.join
files = os.listdir ('tests')

# When looking for things that are broken, I prefer to work with the smallest
#  test that reproduces a problem.  Thus, run the tests in source-size order...
files = [ (os.stat(PJ ('tests', x)).st_size, x) for x in files ]
files.sort()

for size, file in files:
    if file.endswith ('.scm'):
        base, ext = os.path.splitext (file)
        path = os.path.join ('tests', file)
        print 'compiling', path
        fail = file.startswith ('f')
        try:
            compile.compile_file (open (path, 'rb'), path)
        except:
            if not fail:
                raise
        else:
            if fail:
                raise ValueError ("oops - expected compilation to fail")
            compile.cc (path, optimize=False)
            # XXX capture output and compare it
            os.system ('tests/%s' % (base,))
