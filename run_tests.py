import sys
import compile
import os

files = os.listdir ('tests')
print files
for file in files:
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
