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
        compile.compile_file (open (path, 'rb'), path)
        compile.cc (path, optimize=True)
        os.system ('tests/%s' % (base,))
