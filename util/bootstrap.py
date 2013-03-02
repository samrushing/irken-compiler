# -*- Mode: Python -*-

# bootstrap the compiler.

import os
import platform

def getenv_or (name, default):
    v = os.getenv (name)
    if v is not None:
        return v
    else:
        return default

gcc = getenv_or ('CC', 'clang')
cflags = getenv_or ('CFLAGS', '-O3 -g -I.')

windows = platform.uname()[0] == 'Windows'

# this is frustrating, I could have sworn win32 would take either
# forward or backslash...
def tweak (s):
    if windows:
        return s.replace ('/', '\\')
    else:
        return s

def system (cmd):
    if windows:
        cmd = tweak (cmd)
    print cmd
    os.system (cmd)

def move (p0, p1):
    if windows:
        system ('move %s %s' % (p0, p1))
    else:
        system ('mv %s %s' % (p0, p1))

def copy (p0, p1):
    if windows:
        system ('copy %s %s' % (p0, p1))
    else:
        system ('cp %s %s' % (p0, p1))

open ('self/flags.scm', 'wb').write (
"""
(define CC "%s")
(define CFLAGS "%s")
""" % (gcc, cflags))

print 'protecting bootstrap compiler'
copy ('self/compile.c', 'self/compile.backup.c')

print 'compiling stage0 binary:'
system ('%s %s self/compile.c -o self/compile' % (gcc, cflags))

print 'compiling stage1 binary:'
system ('self/compile self/compile.scm')
move ('self/compile.c', 'self/compile.1.c')

print 'compiling stage2 binary:'
system ('self/compile self/compile.scm')
move ('self/compile.c', 'self/compile.2.c')

def diff (p0, p1):
    import os
    p0, p1 = tweak (p0), tweak (p1)
    if os.stat(p0).st_size == os.stat(p1).st_size:
        f0 = open (p0, 'rb')
        f1 = open (p1, 'rb')
        while 1:
            b0 = f0.read (32700)
            b1 = f1.read (32700)
            if b0 != b1:
                return False
            if not b0:
                break
        return True
            
# file comparison on windows?  duh, should just do it in python.
samesame = diff ('self/compile.1.c', 'self/compile.2.c')
if samesame:
    print 'stage1 and stage2 identical, party on wayne!'
    print 'consider running "python util/install.py" now'
else:
    print 'stage1 and stage2 output differs'

def unlink (p):
    try:
        os.unlink (tweak (p))
    except:
        pass

unlink ('self/compile.1.c')
move ('self/compile.2.c', 'self/compile.c')
unlink ('self/compile.backup.c')
