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
cflags = getenv_or ('CFLAGS', '-std=c99 -O3 -fomit-frame-pointer -I./include')

class CommandFailed (Exception):
    pass

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
    if 0 != os.system (cmd):
        raise CommandFailed (cmd)

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

def unlink (p):
    try:
        os.unlink (tweak (p))
    except OSError:
        pass

class WorkInDir(object):
    def __init__ (self, path):
        self.orig = os.getcwd()
        self.path = path
    def __enter__ (self):
        os.chdir (self.path)
    def __exit__ (self, *args):
        os.chdir (self.orig)

def sys_in_dir (where, what):
    with WorkInDir (where):
        system (what)

open ('self/flags.scm', 'wb').write (
"""
(define CC "%s")
(define CFLAGS "%s")
""" % (gcc, cflags))

print 'copying the bootstrap compiler'
copy ('self/bootstrap.byc', 'self/compile.byc')

print 'building VM'
# system ('make vm')
execfile ('util/build_vm.py')

print 'generating posix FFI...'
sys_in_dir ('ffi', 'python gen.py posix.ffi')

print 'compiling with vm...'
system ('vm/irkvm self/compile.byc self/compile.scm -q')

print 'compiling stage0 binary:'
system ('%s %s self/compile.c -o self/compile' % (gcc, cflags))

print 'compiling stage1 binary:'
system ('self/compile self/compile.scm -q')
move ('self/compile.c', 'self/compile.1.c')

print 'compiling stage2 binary:'
system ('self/compile self/compile.scm -q')
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

unlink ('self/compile.1.c')
move ('self/compile.2.c', 'self/compile.c')
