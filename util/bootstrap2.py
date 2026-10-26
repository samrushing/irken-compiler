# -*- Mode: Python -*-

# bootstrap the compiler.

import os
import platform
import contextlib

def getenv_or (name, default):
    v = os.getenv (name)
    if v is not None:
        return v
    else:
        return default

gcc = getenv_or ('CC', 'clang')
default_cflags = '-O3 -I./include'

plat = os.uname()
machine = plat[4]

if machine in ('x86_64', 'amd64', 'x86'):
    # on intel hardware we want to free up a register.
    # NOTE: if you add this flag on an m1/m2 mac, it will
    #   silently change the default target to x86_64!
    default_cflags += ' -fomit-frame-pointer'


if machine in ('amd64', 'x86_64'):
    irkbin = 'irkamd'
elif machine in ('arm64', 'aarch64'):
    irkbin = 'irkarm'
elif machine == 'riscv64':
    irkbin = 'irkrv'
else:
    irkbin = 'compile'

cflags = getenv_or ('CFLAGS', default_cflags)

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
    print (cmd)
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
    with contextlib.suppress(FileNotFoundError):
        os.remove (p)

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

unlink ('self/irken.scm')
os.symlink ('%s.scm' % irkbin, 'self/irken.scm')

open ('self/flags.scm', 'w').write (
"""
(define CC "%s")
(define CFLAGS "%s")
""" % (gcc, cflags))

print ('building VM')
# system ('make vm')
exec(open('util/build_vm.py').read())

print ('generating posix FFI...')
system ('IRKENLIB=. vm/irkvm ffi/gen/genffi.byc -gen ffi/posix.ffi')

# The asm backends take struct sizes, field offsets and constants from
#   ffi/*_ffi.scm (the C backend gets them from the real headers), so
#   these must match the build machine.  The committed copies may come
#   from another platform: e.g. macOS `struct kevent` is 32 bytes but
#   64 on FreeBSD 12+, which corrupts the heap in doom's poller.
# Regenerate the ones doom/tls/h2 use.  An interface whose headers or
#   library aren't installed here (kqueue on Linux, say) is left as-is.
# (stdio/libc are left alone: genffi on Linux currently produces a
#   stdio interface with a duplicate `stdin`.)
def regen_ffi (name):
    path = 'ffi/%s_ffi.scm' % (name,)
    saved = None
    if os.path.isfile (path):
        saved = open (path, 'rb').read()
    cmd = 'IRKENLIB=. vm/irkvm ffi/gen/genffi.byc -gen ffi/%s.ffi > /dev/null 2>&1' % (name,)
    if 0 == os.system (tweak (cmd)):
        print ('  %s: regenerated' % (name,))
    else:
        if saved is not None:
            open (path, 'wb').write (saved)
        print ('  %s: skipped (genffi failed; missing headers?)' % (name,))

print ('regenerating platform-dependent FFI interfaces...')
for name in ('socket', 'kqueue', 'sodium', 'zlib'):
    regen_ffi (name)

# irkenb.byc is not committed: build it from source with the
#   bootstrap compiler.  IRKENLIB=. matters here: without it, the FFI
#   interfaces come from any installed copy (/usr/local/lib/irken), and
#   constants missing from this tree's interfaces (e.g. SIGINFO) end up
#   in the bytecode.
print ('compiling the bytecode compiler (with vm)...')
system ('IRKENLIB=. vm/irkvm self/bootstrap.byc self/irkenb.scm -b -q')

print ('compiling native compiler (with vm)...')
system ('IRKENLIB=. vm/irkvm self/irkenb.byc self/irken.scm -q')

print ('compiling stage0 binary (with vm)...')
system ('IRKENLIB=. vm/irkvm self/irken.byc self/irken.scm -q')

print ('compiling stage1 binary (with stage0):')
system ('IRKENLIB=. self/irken self/irken.scm -q')
move ('self/irken.s', 'self/irken.1.s')

print ('compiling stage2 binary (with stage1):')
system ('IRKENLIB=. self/irken self/irken.scm -q')
move ('self/irken.s', 'self/irken.2.s')

def diff (p0, p1):
    import os
    p0, p1 = tweak (p0), tweak (p1)
    if os.stat(p0).st_size == os.stat(p1).st_size:
        f0 = open (p0, 'r')
        f1 = open (p1, 'r')
        while 1:
            b0 = f0.read (32700)
            b1 = f1.read (32700)
            if b0 != b1:
                return False
            if not b0:
                break
        return True

# file comparison on windows?  duh, should just do it in python.
samesame = diff ('self/irken.1.s', 'self/irken.2.s')
if samesame:
    print ('stage1 and stage2 identical, party on wayne!')
    print ('consider running "python util/install.py" now')
else:
    print ('stage1 and stage2 output differs')

unlink ('self/irken.1.s')
move ('self/irken.2.s', 'self/irken.s')
