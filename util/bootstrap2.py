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

# clang if there is one, otherwise the system's cc (gcc on most Linux).
import shutil
gcc = getenv_or ('CC', 'clang' if shutil.which ('clang') else 'cc')
# (genffi, run below, also compiles C: make sure it uses the same compiler.)
os.environ['CC'] = gcc
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

import time
t_start = time.time()
def step (msg):
    print ('[%5.1fs] %s' % (time.time() - t_start, msg))

step ('building VM')
# system ('make vm')
exec(open('util/build_vm.py').read())

# The compiler needs only the libc and stdio interfaces (see lib/base.scm),
#   which are the same everywhere, so it can be built before anything is
#   generated for this platform.  The VM runs self/bootstrap.byc just once,
#   to compile the native compiler: everything after that runs natively.
# IRKENLIB=. matters: without it, FFI interfaces could come from an
#   installed copy (/usr/local/lib/irken).
native_flag = {'irkarm': '-arm', 'irkamd': '-amd', 'irkrv': '-rv', 'compile': ''}[irkbin]
# (with no native backend, the compiler's output is C.)
ext = '.c' if irkbin == 'compile' else '.s'

step ('compiling stage0 compiler (with vm)...')
system ('IRKENLIB=. vm/irkvm self/bootstrap.byc %s self/irken.scm -q' % (native_flag,))

step ('compiling stage1 compiler (with stage0)...')
system ('IRKENLIB=. self/irken self/irken.scm -q')
move ('self/irken' + ext, 'self/irken.1' + ext)

# Everything after stage1 is independent, so run it at once: stage2
#   (to check that the compiler reproduces itself), self/compile (all the
#   backends; util/test.py uses it), and genffi.  stage1 and stage2 are
#   the same compiler, so self/compile and genffi are built with a copy
#   of stage1 while stage2 is replacing self/irken.
# Each Irken process can use ~800MB (two semispaces, which are cleared),
#   so how much runs at once depends on how much memory there is.
import subprocess
copy ('self/irken', 'self/irken-stage1')

def phys_mem ():
    try:
        return os.sysconf ('SC_PAGE_SIZE') * os.sysconf ('SC_PHYS_PAGES')
    except (ValueError, OSError, AttributeError):
        return 0

slots = int (phys_mem() * 0.75 / (800 * 1024 * 1024))
parallel = slots >= 3
ffi_jobs = (slots - 2) if parallel else 1
if not parallel:
    step ('(not much memory: one step at a time)')

def background (what, cmd, log):
    if not parallel:
        step (what.replace (', in the background', ''))
        p = subprocess.Popen ('%s > %s 2>&1' % (cmd, log), shell=True)
        p.wait()
        return (cmd, log, p)
    step (what)
    return (cmd, log, subprocess.Popen ('%s > %s 2>&1' % (cmd, log), shell=True))

def wait_for (job):
    cmd, log, p = job
    if 0 != p.wait():
        print (open (log).read())
        raise CommandFailed (cmd)
    unlink (log)

stage2 = background (
    'compiling stage2 compiler (with stage1), in the background...',
    'IRKENLIB=. self/irken-stage1 self/irken.scm -q', 'self/irken.log'
)
build_compile = None
if irkbin != 'compile':
    build_compile = background (
        'building self/compile (all backends), in the background...',
        'IRKENLIB=. self/irken-stage1 self/compile.scm -q', 'self/compile.log'
    )

compiler = 'self/irken-stage1'
exec(open('util/gen_ffis.py').read())

def diff (p0, p1):
    p0, p1 = tweak (p0), tweak (p1)
    return open (p0, 'rb').read() == open (p1, 'rb').read()

step ('waiting for stage2...')
wait_for (stage2)
if diff ('self/irken.1' + ext, 'self/irken' + ext):
    print ('stage1 and stage2 identical, party on wayne!')
else:
    print ('stage1 and stage2 output differs')
unlink ('self/irken.1' + ext)

if build_compile is not None:
    step ('waiting for self/compile...')
    wait_for (build_compile)

unlink ('self/irken-stage1')
step ('done.  consider running "python util/install.py" now')
