# -*- Mode: Python -*-

# run this as $ python util/install.py

import os
import sys
PJ = os.path.join

# if you change this, you should consider changing the default
#   value in self/context.scm to match it.
PREFIX = "/usr/local/"
IRKENLIB = PJ (PREFIX, "lib/irken/lib")
IRKENINC = PJ (PREFIX, "lib/irken/include")
IRKENBIN = PJ (PREFIX, "bin")

def getenv_or (name, default):
    v = os.getenv (name)
    if v is not None:
        return v
    else:
        return default

gcc = getenv_or ('CC', 'clang')
cflags = getenv_or ('CFLAGS', '-std=c99 -O3 -fomit-frame-pointer -I./include')
# NOTE: if you are tempted to put "-g" in there and your compiler is gcc,
#   you *must* add -fno-var-tracking as well or your compiles will never finish.
#   See: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=56510

def system (cmd):
    print cmd
    if os.system (cmd) != 0:
        sys.stderr.write ('system cmd failed: %r\n' % (cmd,))
        raise SystemError

def mkdir (path):
    if not os.path.isdir (path):
        system ('mkdir -p %s' % (path,))

mkdir (IRKENLIB)
mkdir (IRKENBIN)
mkdir (IRKENINC)

# copy library
for path in os.listdir ('lib'):
    if path.endswith ('.scm'):
        system ('cp -p %s %s' % (PJ ('lib', path), IRKENLIB))

# copy headers
headers = ['header1.c', 'gc1.c', 'pxll.h', 'rdtsc.h']
for path in headers:
    system ('cp -p include/%s %s' % (path, IRKENINC))

# we need a new binary with the new CFLAGS
print 'building new binary with updated CFLAGS for install...'
cflags = getenv_or ('CFLAGS', '-std=c99 -O3 -fomit-frame-pointer -g -I%s' % (IRKENINC,))
open ('self/flags.scm', 'wb').write (
"""
(define CC "%s")
(define CFLAGS "%s")
""" % (gcc, cflags))

# tricky, result code != 0
os.system ('self/compile self/compile.scm')

# copy binary
system ('cp -p self/compile %s/irken' % (IRKENBIN,))
