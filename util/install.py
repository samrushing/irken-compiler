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
    system ('cp -p %s %s' % (path, IRKENINC))

# copy binary
system ('cp -p self/compile %s/irken' % (IRKENBIN,))


