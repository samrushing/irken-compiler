# -*- Mode: Python -*-

# build the vm.

import os
import platform

def getenv_or (name, default):
    v = os.getenv (name)
    if v is not None:
        return v
    else:
        return default

cc = getenv_or ('CC', 'clang')
cflags = getenv_or ('CFLAGS', '-std=c99 -O3 -I./include -lffi')

uname = platform.uname()
sysname = uname[0]
machine = uname[4]

if sysname == 'FreeBSD':
    # libffi is under /usr/local/
    cflags += ' -I/usr/local/include -L/usr/local/lib'
elif sysname == 'Linux':
    cflags += ' -D_GNU_SOURCE -ldl'

if machine in ('x86_64', 'amd64', 'x86'):
    # on intel hardware we want to free up a register.
    # NOTE: if you add this flag on an m1/m2 mac, it will
    #   silently change the default target to x86_64!
    cflags += ' -fomit-frame-pointer'

cmd = '%s vm/irkvm.c -o vm/irkvm %s' % (cc, cflags)
print (cmd)
os.system (cmd)
