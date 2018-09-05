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
cflags = getenv_or ('CFLAGS', '-std=c99 -O3 -fomit-frame-pointer -I./include -lffi')

sysname = platform.uname()[0]

if sysname == 'FreeBSD':
    # libffi is under /usr/local/
    cflags += ' -I/usr/local/include -L/usr/local/lib'
elif sysname == 'Linux':
    cflags += ' -D_GNU_SOURCE -ldl'

cmd = '%s vm/irkvm.c -o vm/irkvm %s' % (cc, cflags)
print cmd
os.system (cmd)
