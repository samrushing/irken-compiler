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
cflags = getenv_or ('CFLAGS', '-std=c99 -O3 -fomit-frame-pointer -I./include')

if platform.uname()[0] == 'FreeBSD':
    # libffi is under /usr/local/
    cflags += ' -I/usr/local/include -L/usr/local/lib'

cmd = '%s %s vm/irkvm.c -o vm/irkvm -lffi' % (cc, cflags)
print cmd
os.system (cmd)
