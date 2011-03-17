# -*- Mode: Python -*-

# bootstrap the compiler.

import os

def getenv_or (name, default):
    v = os.getenv (name)
    if v is not None:
        return v
    else:
        return default

gcc = getenv_or ('CC', 'gcc')
cflags = getenv_or ('CFLAGS', '-g -I.')

if os.uname()[0] == 'Darwin' and gcc == 'gcc':
    # stock xcode gcc has nested functions disabled by default
    cflags += ' -fnested-functions'

print 'compiling stage0 binary:'
cmd = '%s %s self/compile.c -o self/compile' % (gcc, cflags)
print cmd
os.system (cmd)

print 'compiling stage1 binary:'
cmd = 'self/compile self/compile.scm'
print cmd
os.system (cmd)
os.system ('mv self/compile.c self/compile.1.c')

print 'compiling stage2 binary:'
cmd = 'self/compile self/compile.scm'
print cmd
os.system (cmd)
os.system ('mv self/compile.c self/compile.2.c')

status = os.system ('diff self/compile.1.c self/compile.2.c')
if status != 0:
    print 'stage1 and stage2 output differs'
else:
    print 'stage1 and stage2 identical, party on wayne!'



