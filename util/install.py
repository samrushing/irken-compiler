# -*- Mode: Python -*-

# run this as $ python util/install.py

import os
import sys
PJ = os.path.join

# if you change this, you should consider changing the default
#   value in self/context.scm to match it.
# N.B.: you will also need to change the definition of FFI-PATH
#   in lib/ctype.scm.
PREFIX = "/usr/local/"

IRKENTOP = PJ (PREFIX, "lib/irken")
IRKENLIB = PJ (PREFIX, "lib/irken/lib")
IRKENINC = PJ (PREFIX, "lib/irken/include")
IRKENFFI = PJ (PREFIX, "lib/irken/ffi")
IRKENBIN = PJ (PREFIX, "bin")

def getenv_or (name, default):
    v = os.getenv (name)
    if v is not None:
        return v
    else:
        return default

def system (cmd):
    print (cmd)
    if os.system (cmd) != 0:
        sys.stderr.write ('system cmd failed: %r\n' % (cmd,))
        raise SystemError

def mkdir (path):
    if not os.path.isdir (path):
        system ('mkdir -p %s' % (path,))

def copy_tree (src, dst, patterns):
    for root, dirs, files in os.walk (src):
        mkdir (PJ (dst, root))
        for file in files:
            for pattern in patterns:
                if file.endswith (pattern):
                    path = PJ (root, file)
                    system ('cp -p %s %s/' % (path, PJ (dst, root)))

# copy library
copy_tree ('lib', IRKENTOP, ['.scm'])
# copy headers
# (.s: the native backends' preambles, e.g. amd-mach-preamble.s)
copy_tree ('include', IRKENTOP, ['.h', '.c', '.ll', '.s'])
# copy ffi
copy_tree ('ffi', IRKENTOP, ['_ffi.scm'])

# The installed compiler needs its own self/flags.scm, pointing at the
#   installed headers.  Build it in a scratch directory, with that
#   flags.scm there: -I puts the directory first in the search path, so
#   it's found before this tree's.  Nothing in this tree is written (so
#   nothing here ends up owned by root, under sudo).
import shutil
import tempfile

for path in (IRKENBIN, IRKENLIB):
    mkdir (path)

scratch = tempfile.mkdtemp (prefix='irken-install-')
try:
    os.mkdir (PJ (scratch, 'self'))
    flags = open ('self/flags.scm', 'r').read()
    # this way we pull in whatever decisions were made for bootstrap.py, updating only IRKENINC
    open (PJ (scratch, 'self', 'flags.scm'), 'w').write (flags.replace ('./include', IRKENINC))
    shutil.copy ('self/compile.scm', PJ (scratch, 'irken.scm'))
    src = PJ (scratch, 'irken.scm')

    print ('building new binary with updated CFLAGS for install...')
    system ('IRKENLIB=. self/compile -I %s %s -q' % (scratch, src))
    print ('building new bytecode image...')
    system ('IRKENLIB=. self/compile -I %s %s -b -q' % (scratch, src))

    # copy binary
    system ('cp -p %s %s/irken' % (PJ (scratch, 'irken'), IRKENBIN))
    # copy byte compiler image
    system ('cp -p %s %s/compile.byc' % (PJ (scratch, 'irken.byc'), IRKENLIB))
    # copy vm
    system ('cp -p vm/irkvm %s/irkvm' % (IRKENBIN,))
    # make irkc script
    irkc = PJ (scratch, 'irkc')
    open (irkc, 'w').write (
        "#!/bin/sh\n%s/irkvm %s/compile.byc $@\n" % (IRKENBIN, IRKENLIB)
    )
    os.chmod (irkc, 0o755)
    system ('cp -p %s %s/' % (irkc, IRKENBIN))
finally:
    shutil.rmtree (scratch, ignore_errors=True)
