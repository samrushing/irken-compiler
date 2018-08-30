# -*- Mode: Python -*-

import os

def getenv_or (name, default):
    v = os.getenv (name)
    if v is not None:
        return v
    else:
        return default

class CommandFailed (Exception):
    pass

def system (cmd):
    print cmd
    if 0 != os.system (cmd):
        raise CommandFailed (cmd)

system ('self/compile self/compile.scm -b -q')
system ('cp self/compile.byc self/bootstrap.byc')
system ('self/compile ffi/gen/genffi.scm -b -q')
