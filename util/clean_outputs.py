# -*- Mode: Python -*-

# find .c, .ll, and binaries for compiled irken
#   and nuke them all.

import os
import sys

if len(sys.argv) == 2:
    path = sys.argv[1]
else:
    path = '.'

pj = os.path.join

to_remove = []

for root, dirs, files in os.walk (path):
    for name in files:
        base, ext = os.path.splitext (name)
        if ext == '.scm':
            if base + '.c' in files:
                to_remove.append (pj (root, base + '.c'))
            if base + '.ll' in files:
                to_remove.append (pj (root, base + '.ll'))
            if base + '.byc' in files:
                to_remove.append (pj (root, base + '.byc'))
            if base in files:
                to_remove.append (pj (root, base))

PRECIOUS = [
    './self/compile',
    './self/compile.c',
    './self/compile.byc',
    './ffi/gen/genffi.byc',
    ]

for path in to_remove:
    if path not in PRECIOUS:
        os.unlink (path)
