# -*- Mode: Python -*-

# llvm circa nov 2017 has an unfun feature: if your calling
#  conventions do not match between use and def, it will silently
#  create a broken (and tiny) binary rather than telling you where you
#  screwed up.
# this utility compares use & def for 'fastcc', since we have a mixture of
#  the two calling conventions.

from __future__ import print_function
import sys

def find_name (line):
    parts = line.split()
    for part in parts:
        if part[0] == '@':
            name = part.split('(')[0]
            fast = ('fastcc' in parts)
            return name, fast
    else:
        raise ValueError (line)

d_decl = {}
d_defs = {}
d_uses = {}

path = sys.argv[1]
fd = open (path, 'r')
while 1:
    line = fd.readline()
    if not line:
        break
    elif line.startswith ('declare '):
        k, v = find_name(line)
        d_decl[k] = v
    elif line.startswith ('define '):
        k, v = find_name(line)
        d_defs[k] = v
    else:
        parts = line.split()
        if 'call' in parts:
            try:
                k, v = find_name (line)
                if k in d_uses:
                    d_uses[k].append (v)
                else:
                    d_uses[k] = [v]
            except ValueError:
                print ('no fun: ' + line[:-1])

# now that we've scanned everything, check that all uses are correct.
for fun, values in d_uses.items():
    for fast in values:
        if fun in d_defs:
            if d_defs[fun] != fast:
                print (fun, end=' ')
        elif fun in d_decl:
            if d_decl[fun] != fast:
                print (fun, end=' ')
        else:
            raise ValueError ("unknown fun?: %s" % fun)
