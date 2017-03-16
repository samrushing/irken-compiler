# -*- Mode: Python -*-

# keep the last 5 compilers in case of nasty bugs that are
#   difficult to back out of.

import os
import re

files = []
for path in os.listdir ('./self'):
    m = re.match ('^compile([0-9])?$', path)
    if m is not None:
        if m.group(1) is None:
            num = 0
        else:
            num = int (m.group(1))
        files.append ((num, path))

files.sort()
files.reverse()

for num, path in files:
    #print 'self/%s' %path, 'self/compile%d' % (num+1)
    os.rename ('self/%s' %path, 'self/compile%d' % (num+1))    

os.system ('self/compile1 self/compile.scm')
