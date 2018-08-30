# -*- Mode: Python -*-

# keep the last 10 compilers in case of nasty bugs that are
#   difficult to back out of.

import os
import re

def rename_binaries():
    files = []
    for path in os.listdir ('./self'):
        m = re.match ('^compile([0-9])$', path)
        if m is not None:
            num = int (m.group(1))
            files.append ((num, path))

    files.sort()
    files.reverse()

    for num, path in files:
        #print 'self/%s' %path, 'self/compile%d' % (num+1)
        os.rename ('self/%s' %path, 'self/compile%d' % (num+1))
    # over the horizon...
    if os.path.isfile ('self/compile10'):
        os.unlink ('self/compile10')

os.rename ('self/compile', 'self/compile0')
if 0 == os.system ('self/compile0 self/compile.scm'):
    rename_binaries()
else:
    os.rename ('self/compile0', 'self/compile')
