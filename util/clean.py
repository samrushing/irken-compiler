# -*- Mode: Python -*-

import os

# Delete everything reachable from the directory named in 'top',
# assuming there are no symbolic links.
# CAUTION:  This is dangerous!  For example, if top == '/', it
# could delete all your disk files.

for root, dirs, files in os.walk ('.', topdown=False):
    if root.find ('.dSYM') != -1:
        # get rid of annoying MacOS .dSYM directories
        for name in files:
            os.remove (os.path.join (root, name))
        os.rmdir (root)
    else:
        for name in files:
            if name.endswith ('~') or name.endswith ('.pyc'):
                # get rid of emacs backup files and pyc files
                os.remove (os.path.join (root, name))

for root, dirs, files in os.walk ('./tests', topdown=False):
    for name in files:
        jp = os.path.join (root, name)
        if name.endswith ('.c'):
            os.remove (jp)
        else:
            stat = os.stat (jp)
            if stat.st_mode & 1:
                # an executable
                os.remove (jp)
