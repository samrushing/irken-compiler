
import os
from pygments.cmdline import main as highlight

for root, dirs, files in os.walk ('.', topdown=False):
    for name in files:
        if name.endswith ('.py') or name.endswith ('.scm'):
            path = os.path.join (root, name)
            highlight (('pygmentize -O full -f html -o %s.html %s' % (path, path)).split())
