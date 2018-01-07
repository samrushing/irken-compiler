# -*- Mode: Python -*-

# measure the effect of 'inline-threshold' on the size and speed of the compiler.

class CommandFailed (Exception):
    pass

import time
import os

class Timer:
    def __enter__(self):
        self.start = time.time()
        return self
    def __exit__(self, *args):
        self.interval = time.time() - self.start

def system (cmd):
    print cmd
    if 0 != os.system (cmd):
        raise CommandFailed (cmd)

def do_one (val):
    # compile twice to reach steady state
    system ("self/compile self/compile.scm -q -i %d" % (val,))
    system ("self/compile self/compile.scm -q -i %d" % (val,))
    with Timer() as t:
        # time just the code generation, not the C compiler.
        for i in range (5):
            system ("self/compile self/compile.scm -q -c -i %d" % (val,))
    return (
        val,
        t.interval / 5.0,
        os.stat('self/compile.c').st_size,
        os.stat('self/compile').st_size
    )

values = []
for thresh in range (0, 21):
    r = do_one (thresh)
    print r
    values.append (r)

print values
