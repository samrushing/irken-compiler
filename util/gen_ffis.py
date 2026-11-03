# -*- Mode: Python -*-

# generate this platform's FFI interfaces with a native genffi.
#   exec'd by util/bootstrap.py and util/bootstrap2.py, which define
#   `system`, `tweak`, `step` and `CommandFailed`; `compiler` is the
#   compiler binary to build genffi with.

step ('compiling genffi (native)...')
system ('IRKENLIB=. %s ffi/gen/genffi.scm -q' % (compiler,))

# The asm backends take struct sizes, field offsets and constants from
#   ffi/*_ffi.scm (the C backend gets them from the real headers), so
#   these must match the build machine.  The committed copies may come
#   from another platform: e.g. macOS `struct kevent` is 32 bytes but
#   64 on FreeBSD 12+, which corrupts the heap in doom's poller.
# posix is always generated here (it's not committed).  An interface
#   whose headers or library aren't installed here (kqueue on Linux,
#   epoll elsewhere) is left as-is.
import subprocess

def start_gen (name):
    path = 'ffi/%s_ffi.scm' % (name,)
    saved = None
    if os.path.isfile (path):
        saved = open (path, 'rb').read()
    env = dict (os.environ, IRKENLIB='.')
    p = subprocess.Popen (
        [tweak ('ffi/gen/genffi'), '-gen', 'ffi/%s.ffi' % (name,)], env=env,
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )
    return (name, path, saved, p)

# these are independent, so run them all at once.
step ('generating platform-dependent FFI interfaces...')
jobs = [start_gen (name) for name in ('posix', 'socket', 'kqueue', 'epoll', 'sodium', 'zlib')]
for name, path, saved, p in jobs:
    if 0 == p.wait():
        print ('  %s: generated' % (name,))
    else:
        if saved is not None:
            open (path, 'wb').write (saved)
        print ('  %s: skipped (genffi failed; missing headers?)' % (name,))
        if name == 'posix':
            raise CommandFailed ('genffi -gen ffi/posix.ffi')
