#!/usr/bin/env python3
# -*- Mode: Python -*-

# check that every library compiles on its own: a program that requires
#   lib/basis.scm and that one file, and nothing else.  A library that
#   uses something without requiring it fails with "unknown variable"
#   (or datatype, ...), and only works when its users happen to have
#   required the missing piece first.
#
#   $ python3 util/check_libs.py [-j JOBS] [path ...]
#
# run from the top of the source tree, after a bootstrap (it uses
#   self/compile, with the C backend, compiling only).  paths default
#   to every .scm under lib/ and doom/, minus test directories.  A
#   library whose FFI interface hasn't been generated on this machine
#   is skipped.

import argparse
import concurrent.futures
import glob
import os
import re
import subprocess
import sys
import tempfile

# files that aren't meant to compile alone, and why.
EXPECTED = {
    # pieces of a larger file, which includes them.
    'doom/timeq.scm':         'part of doom/scheduler.scm',
    'doom/epoll.scm':         'part of doom/scheduler.scm',
    'doom/ssh/codec.scm':     'part of doom/ssh/ssh.scm',
    'doom/ssh/transport.scm': 'part of doom/ssh/ssh.scm',
    'doom/ssh/cipher.scm':    'part of doom/ssh/ssh.scm',
    'doom/ssh/auth.scm':      'part of doom/ssh/ssh.scm',
    'doom/ssh/channel.scm':   'part of doom/ssh/ssh.scm',
    # the program must define server-host-pkey.
    'doom/ssh/ssh.scm':       'needs the program\'s server-host-pkey',
    # alternatives to what lib/basis.scm already has.
    'lib/aa_map.scm':         'an alternative to lib/basis.scm\'s tree',
    'lib/file.scm':           'an alternative to lib/io.scm',
    # the compiler loads it itself, as the standard macros.
    'lib/derived.scm':        'the standard macros',
    # known bug: the compiler halts on it ("Runtime Error, halting: cmp").
    'lib/stack2.scm':         'known compiler crash',
}

def libraries ():
    for top in ('lib', 'doom'):
        for path in sorted (glob.glob ('%s/**/*.scm' % (top,), recursive=True)):
            parts = path.split ('/')
            if 'test' in parts or 'tests' in parts:
                continue
            yield path

def check (path):
    d = tempfile.mkdtemp (prefix='irken-libcheck-')
    try:
        prog = os.path.join (d, 'x.scm')
        with open (prog, 'w') as f:
            f.write ('(require "lib/basis.scm")\n(require "%s")\n(printn 1)\n' % (path,))
        env = dict (os.environ, IRKENLIB='.')
        try:
            p = subprocess.run (
                ['self/compile', '-C', '-c', prog, '-q'], env=env,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=300
            )
        except subprocess.TimeoutExpired:
            return path, 'fail', 'timed out'
        out = p.stdout.decode ('utf-8', 'replace')
        if p.returncode == 0:
            return path, 'ok', ''
        m = re.search (r'failed to open file for read" "(ffi/\w+_ffi\.scm)"', out)
        if m:
            return path, 'skip', 'no %s here' % (m.group (1),)
        lines = [l for l in out.split ('\n') if 'error:' in l or 'Exception' in l or 'Runtime Error' in l]
        msg = lines[0] if lines else out.strip().split ('\n')[-1]
        msg = re.sub (r'^\S*/x\.scm:\S* ', '', msg)
        return path, 'fail', msg
    finally:
        for name in os.listdir (d):
            os.unlink (os.path.join (d, name))
        os.rmdir (d)

def main ():
    ap = argparse.ArgumentParser (description='check that each library compiles on its own')
    ap.add_argument ('-j', '--jobs', type=int, default=os.cpu_count() or 2)
    ap.add_argument ('paths', nargs='*')
    args = ap.parse_args()
    if not os.path.isfile ('self/compile'):
        sys.exit ('run from the top of the source tree, after a bootstrap (no self/compile)')
    paths = args.paths or list (libraries())
    counts = {'ok': 0, 'skip': 0, 'fail': 0, 'expected': 0}
    with concurrent.futures.ThreadPoolExecutor (args.jobs) as pool:
        for path, status, msg in pool.map (check, paths):
            if status == 'fail' and path in EXPECTED:
                status = 'expected'
            elif status == 'ok' and path in EXPECTED:
                print ('note: %s compiles now (remove it from EXPECTED)' % (path,))
            counts[status] += 1
            if status == 'fail':
                print ('FAIL %s\n    %s' % (path, msg))
            elif status == 'skip':
                print ('skip %s (%s)' % (path, msg))
    print ('%d libraries: %d ok, %d skipped (no FFI), %d expected to fail, %d FAIL' % (
        len (paths), counts['ok'], counts['skip'], counts['expected'], counts['fail']))
    sys.exit (1 if counts['fail'] else 0)

main()
