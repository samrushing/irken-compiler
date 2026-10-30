# -*- Mode: Python -*-

# run the tests under test/ with every backend this machine can run.
#
#   python3 util/test.py                    # everything
#   python3 util/test.py test/records       # a directory
#   python3 util/test.py test/core/t_arith.scm -B c,arm -v
#
# a test is an ordinary irken program.  comments in it say what to expect:
#
#   ;; => some text       a line of expected output, in order.  (";; =>"
#                         alone is an empty line.)  the native backends'
#                         "{total ticks...}" and "[gc...]" lines are ignored.
#   ;; expect-error: txt  the program must fail to compile, with txt
#                         somewhere in the compiler's output.
#   ;; skip: amd rv       don't run with these backends (or platforms,
#                         e.g. Darwin), with an optional reason after "--".
#   ;; xfail: arm -- why  expected to fail: a known bug.  reported, but
#                         doesn't fail the run (and an unexpected pass
#                         is reported too).
#
# backends: c (the C backend), b (bytecode, run with vm/irkvm), and the
#   native one for this machine: arm (arm64), amd (x86_64) or rv (riscv64).
#
# each test is compiled in a scratch directory, one per backend, so the
#   backends can run in parallel without trampling each other's output.
#   run from the top of the source tree: the compiler is self/compile, and
#   IRKENLIB defaults to ./

import os
import re
import sys
import shutil
import platform
import argparse
import tempfile
import subprocess
import concurrent.futures

PJ = os.path.join

NOISE = re.compile (r'^(\{total ticks: \d+ gc ticks: \d+\}|\[gc\.\.\..*)$')

def host_backends ():
    r = ['c']
    if os.path.isfile ('vm/irkvm'):
        r.append ('b')
    machine = platform.machine().lower()
    if machine in ('arm64', 'aarch64'):
        r.append ('arm')
    elif machine in ('x86_64', 'amd64'):
        r.append ('amd')
    elif machine.startswith ('riscv64'):
        r.append ('rv')
    return r

class Test:
    def __init__ (self, path):
        self.path = path
        self.name = os.path.splitext (path)[0]
        self.expected = []
        self.expect_error = None
        self.skip = {}      # backend/platform -> reason
        self.xfail = {}
        for line in open (path, encoding='utf-8'):
            s = line.strip()
            if s.startswith (';; =>'):
                self.expected.append (s[5:][1:] if s[5:].startswith (' ') else s[5:])
            elif s.startswith (';; expect-error:'):
                self.expect_error = s[len(';; expect-error:'):].strip()
            elif s.startswith (';; skip:') or s.startswith (';; xfail:'):
                kind, rest = s[3:].split (':', 1)
                names, _, reason = rest.partition ('--')
                d = self.skip if kind == 'skip' else self.xfail
                for n in names.split():
                    d[n] = reason.strip()

def run (cmd, timeout, cwd=None, env=None):
    try:
        p = subprocess.run (cmd, cwd=cwd, env=env, timeout=timeout,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        return p.returncode, p.stdout.decode ('utf-8', 'replace')
    except subprocess.TimeoutExpired as e:
        out = (e.stdout or b'').decode ('utf-8', 'replace')
        return 'timeout', out

def run_one (test, backend, scratch, args):
    """-> (status, detail): status is one of pass fail skip xfail xpass"""
    osname = platform.system()
    for key in (backend, osname):
        if key in test.skip:
            return 'skip', test.skip[key]
    xfail = test.xfail.get (backend, test.xfail.get (osname))
    # copy the source into this backend's scratch directory.
    work = PJ (scratch, backend, os.path.dirname (test.path))
    os.makedirs (work, exist_ok=True)
    src = PJ (work, os.path.basename (test.path))
    shutil.copy (test.path, src)
    base = os.path.splitext (src)[0]
    flag = {'c': [], 'b': ['-b'], 'arm': ['-arm'], 'amd': ['-amd'], 'rv': ['-rv']}[backend]
    env = dict (os.environ)
    env.setdefault ('IRKENLIB', './')
    rc, cout = run (['self/compile'] + flag + [src, '-q'], args.timeout * 4, env=env)
    if test.expect_error is not None:
        if rc == 0:
            return result (xfail, 'fail', 'compiled, but expected an error: ' + test.expect_error)
        elif test.expect_error in cout:
            return result (xfail, 'pass', '')
        else:
            return result (xfail, 'fail', 'wrong error; expected %r in:\n%s' % (test.expect_error, tail (cout)))
    if rc != 0:
        return result (xfail, 'fail', 'compile failed (%s):\n%s' % (rc, tail (cout)))
    if backend == 'b':
        cmd = ['vm/irkvm', base + '.byc']
    else:
        cmd = [base]
    rc, out = run (cmd, args.timeout, env=env)
    lines = [l for l in out.split ('\n') if not NOISE.match (l)]
    while lines and lines[-1] == '':
        lines.pop()
    # a program's exit status comes from its final value, so it isn't
    #  meaningful by itself; but dying from a signal or a timeout is.
    if rc == 'timeout' or rc < 0:
        return result (xfail, 'fail', '%s; output:\n%s' % (
            'timed out' if rc == 'timeout' else 'killed by signal %d' % (-rc,), tail ('\n'.join (lines))))
    if lines != test.expected:
        return result (xfail, 'fail', diff (test.expected, lines))
    return result (xfail, 'pass', '')

def result (xfail, status, detail):
    if xfail is None:
        return status, detail
    elif status == 'fail':
        return 'xfail', xfail
    else:
        return 'xpass', 'expected to fail (%s), but passed' % (xfail,)

def tail (s, n=12):
    lines = s.rstrip().split ('\n')
    return '\n'.join ('    ' + l for l in lines[-n:])

def diff (expected, got):
    r = []
    for i in range (max (len (expected), len (got))):
        e = expected[i] if i < len (expected) else '<missing>'
        g = got[i] if i < len (got) else '<missing>'
        if e != g:
            r.append ('    line %d: expected %r\n    %s  got %r' % (i + 1, e, ' ' * len (str (i + 1)), g))
        if len (r) >= 6:
            r.append ('    ...')
            break
    return 'wrong output:\n' + '\n'.join (r)

def find_tests (paths):
    r = []
    for p in paths:
        if os.path.isdir (p):
            for root, dirs, files in os.walk (p):
                dirs.sort()
                for f in sorted (files):
                    if f.endswith ('.scm') and not f.startswith ('_'):
                        r.append (PJ (root, f))
        else:
            r.append (p)
    return r

def newest (paths):
    return max ((os.stat (p).st_mtime for p in paths if os.path.isfile (p)), default=0)

def sources (*dirs):
    return [PJ (d, f) for d in dirs for f in os.listdir (d) if f.endswith ('.scm')]

def warn_stale ():
    # a stale compiler or VM gives confusing results: say so up front.
    def check (binary, srcs, how):
        if os.path.isfile (binary) and newest (srcs) > os.stat (binary).st_mtime:
            print ('warning: %s is older than its sources; rebuild it with:\n    %s' % (binary, how))
    check ('self/compile', sources ('self', 'lib'), 'IRKENLIB=. self/compile self/compile.scm')
    check ('vm/irkvm', ['vm/irkvm.c', 'include/header1.c', 'include/gc1.c'], 'python3 util/build_vm.py')

def main ():
    ap = argparse.ArgumentParser (description='run irken tests')
    ap.add_argument ('paths', nargs='*', default=['test'])
    ap.add_argument ('-B', '--backends', help='comma-separated: c,b,arm,amd,rv (default: all this machine can run)')
    # (compiling is memory-hungry: too many at once and they get OOM-killed.)
    ap.add_argument ('-j', '--jobs', type=int, default=min (4, os.cpu_count() or 2))
    ap.add_argument ('-t', '--timeout', type=int, default=60, help='seconds to run a test (compiles get 4x)')
    ap.add_argument ('-k', '--keep', action='store_true', help='keep the scratch directory')
    ap.add_argument ('-v', '--verbose', action='store_true', help='list every result, not just problems')
    args = ap.parse_args()
    if not os.path.isdir ('self'):
        sys.exit ('run this from the top of the source tree')
    if not os.path.isfile ('self/compile'):
        sys.exit ('no self/compile, the compiler with all the backends.  build it with:\n'
                  '    IRKENLIB=. self/irken self/compile.scm\n'
                  '(or re-run util/bootstrap2.py, which now does that too)')
    warn_stale()
    backends = args.backends.split (',') if args.backends else host_backends()
    tests = [Test (p) for p in find_tests (args.paths)]
    scratch = tempfile.mkdtemp (prefix='irken-test-')
    jobs = [(t, b) for t in tests for b in backends]
    counts = {}
    problems = []
    try:
        with concurrent.futures.ThreadPoolExecutor (args.jobs) as ex:
            futures = {ex.submit (run_one, t, b, scratch, args): (t, b) for (t, b) in jobs}
            for f in concurrent.futures.as_completed (futures):
                t, b = futures[f]
                status, detail = f.result()
                counts[status] = counts.get (status, 0) + 1
                if status in ('fail', 'xpass') or (args.verbose and status != 'pass'):
                    problems.append ((t.path, b, status, detail))
                if args.verbose:
                    print ('%-5s %-4s %s' % (status, b, t.path), flush=True)
                else:
                    print ({'pass': '.', 'fail': 'F', 'skip': 's', 'xfail': 'x', 'xpass': 'X'}[status], end='', flush=True)
    finally:
        if args.keep:
            print ('\nscratch directory:', scratch)
        else:
            shutil.rmtree (scratch, ignore_errors=True)
    print()
    for path, b, status, detail in sorted (problems):
        print ('%s [%s] %s' % (status.upper(), b, path))
        if detail:
            print (detail)
    print ('%d tests x backends %s: %s' % (
        len (tests), ','.join (backends),
        ', '.join ('%d %s' % (counts[k], k) for k in ('pass', 'fail', 'xfail', 'xpass', 'skip') if k in counts)))
    sys.exit (1 if counts.get ('fail') or counts.get ('xpass') else 0)

if __name__ == '__main__':
    main()
