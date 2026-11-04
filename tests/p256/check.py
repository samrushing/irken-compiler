# Compare include/p256 against the python `ecdsa` package (pip install ecdsa).
#
#   cc -O2 -I include/p256 -o /tmp/p256kat tests/p256/kat.c include/p256/p256_ec.c
#   python3 tests/p256/check.py /tmp/p256kat 500
#
# Checks the RFC 6979 A.2.5 vector, edge-case scalars and nonces, and
# random signatures and public keys.

import sys, random, subprocess
from ecdsa import NIST256p

G = NIST256p.generator
n = G.order()
exe = sys.argv[1]
N = int(sys.argv[2]) if len(sys.argv) > 2 else 200
rnd = random.Random(1234)

def h32 (x):
    return '%064x' % x

cases = []
# RFC 6979 A.2.5, SHA-256, message "sample"
cases.append ((
    'sign',
    0xC9AFA9D845BA75166B5C215767B1D6934E50C3DB36E89B127B8A622B120F6721,
    0xA6E3C57DD01ABE90086538398355DD4C3B17AA873382B0F24D6129493D8AAD60,
    0xaf2bdbe1aa9b6ec1e2ade1d694f41fc71a831d0268e9891562113d8a62add1bf,
))
edge = [1, 2, 3, n-1, n-2, n-3, 1 << 255]
for i in range (N):
    sk = rnd.randrange (1, n)
    k = rnd.randrange (1, n)
    h = rnd.randrange (0, 1 << 256)
    if i < len(edge):
        k = edge[i]
    elif i < 2 * len(edge):
        sk = edge[i - len(edge)]
    cases.append (('sign', sk, k, h))
for i in range (N):
    cases.append (('pub', edge[i] if i < len(edge) else rnd.randrange (1, n)))

def fmt (c):
    if c[0] == 'sign':
        return 'sign %s %s %s\n' % (h32(c[1]), h32(c[2]), h32(c[3]))
    else:
        return 'pub %s\n' % (h32(c[1]),)

inp = ''.join (fmt (c) for c in cases)
out = subprocess.run ([exe], input=inp, capture_output=True, text=True).stdout.split ('\n')
bad = 0
for c, line in zip (cases, out):
    if c[0] == 'sign':
        _, sk, k, h = c
        R = G * k
        r = R.x() % n
        s = pow (k, -1, n) * (h + r * sk) % n
        exp = '%s %s' % (h32(r), h32(s))
    else:
        P = G * c[1]
        exp = '%s %s' % (h32(P.x()), h32(P.y()))
    if line != exp:
        bad += 1
        if bad <= 5:
            print ('MISMATCH', c[0], [hex(x) for x in c[1:]], '\n got', line, '\n exp', exp)
print ('%d cases, %d mismatches' % (len(cases), bad))
sys.exit (1 if bad else 0)
