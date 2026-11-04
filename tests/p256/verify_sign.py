# check the output of tests/t_p256_sign against python's ecdsa package.
#   tests/t_p256_sign | python3 tests/p256/verify_sign.py
import sys, hashlib
from ecdsa import NIST256p, SigningKey, VerifyingKey
from ecdsa.util import sigdecode_der

expect = [False, True, True, False, False, False, True]
oks = []
sk = pub = None
nsig = bad = 0
for line in sys.stdin:
    parts = line.split()
    if not parts:
        continue
    if parts[0] == 'ok?':
        oks.append(parts[2] == '#t')
    elif parts[0] == 'sk':
        sk = SigningKey.from_string(bytes.fromhex(parts[1]), curve=NIST256p)
    elif parts[0] == 'pub':
        pub = bytes.fromhex(parts[1])
        assert pub == b'\x04' + sk.get_verifying_key().to_string(), 'public key mismatch'
        vk = VerifyingKey.from_string(pub, curve=NIST256p)
    elif parts[0] == 'sig':
        m, sig = bytes.fromhex(parts[1]), bytes.fromhex(parts[2])
        nsig += 1
        try:
            # sigdecode_der rejects non-minimal INTEGER encodings.
            vk.verify(sig, m, hashfunc=hashlib.sha256, sigdecode=sigdecode_der)
        except Exception as e:
            bad += 1
            if bad <= 5:
                print('BAD', parts[1], parts[2], repr(e))
assert oks == expect, (oks, expect)
print('range checks ok; public key ok; %d signatures, %d bad' % (nsig, bad))
sys.exit(1 if bad else 0)
