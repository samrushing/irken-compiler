# P-256 ECDSA signing and ECDH

DRAFT: motivation and design notes, to be edited.

This directory holds a small, self-contained implementation of ECDSA
signing on the NIST P-256 curve (secp256r1), computing the public key for
a private key, and ECDH.  There is no verification, no other curves, and
no hashing; the caller hashes the message (with libsodium's SHA-256) and
supplies the nonce.

ECDH came later, for TLS clients: PostgreSQL before version 18 offers
only P-256 for key exchange, so a client that does only X25519 can't
reach it over TLS.  It reuses the same ladder, and checks every peer
point (see below).

## Why this exists

Irken's crypto policy is to do as much as possible with one vetted
library, libsodium, and to avoid OpenSSL and its relatives entirely.
They are enormous, have a long history of serious bugs, and pulling one in
would make it the largest and least-understood dependency of any program
that uses it.

libsodium covers almost everything a minimal TLS 1.3 server needs:
X25519 for key exchange, ChaCha20-Poly1305 and AES-GCM for records,
SHA-256 / HMAC / HKDF for the key schedule, and Ed25519 for signatures.

The gap is the certificate.  The CA/Browser Forum still does not allow
Ed25519 certificates, so a server that browsers will talk to (and the
reason for wanting TLS at all here is to serve HTTP/2, which browsers only
speak over TLS) needs an RSA or ECDSA certificate, and has to sign the
handshake with that key.  ECDSA over P-256 is the smallest and simplest of
those choices, and signing is the only operation the server needs.

So this is the *one* piece of public-key crypto that Irken carries itself.

## Design

* **Field arithmetic is generated, not written.**  `p256_64.c` (arithmetic
  mod p) and `n256_64.c` (arithmetic mod the group order n) are generated
  by [fiat-crypto](https://github.com/mit-plv/fiat-crypto)'s
  `word_by_word_montgomery`, which produces C from formally verified
  Coq code.  The exact invocation is in the comment at the top of each
  file.  Elements are 4x64-bit limbs in the Montgomery domain.  These
  files are included directly by `p256_ec.c`, not compiled separately.

* **Homogeneous (projective) coordinates with complete formulas.**
  Points are (X : Y : Z).  Addition and doubling use the Renes, Costello,
  Batina (2015) "Complete addition formulas for prime order elliptic
  curves" (`add_2015_rcb`, `dbl_2015_rcb`).  "Complete" means the same
  formula is correct for every input, including the point at infinity and
  P + P, so there are no special cases, and therefore no branches on
  secret data.

* **Constant-time Montgomery ladder.**  `p256_scalarmult` walks all 256
  bits of the scalar, most significant first, and does the same
  cswap / add / double / cswap for every bit.  The swap is done with an
  XOR mask, with a compiler barrier on the mask so the compiler can't
  turn it back into a branch.  Leading zero bits need no special
  handling: the ladder starts at infinity, and the complete formulas
  handle it.  The scalar (a private key or a nonce) never affects control
  flow or memory addresses.

  This is deliberately simpler (and slower) than the usual approach in
  libraries like BoringSSL, which use Jacobian coordinates and
  precomputed tables of multiples of G with constant-time table lookups.
  A TLS server signs once per handshake, so simplicity wins.

* **Inversion by Bernstein-Yang divsteps.**  Converting back to affine
  coordinates, and computing k^-1 mod n, use the constant-time
  `divstep` that fiat-crypto also generates, following
  "High-assurance field inversion for curve-based cryptography"
  (https://eprint.iacr.org/2021/549.pdf) and fiat-crypto's
  `inversion/c/montgomery_template.c`.

* **Interface.**  Everything is 32-byte big-endian strings:

  ```c
  void p256_ecdsa_sign (uint8_t out_r[32], uint8_t out_s[32],
                        const uint8_t sk[32], const uint8_t k[32], const uint8_t h[32]);
  void p256_public_key (uint8_t out_x[32], uint8_t out_y[32], const uint8_t sk[32]);
  int  p256_ecdh (uint8_t out_x[32], const uint8_t sk[32],
                  const uint8_t px[32], const uint8_t py[32]);
  ```

  The caller is responsible for choosing `sk` and `k` uniformly in
  [1, n-1] (or deriving `k` with RFC 6979), and for never reusing a nonce.
  `p256_ecdsa_sign` does not check for the (astronomically unlikely)
  r = 0 or s = 0 cases.

  `p256_ecdh` returns -1, and computes nothing, unless (px, py) is a
  point on the curve: both coordinates below p, and y^2 = x^3 + ax + b.
  Without that check a peer could send a point that isn't on the curve:
  the ladder would still compute something, but not in P-256's
  prime-order group, and invalid-point attacks use such results to learn
  sk a few bits at a time.  The point is public, so the check may branch.

## Using it from Irken

`ffi/p256_ecdsa.ffi` points at these files via `$IRKENLIB`, so a program
that does `(require-ffi 'p256_ecdsa)` compiles and links `p256_ec.c`
directly.  `lib/crypto/p256.scm` wraps it: hashing, nonce generation, and
DER encoding of the signature; and `p256-ecdh`, which takes the peer's
key as TLS sends it (04 || x || y).

The generated interface, `ffi/p256_ecdsa_ffi.scm`, isn't in the
repository: the bootstrap generates it (`util/gen_ffis.py`).

## Testing

`tests/p256/` has a small C driver and a Python script that checks
signatures and public keys against the `ecdsa` package, including the
RFC 6979 A.2.5 test vector and edge-case scalars (1, 2, n-1, ...):

    cc -O2 -I include/p256 -o /tmp/p256kat tests/p256/kat.c include/p256/p256_ec.c
    python3 tests/p256/check.py /tmp/p256kat 500

ECDH is checked by `test/crypto/t_p256_ecdh.scm` (the RFC 5903 8.1
vector, both ways, and points that must be refused).

## License

The fiat-crypto generated files carry fiat-crypto's license (MIT /
Apache-2.0 / BSD-1-Clause, at your option).
