// -*- Mode: C -*-
//
// Known-answer driver for include/p256.  Reads lines from stdin:
//   sign <sk> <k> <h>   -> prints "<r> <s>"
//   pub <sk>            -> prints "<x> <y>"
// all values are 32-byte big-endian hex.  See check.py.
//
// build: cc -O2 -I include/p256 -o /tmp/p256kat tests/p256/kat.c include/p256/p256_ec.c

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "p256_ec.h"

static void unhex (uint8_t *out, const char *s) {
  for (int i = 0; i < 32; i++) { unsigned v; sscanf (s + 2 * i, "%2x", &v); out[i] = v; }
}

static void phex (const uint8_t *b) {
  for (int i = 0; i < 32; i++) printf ("%02x", b[i]);
}

int main (void) {
  char a[80], b[80], c[80], op[8];
  while (scanf ("%7s", op) == 1) {
    if (!strcmp (op, "sign")) {
      scanf ("%79s %79s %79s", a, b, c);
      uint8_t sk[32], k[32], h[32], r[32], s[32];
      unhex (sk, a); unhex (k, b); unhex (h, c);
      p256_ecdsa_sign (r, s, sk, k, h);
      phex (r); printf (" "); phex (s); printf ("\n");
    } else if (!strcmp (op, "pub")) {
      scanf ("%79s", a);
      uint8_t sk[32], x[32], y[32];
      unhex (sk, a);
      p256_public_key (x, y, sk);
      phex (x); printf (" "); phex (y); printf ("\n");
    }
  }
  return 0;
}
