// -*- Mode: C -*-

#include <inttypes.h>

void p256_ecdsa_sign (uint8_t out_r[32], uint8_t out_s[32], const uint8_t sk[32], const uint8_t k[32], const uint8_t h[32]);
void p256_public_key (uint8_t out_x[32], uint8_t out_y[32], const uint8_t sk[32]);
