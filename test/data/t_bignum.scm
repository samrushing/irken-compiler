;; -*- Mode: Irken -*-

;; bignum multiply and divide (mul2 and div2b1 are per-backend primitives).

(include "lib/basis.scm")
(include "lib/bignum.scm")

(define A (dec->big "123456789012345678901234567890123456789"))
(define B (dec->big "987654321098765432109876543210987654321"))
(define P (big-mul A B))
(printf (big->dec P) "\n")
(printf (big->dec (big-quo P B)) "\n")
(printf (big->dec (big-rem (big-add P big/7) A)) "\n")
(printf (big->dec (big-pow big/3 100)) "\n")
(printf (big->dec (big-exp-mod A B (dec->big "170141183460469231731687303715884105727"))) "\n")
(printf (big->dec (big-mul (big-negate A) B)) "\n")
;; => 121932631137021795226185032733866788594487120865336229233322374638011112635269
;; => 123456789012345678901234567890123456789
;; => 7
;; => 515377520732011331036461129765621272702107522001
;; => 78702211736378042618752783341628818082
;; => -121932631137021795226185032733866788594487120865336229233322374638011112635269
