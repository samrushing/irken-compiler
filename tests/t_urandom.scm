;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/urandom.scm")
(include "lib/codecs/hex.scm")

(define RNG (urandom-make))

(printf "32 random bytes: " (string->hex (RNG 32)) "\n")
