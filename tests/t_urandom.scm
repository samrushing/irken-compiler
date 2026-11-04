;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/urandom.scm")
(require "lib/codecs/hex.scm")

(define RNG (urandom-make))

(printf "32 random bytes: " (string->hex (RNG 32)) "\n")
