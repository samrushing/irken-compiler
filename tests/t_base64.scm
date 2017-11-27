;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/codecs/base64.scm")

(define t0 "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

(for ch (b64-dec (b64-enc (string-generator t0)))
  (printf (char ch)))
(printf "\n")

(printf (string (b64-encode t0)) "\n")
(printf (string (b64-decode (b64-encode t0))) "\n")
(printf (string (b64-encode "fnord01")) "\n")
(printf (string (b64-decode "Zm5vcmQwMQ==")) "\n")
(printf (string (b64-decode "Zm5vcmQwMQ")) "\n")
(printf (base64 t0) "\n")

