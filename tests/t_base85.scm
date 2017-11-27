;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/codecs/base85.scm")

(define sample "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.---") ;; note padding.

(for val (int32-enc (string-generator sample))
  (printf (hex val) " ")
  )
(printf "\n\n")

(for ch (int32-dec (int32-enc (string-generator sample)))
  (printf (char ch))
  )
(printf "\n\n")

(for ch (b85/32-enc (int32-enc (string-generator sample)))
  (printf (char ch))
  )
(printf "\n\n")

(for val (b85/32-dec (b85/32-enc (int32-enc (string-generator sample))))
  (printf (hex val) " ")
  )
(printf "\n\n")

(for ch (int32-dec (b85/32-dec (b85/32-enc (int32-enc (string-generator sample)))))
  (printf (char ch))
  )
(printf "\n\n")

(for ch (b85-enc (string-generator sample))
  (printf (char ch)))
(printf "\n\n")

(for ch (b85-dec (b85-enc (string-generator sample)))
  (printf (char ch)))
(printf "\n\n")
