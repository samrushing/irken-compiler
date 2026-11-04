;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/crypto/sodium.scm")
(require "lib/codecs/hex.scm")

;; 4712153bb4b3d22da8921160b10691282de9b26a4a97598f3c735d126de28b5d
;; 38e9939f5fa259b474172924db12efddb2e78e9e1363587440b03d355123ea29

;; wrong curve!
(define (test-c25519-scalarmult)
  (let ((skey (hex->string "38e9939f5fa259b474172924db12efddb2e78e9e1363587440b03d355123ea29"))
        (pkey (curve25519-scalarmult-base skey)))
    (printf "scalarmult:\n")
    (printf "pkey " (string->hex pkey) "\n")
    ))

(define (test-ed25519-keygen)
  (let ((seed (hex->string "4712153bb4b3d22da8921160b10691282de9b26a4a97598f3c735d126de28b5d"))
        ((sk pk) (ed25519-seed-to-keypair seed)))
    (printf "keygen:\n")
    (printf "seed " (string->hex seed) "\n"
            "sk " (string->hex sk) "\n"
            "pk " (string->hex pk) "\n")
    ))

;; ok, this is confusing compared to normal ECC
;; normally, the sk is the secret scalar, and pk is either x,y or x from sk*G.
;; ed25519 is different.  the 'sk' contains both parts, so THIS function does nothing
;;  more than give you the second half.
;; it seems like sk and seed are the same thing, but I'm not sure.  Probably the bit-twiddling?
;;
;; part of this is that ed25519/curve25519 doesn't really truck with y coordinates.
(define (test-ed25519-sk-to-pk)
  (let ((sk (hex->string "4712153bb4b3d22da8921160b10691282de9b26a4a97598f3c735d126de28b5d38e9939f5fa259b474172924db12efddb2e78e9e1363587440b03d355123ea29"))
        (pk (ed25519-sk-to-pk sk)))
    (printf "sk_to_pk:\n")
    (printf "sk " (string->hex sk) "\n")
    (printf "pk " (string->hex pk) "\n")
    ))

(test-c25519-scalarmult)
(test-ed25519-keygen)
(test-ed25519-sk-to-pk)
