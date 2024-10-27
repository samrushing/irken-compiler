;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/codecs/hex.scm")
(require "doom/http/hpack2.scm")

(define encoded
  (list
    "885f87497ca589d34d1f628fe4c7f21742f3e275a6db5d97021fe76496df3dbf4a05b521b665040134a085702ddc65953168df"
    "885f8b352398ac23bcdfefcd347f6291e4c7f216c4ebef01c75a5c4eb2fb2eff3fc0"
    "88bf6291e4c7f216c4ebef34fb4f2ed3e07da7bf9fc1"
    "88c06291e4c7f216c4ebec8590b4bbcebc06dffe7fc2"
   ))

(define (print-headers h)
  (define print-pair
    (:tuple name val)
    -> (printf "  " name ": " val "\n")
    )
  (printf "headers {\n")
  (for-each print-pair h)
  (printf "}\n"))

(define (t0 encoded)
  (let ((decoder (hpack/decoder)))
    (for-list enc encoded
      (printf "--------------------------------------------\n")
      (print-headers (decoder (hex->string enc))))))

(t0 encoded)

