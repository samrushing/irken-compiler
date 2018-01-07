;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

;; to prepare:
;;    $ ffi/gen/genffi -gen ffi/sodium.ffi
;;    $ sudo cp ffi/sodium_ffi.scm /usr/local/lib/irken/ffi/
;;
;; to build:
;;    $ self/compile tests/t_sodium.scm -l sodium

(require-ffi 'sodium)

(define (string->hex s)
  (format
   (join
    (map
     (lambda (x) (format (zpad 2 (hex x))))
     (map char->int (string->list s))))))

(define (hash-string m)
  (let ((h (make-string 32))
        (h* (%c-cast uchar (%string->cref #f h)))
        (m* (%c-cast uchar (%string->cref #f m)))
        (len (string-length m)))
    (sodium/crypto_hash_sha256 h* m* len)
    (printf "input:  " (string m) "\n")
    (printf "sha256: " (string->hex h) "\n")
    ))

(hash-string "testing, testing!")
