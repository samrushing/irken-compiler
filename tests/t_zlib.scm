;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/codecs/zlib.scm")
(require "lib/codecs/hex.scm")
(require "lib/crypto/sha1.scm")

(define s0
  (list "The curious task"
        " of economics is"
        " to demonstrate "
        "to men how littl"
        "e they really kn"
        "ow about what th"
        "ey imagine they "
        "can design."))

(define s1
  (map
   hex->string
   (list "789c2dccd10d80200c04d0556e02a771818a151aa14d6889717b31fa79ef2eb7"
         "16461a5d6c3882fc841de0646a4d9243261a766ea61e9d82dfd85851ec429588"
         "3aa5f08dce54eb8d53a7d3662370158aaf934659f41f26d2f9e7927579000c82"
         "2d71")))

(define (t0)
  (let ((s1 (string-concat
             (generator->list
              (inflate (deflate (list-generator s0) 1024 Z_BEST_COMPRESSION) 1024)
              ))))
    (printf (string s1) "\n")
    (assert (string=? (string-concat s0) s1))))

(define (t1)
  (let ((parts '()))
    (for chunk (inflate (list-generator s1) 1024)
      (push! parts chunk))
    (assert
     (string=? (string-concat (reverse parts))
               (string-concat s0)))))

(define (t2)
  (let ((f0 (make-file-generator (file/open-read "self/compile")))
        (f1 (make-file-generator (file/open-read "self/compile")))
        (h0 (sha1 f0))
        (h1 (sha1 (inflate (deflate f1 16384 Z_BEST_SPEED) 16384))))
    (assert (string=? h0 h1))
    (printf (string->hex h0) "\n")
    (printf (string->hex h1) "\n")
    ))

(t0)
(t1)
(t2)

