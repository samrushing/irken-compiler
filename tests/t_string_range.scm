;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(define tests-passed 0)

(defmacro assert2
  (assert2 exp)
  -> (if (not exp)
         (begin
           (printf "assertion failed: " (repr (car (%%sexp exp))) "\n")
           (raise (:AssertionFailed)))
         (set! tests-passed (+ tests-passed 1))))

(defmacro good
  (good exp)
  -> (assert2
      (try
       (begin exp #t)
       except
       (:String/Range _ _ _)
       -> #f
       )))

(defmacro bad
  (bad exp)
  -> (assert2
      (try
       (begin exp #f)
       except
       (:String/Range _ _ _)
       -> #t
       )))

(define (t0)
  (let ((s "0123456789"))
    (good (string-ref s 4))
    (good (string-ref s 0))
    (good (string-ref s 9))
    (bad  (string-ref s -3))
    (bad  (string-ref s 10))
    (bad  (string-ref s 10000))
    ))

(define (t1)
  (let ((s0 "0123456789")
        (s1 "abcdefghij"))
    (bad  (buffer-copy s1 0 11 s0 0))
    (good (buffer-copy s1 0 10 s0 0))
    (bad  (buffer-copy s1 0 10 s0 1))
    (good (buffer-copy s1 0  9 s0 1))
    (bad  (buffer-copy s1 20 5 s0 0))
    (good (buffer-copy s1 5  5 s0 5))
    ))

(t0)
(t1)
(printf "passed " (int tests-passed) " tests.\n")
