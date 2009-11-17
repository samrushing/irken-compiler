;; test vectors and argc/argv

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define (make-vector n x)
  (%make-vector n x))

(define (t0)

  (define (argv n)
    (let* ((len (%%cexp (int -> int) "strlen(argv[%s])" n))
	   (r (make-string len)))
      (%%cexp
       (string int int -> undefined)
       "(memcpy (%s, argv[%s], %s), PXLL_UNDEFINED)"
       r n len)
      r))

  (let ((v (make-vector (sys:argc) "")))
    (let loop ((n (sys:argc)))
      (cond ((zero? n) v)
	    (else
	     (set! v[(- n 1)] (argv (- n 1)))
	     (loop (- n 1)))))))

;; dump argv as a vector of strings
(t0)
