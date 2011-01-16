(define (- a b)
  (%%cexp (int int -> int) "%0-%1" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%0==%1" a b))

(define (zero? x)
  (%%cexp (int -> bool) "%0==0" x))

;; variant with zero-arg loop

(let ((n 1000000))
  (let loop ()
    (if (zero? n)
	"done"
	(let ((n0 (- n 1)))
	  (set! n n0)
	  (loop)))))
