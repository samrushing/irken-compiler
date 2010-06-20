(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (zero? x)
  (%%cexp (int -> bool) "%s==0" x))

;; variant with zero-arg loop

(let ((n 1000000))
  (let loop ()
    (if (zero? n)
	"done"
	(let ((n0 (- n 1)))
	  (set! n n0)
	  (loop)))))
