
(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (< a b)
  (%%cexp (int int -> bool) "%s<%s" a b))

(define (string-length s)
  (%%cexp ((raw string) -> int) "%s->len" s))

(define (string-compare a b)
  (define (min x y)
    (if (< x y) x y))
  (let* ((smin (min (string-length a) (string-length b)))
	 (cmp (%%cexp (string string int -> int) "memcmp (%s, %s, %s)" a b smin)))
    (cond ((= cmp 0)
	   (if (= (string-length a) (string-length b))
	       0
	       (if (< (string-length a) (string-length b)) -1 1)))
	  (else cmp))))

(define (thing1 s)
  (let ((x "howdy")
	(y "there"))
    (string-compare x y)))
	
(thing1 "blurble")
