
;; typechecks, of course!

(define (make-vector n)
  (%make-tuple #x14 n))

(define (vector-ref v n)
  (cexp "((pxll_vector*)(%s))->val[unbox(%s)]" v n))

(define (vector-set! v n x)
  (cexp "((pxll_vector*)(%s))->val[unbox(%s)] = %s" v n x))

(define (vector-length v)
  (cexp "get_tuple_size(%s)" v))

(define (test-vector n)
  (let ((v (make-vector n)))
    (let loop ((i 0))
      (if (%== i n)
	  v
	  (begin
	    (vector-set! v i (%* i i))
	    (loop (%+ i 1)))))))

(test-vector 10)
