(define (make-vector n)
  (%make-tuple #x14 n))

(define (vector-set! v n x)
  (%%cexp "((pxll_vector*)(%s))->val[unbox(%s)] = %s" v n x))

(define (test)
  (let ((v (make-vector 6)))
    (vector-set! v 0 0)
    (vector-set! v 1 1)
    (vector-set! v 2 2)
    (vector-set! v 3 3)
    (vector-set! v 4 4)
    (vector-set! v 5 5)
    v))

(test)
