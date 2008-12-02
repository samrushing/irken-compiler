(define (make-vector n)
  (%make-tuple #x14 n))

;; XXX range check!
(define (vector-set! v n x)
  (%%verify "TC_VECTOR" 1 v)
  (%%cexp "(((pxll_vector*)(%s))->val[unbox(%s)] = %s, PXLL_UNDEFINED)" v n x))

;; XXX range check!
(define (vector-ref v n)
  (%%verify "TC_VECTOR" 1 v)
  (%%cexp "((pxll_vector*)(%s))->val[unbox(%s)]" v n))

(define (vector . args)
  (let loop ((i (tuple-length args))
	     (v (make-vector (tuple-length args))))
    (cond ((< i 0)
	   v)
	  (else
	   (vector-set! v i (tuple-ref args i))
	   (loop (- i 1) v)))))
