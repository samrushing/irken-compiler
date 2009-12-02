
(define (< a b)
  (%%cexp (int int -> bool) "%s<%s" a b))

(define (write-substring fd s start len)
  ;; XXX range check
  (%%cexp (int string int int -> int) "write (%s, %s+%s, %s)" fd s start len))

(define (file:flush self)
  (let loop ((start 0))
    (let ((n (write-substring self.fd self.buf start self.pos)))
      (if (< n self.pos)
	  (loop n)
	  #u))))

(let ((f {fd=1 buf="hello there" pos=0}))
  (file:flush f))
