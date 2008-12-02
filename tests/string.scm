
(define (make-string n)
  (let ((s (%make-tuple #x10 (cexp "box(string_tuple_length(unbox(%s)))" n))))
    (%set-string-len s n)
    s))

(define (make-char n)
  (%make-immediate n #x02))

(define (string-ref s n)
  (make-char (cexp "box(((pxll_string *)%s)->data[unbox(%s)])" s n)))

(define (string-set! s n c)
  (cexp "((pxll_string *)%s)->data[unbox(%s)] = GET_PAYLOAD (%s)" s n c))

(let ((s1 (make-string 10))
      (s2 "howdy")
      (c0 #\X)
      )
  (%printn s1)
  (%printn s2)
  (%printn c0)
  (%printn (%string-length s1))
  (%printn (%string-length s2))
  (%printn (string-ref s2 0))
  (string-set! s2 1 #\A)
  (%printn s2)
  )
