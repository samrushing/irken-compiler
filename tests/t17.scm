
(define (random)
  (%%cexp (-> int) "random()"))

;; hmmm... think about defining a *boxed* (rather than tagged) type that will
;;   hold a pointer.  [or maybe better... consider switching to untagged ints
;;   and having something like a 'stack map' for the gc that knows the type
;;   of everything on the stack]

(define (malloc n)
  (%%cexp (int -> int) "(pxll_int)malloc(%s)" n))

(define (free n)
  (%%cexp (int -> undefined) "free(%s); PXLL_UNDEFINED" n))

(define (write-int p n)
  (%%cexp (int int -> undefined) "(*(pxll_int *)(%s)) = %s" p n))

(define (read-int p)
  (%%cexp (int -> int) "(*(pxll_int *)(%s))" p))

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (sizeof-int)
  (%%cexp (-> int) "sizeof(pxll_int)"))

(let ((x 3)
      (y (malloc 16)))
  (set! x (random))
  (printn y)
  (write-int y 3141)
  (printn (read-int y))
  (free y)
  (printn (sizeof-int))
  #t
  )

