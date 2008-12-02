
(define (make-tuple tag len)
  (%%cexp (int int -> ?) "allocate (%s, %s)" tag len))

(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(class point (x:int y flag:bool)

   (define (init self x y flag)
     (set! self.x x)
     (set! self.y y)
     (set! self.flag flag))

   (define (swap self)
     (let ((tmp self.x))
       (set! self.x self.y)
       (set! self.y tmp)))

   (define (dist self plus)
     (+ (+ self.x self.y) plus))

   )

(let ((p (make-point 3 4 #t))
      (zord 0))
  (if p.flag
      (+ p.y p.x)
      (- p.x p.y))
  (set! p.x 19)
  (set! zord 34)
  (p.swap)
  (p.dist 20)
  (printn p)
  )
