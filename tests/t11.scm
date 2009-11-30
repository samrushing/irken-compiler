;; give a good workout to variant case, including don't-care binding, inlining, etc..

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (glort0 a)
  (vcase a
     ((:blurb) 20)
     ((:blort a b c) (+ a (+ b c)))
     ((:urk) 666)))
    
(define (glort1 a)
  (vcase a
     ((:blurb) 20)
     ((:blort _ _ _ ) 42)
     ((:urk) 666)))

(let ((x (:blurb))
      (y (:blort 12 13 14))
      (z (:urk))
      )
  (printn (glort0 x))
  (printn (glort0 y))
  (printn (glort0 z))
  (printn (glort1 x))
  (printn (glort1 y))
  (printn (glort1 z))
  )
