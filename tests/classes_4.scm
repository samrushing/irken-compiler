
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (* x y)
  (%%cexp (int int -> int) "%s*%s" x y))

(define (string-length s)
  (%%cexp (string -> int) "((pxll_string*)(%s))->len" s))

(class thing (ob size:int))

(define (frob-thing t:thing n)
  ;; this should work with any <thing>
  (* t.size n))

(define (glorp-thing t:thing)
  ;; this will only work with <thing>s that have string <ob>s.
  (string-length t.ob))

(let ((o1 (thing "hello" 5))
      (o2 (thing 34 9))
      (o3 (thing #f 3))
      )
  (+ o1.size 3)
  (if o3.ob
      (printn "yes")
      (printn #f))
  (printn (frob-thing o1 2))
  (printn (glorp-thing o1))
  )
