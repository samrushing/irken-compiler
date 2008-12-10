
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (- x y)
  (%%cexp (int int -> int) "%s-%s" x y))

(define (= x y)
  (%%cexp (int int -> bool) "%s==%s" x y))

(define (<< x y)
  (%%cexp (int int -> int) "%s<<%s" x y))

(let ((o1 #(1 2 3))
      (o2 #("hello" "there"))
      (o3 #(+ - <<))
      )
  (printn o1)
  (printn o2)
  (printn o3)
  (printn (o3[0] 3 4))
  (printn (o3[2] 2 (o3[1] 6 o1[1])))
  (set! o3[1] o3[0])
  (printn (o3[2] 2 (o3[1] 6 o1[1])))
  ;; this will trigger a type error
  ;;(set! o3[0] =)
  )
