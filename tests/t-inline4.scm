;; -*- Mode: Irken -*-

(define (print x)
  (%backend arm (%arm-c-call ("irk_dump_object" ('a -> undefined)) x))
  )


(define (newline)
  (%backend arm (%arm-c-call ("irk_putc" (char -> undefined)) #\newline))
  )

(define (printn x)
  (print x)
  (newline))

(datatype cmp
  (:<)
  (:=)
  (:>)
  )

(define (int-cmp a b)
  (%arm
   ((int int -> cmp)
    (mov (s 0) (i 22))
    (movk (s 0) (i 1) (lsl 16))
    (add (s 1) (s 0) (i 16) (lsl 12))
    (cmp (a 0) (a 1))
    (csel (s 0) (s 1) (s 0) "hi")
    (mov (s 1) (i 22))
    (csel trg (s 1) (s 0) "lo"))
   a b
   ))

(printn (int-cmp 4 5))
(printn (int-cmp 4 4))
(printn (int-cmp 5 4))
(printn (int-cmp 50 100))
(printn (int-cmp 75 75))
(printn (int-cmp 100 30))
(printn (cmp:<))
(printn (cmp:=))
(printn (cmp:>))



