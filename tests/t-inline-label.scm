;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (t0 x)
  (%rv
   ((int -> int)
    (li (s 0) (i 99))
    (beq (a 0) (s 0) "@0")
    (li trg (i 7))
    (j "@1")
    (label "@0")
    (li trg (i 9))
    (label "@1"))
   x
   ))

(printn (t0 88))
(printn (t0 99))
(printn (t0 49))

