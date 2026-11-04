;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline) #u)

(define (int-cmp a b)
  (%asm-c-call ("irk_int_cmp" (int int -> cmp) #f) a b))

(define (int-cmp2 a b)
  (%rv
   ((int int -> cmp)
    (bltu  (a 1) (a 0) "2")
    (lui   (s 0) (i 16))
    (addiw (s 1) (s 0) (i 22))
    (li    (s 0) (i 22))
    (bgeu  (a 0) (a 1) "3")
    (j "4")
    (label "2")
    (lui   (s 0) (i 32))
    (addiw (s 1) (s 0) (i 22))
    (li    (s 0) (i 22))
    (bltu  (a 0) (a 1) "4")
    (label "3")
    (mv    (s 0) (s 1))
    (label "4")
    (mv    trg (s 0)))
   a b))

(printn (int-cmp 7 10))
(printn (int-cmp 10 10))
(printn (int-cmp 10 7))

(printn (int-cmp2 7 10))
(printn (int-cmp2 10 10))
(printn (int-cmp2 10 7))
