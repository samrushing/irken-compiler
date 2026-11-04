;; -*- Mode: Irken -*-

(define (print x)
  (%asm-c-call ("irk_dump_object" ('a -> undefined)) x)
  )


(define (newline)
  (%asm-c-call ("irk_putc" (char -> undefined)) #\newline)
  )

(define (printn x)
  (print x)
  (newline))

(datatype cmp
  (:<)
  (:=)
  (:>)
  )

(%backend amd
  (define (int-cmp a b)
    (%amd
     ((int int -> cmp)
      (zero (s 0))
      (cmpq (a 0) (a 1))
      (setb (B (s 0)))
      (shlq (i 16) (s 0))
      (addq (i 65558) (s 0))
      (cmpq (a 1) (a 0))
      (movq (i 22) (s 1))
      (cmovaeq (s 0) (s 1))
      (movq (s 1) trg))
     a b
     )))

(printn (int-cmp 4 5))
(printn (int-cmp 4 4))
(printn (int-cmp 5 4))
(printn (int-cmp 50 100))
(printn (int-cmp 75 75))
(printn (int-cmp 100 30))
(printn (cmp:<))
(printn (cmp:=))
(printn (cmp:>))



