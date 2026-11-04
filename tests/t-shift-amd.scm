;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (x<< a b)
  (%backend amd ;; shift has to be in %CL
    (%amd
     ((int int -> int)
      (untag (a 1))
      (untag (a 0))
      (movq (a 1) (r rcx))
      (salq (B (r rcx)) (a 0))
      (movq (a 0) trg)
      (tag trg))
     a b))
  )

(define (x>> a b)
  (%backend amd ;; shift has to be in %CL
    (%amd
     ((int int -> int)
      (untag (a 1))
      (untag (a 0))
      (movq (a 1) (r rcx))
      (sarq (B (r rcx)) (a 0))
      (movq (a 0) trg)
      (tag trg))
     a b))
  )

(define (~ a)
  (%backend amd
    (%amd
     ((int -> int)
      (untag (a 0))
      (movq (a 0) (s 0))
      (notq (s 0))
      (movq (s 0) trg)
      (tag trg))
     a)))

(printn (x<< 3141 3))
(printn (x>> 314159 7))
(printn (~ 314159))

