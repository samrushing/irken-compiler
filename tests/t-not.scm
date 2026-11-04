;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

;; NOT is a pseudo-instruction for xori rd, rs, -1
;; so this isn't really the best approach on RV64.
(define (not a)
  (%backend rv
    (%rv
     ((int -> int)
      (untag (a 0))
      (not trg (a 0))
      (tag trg))
     a
     )))

(printn (not 314159))
(printn (not 1))
(printn (not -1))
(printn (not 0))
(printn (not #xffffffffffff))

