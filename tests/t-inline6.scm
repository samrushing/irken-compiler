;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (int->char n)
  (%backend arm
    (%arm
     ((int -> char)
      (lsr (s 0) (a 0) (i 1))
      (mov trg (i 2))
      (bfi trg (s 0) (i 16) (i 48)))
     n))
  (%backend amd
    (%amd
     ((int -> char)
      (andq (i -2) (a 0))
      (shlq (i 15) (a 0))
      (leaq (ind 2 (a 0)) trg)
      )
     n))
  (%backend rv
    (%rv
     ((int -> char)
      (srli (a 0) (a 0) (i 1))
      (slli (a 0) (a 0) (i 16))
      (ori  trg (a 0) (i 2))
      )
     n))
  )

(define (char->int ch)
  (%backend arm
    (%arm
     ((char -> int)
      (lsr (s 0) (a 0) (i 15))
      (orr trg (s 0) (i 1)))
     ch
     ))
  (%backend amd
    (%amd
     ((char -> int)
      (movq (a 0) trg)
      (shrq (i 15) trg)
      (orq (i 1) trg))
     ch
     ))
  (%backend rv
    (%rv
     ((char -> int)
     (srli (a 0) (a 0) (i 15))
     (ori  trg (a 0) (i 1)))
     ch))
  )

(define (string-length s)
  (%backend arm
    (%arm
     ((string -> int)
      (ldrsw trg (ind (a 0) (i 8))) ;; trg = irk_string.len
      (lsl trg trg (i 1))           ;; trg <<= 1
      (orr trg trg (i 1)))          ;; trg |= 1
     s)
    )
  (%backend amd
    (%amd
     ((string -> int)
      (movl (ind 8 (a 0)) (W trg)) ;; grab 32-bit size
      (movslq (W trg) trg)         ;; sign extend
      (tag trg))                   ;; tag
     s)
    )
  (%backend rv
    (%rv
     ((string -> int)
      (lw trg (ind 8 (a 0)))
      (tag trg))
     s))
  )

(printn (int->char 65))
(printn (char->int #\z))
(printn (int->char 122))
(printn (string-length "testing"))

