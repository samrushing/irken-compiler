;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (eq? a b)
  (%backend arm
    (%arm
     (('a 'a -> bool)
      (com "load #f")
      (mov (s 0) (i 6)) ;; 0x0006 (#f)
      (com "load #t")
      (mov (s 1) (i 6))
      (movk (s 1) (i 1) (lsl 16)) ;; 0x10006 (#t)
      (com "compare")
      (cmp (a 0) (a 1))
      (com "select")
      (csel trg (s 1) (s 0) "eq"))
     a b))
  (%backend amd
    (%amd
     (('a 'a -> bool)
      (com "eq?")
      (zero (s 0))
      (cmpq (a 0) (a 1))
      (sete (B (s 0)))
      (shlq (i 16) (s 0))
      (orq (i 6) (s 0))
      (movq (s 0) trg))
     a b
     ))
  (%backend rv
    (%rv
     (('a 'a -> bool)
      (beq (a 0) (a 1) "@0")
      (li  trg (i 6))
      (j "@1")
      (label "@0")
      (lui trg (i 16))
      (addiw trg trg (i 6))
      (label "@1"))
     a b))
  )

(printn (eq? 3 4))
(printn (eq? 4 4))
(printn (eq? #\A #\B))
(printn (eq? #\A #\A))
(printn (eq? #\C #\A))


