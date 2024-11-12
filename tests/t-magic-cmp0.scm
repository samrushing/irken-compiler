;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))

(define (printn x)
  (print x)
  (newline))

(datatype cmp
  (:<)
  (:=)
  (:>)
  )

(define (magic-cmp a b)
  (%backend amd
    (%amd
     (('a 'a -> cmp)
      (movq (a 0) (c 0))
      (movq (a 1) (c 1))
      (callq "magic_cmp")
      (shlq (i 16) (r rax))
      (addq (i #x10016) (r rax))
      (movq (r rax) trg))
     a b
     ))
  (%backend rv
    (%rv
     (('a 'a -> cmp)
      (mv (c 0) (a 0))
      (mv (c 1) (a 1))
      (jal "magic_cmp")
      (slli (c 0) (c 0) (i 16))
      (lui (s 0) (i 16))
      (addiw (s 0) (s 0) (i 22))
      (add trg (c 0) (s 0)))
     a b
     )))

(printn (magic-cmp (:one 2 3) (:one 3 4)))
(printn (magic-cmp (:one 3 4) (:one 3 4)))
(printn (magic-cmp (:one 3 4) (:one 2 3)))




