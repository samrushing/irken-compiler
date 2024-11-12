;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (rdtsc)
  (%amd
   ((-> int)
    (rdtsc)
    (shlq (i 32) (r rdx))
    (orq (r rax) (r rdx))
    (movq (r rdx) trg)
    (tag trg))))

(printn (rdtsc))
(printn (rdtsc))
(printn (rdtsc))
(printn (rdtsc))
(printn (rdtsc))


