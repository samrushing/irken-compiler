;; -*- Mode: Irken -*-


(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define thing #(0 1 2 3 4 5))

(printn thing)
(printn thing[3])
(set! thing[3] 99)
(printn thing)
(printn thing[3])
