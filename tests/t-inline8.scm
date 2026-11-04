;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (get-argv)
  (%backend amd
    (%asm-c-call ("irk_make_argv" (-> (vector string))))))

(printn (get-argv))

