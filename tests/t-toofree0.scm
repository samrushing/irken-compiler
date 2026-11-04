;; -*- Mode: Irken -*-


(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (+ a b)
 (%llarith add a b))

(let ((x 0))
  (printn x)
  (let ((y 1))
    (printn y)
    (let ((z 2))
      (printn y)
      (let ((a 1))
        (printn y)
        (let ((b 2))
          (printn y)
          (+ z (+ y x)))))))


