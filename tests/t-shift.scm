;; -*- Mode: Irken -*-

(define (print x)
  (%backend arm (%arm-c-call ("irk_dump_object" ('a -> undefined)) x))
  )

(define (printn x)
  (print x)
  (newline))

(define (newline)
  (%backend arm (%arm-c-call ("irk_putc" (char -> undefined)) #\newline))
  )

(define (<< a b)
  (%backend arm (%llarith lsl a b))
  )

(define (>> a b)
  (%backend arm (%llarith asr a b))
  )

(printn (<< 3141 3))
(printn (>> 314159 7))

