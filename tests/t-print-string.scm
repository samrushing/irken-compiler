;; -*- Mode: Irken -*-

;; note: discards return value.
(define (print-string s)
  (%arm-c-call ("irk_write_stdout" (string int -> int) #f) s (string-length s))
  #u
  )

(define (string-length s)
  (%arm
   ((string -> int)
    (ldrsw trg (ind (a 0) (i 8))) ;; trg = irk_string.len
    (lsl trg trg (i 1))           ;; trg <<= 1
    (orr trg trg (i 1)))          ;; trg |= 1
   s))

(define (print x)
  (%arm-c-call ("irk_dump_object" ('a -> undefined)) x)
  )

(define (newline)
  (%arm-c-call ("irk_putc" (char -> undefined)) #\newline)
  )

(define (printn x)
  (print x)
  (newline))

;;(printn (string-length "Howdy, Worldo!\n"))
(print-string "Howdy, Worldo!\n")

