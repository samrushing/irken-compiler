;; -*- Mode: Irken -*-

(define s "howdy, citizens!")

(define (string-ref s n)
  (%backend amd
    (%amd
     ((string int -> char)
      (sarq (a 1))
      (movzbl (ind 12 (a 0) (a 1) 1) (W (s 0)))
      (shlq (i 16) (s 0))
      (orq (i 2) (s 0))
      (movq (s 0) trg))
     s n
     )))

(define (string-set! s n v)
  (%amd
   ((string int char -> undefined)
    (sarq (a 1))
    (shrq (i 16) (a 2))
    (movb (B (a 2)) (ind 12 (a 0) (a 1) 1))
    )
   s n v
   ))

(let ((ch (string-ref s 4)))
  (string-set! s 5 #\!)
  )


