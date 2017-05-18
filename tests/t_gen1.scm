;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/os.scm")

(define (range-generator start stop)
  (make-generator
   (lambda (consumer)
     (let loop ((n start))
       (if (= n stop)
           (forever (consumer (maybe:no)))
           (begin
             (consumer (maybe:yes n))
             (loop (+ n 1))))))))

(define (char-generator)
  (make-generator
   (lambda (consumer)
     (let loop ((n 65))
       (if (= n (+ 65 26))
           (forever (consumer (maybe:no)))
           (begin
             (consumer (maybe:yes (ascii->char n)))
             (loop (+ n 1))))))))

(for (range-generator 100 120) i
  (printf "i = " (int i) "\n"))

(for (char-generator) ch
  (printf "ch = " (char ch) "\n"))
