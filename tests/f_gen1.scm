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

(for (range-generator 100 120) i
  (printf "i = " (char i) "\n"))
