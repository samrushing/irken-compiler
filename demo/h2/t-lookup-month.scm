;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(define lookup-month
  (let ((months #("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")))
    (lambda (month)
      (let loop ((i 0)
                 (m (downcase month)))
        (if (= i 12)
            (maybe:no)
            (if (string=? months[i] m)
                (maybe:yes (+ i 1))
                (loop (+ i 1) m)))))
    ))

(define (test m)
  (printf (string m) " => " (int (maybe (lookup-month m) -1)) "\n")
  )

(for-list mon (list "jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
  (test mon))

(test "SKLWD")
(test "")
(test "Nov")
(test "NOV")
