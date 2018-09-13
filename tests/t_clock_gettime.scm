;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/time.scm")

(define (test kind)
  (let ((ts (clock-gettime kind)))
    (printf "nsec " (int ts.nsec) "\n")
    (printf "current time: " (int ts.sec) "." (zpad 9 (int ts.nsec)) "\n")
    ))

(test CLOCK_REALTIME)
(test CLOCK_MONOTONIC)
(test CLOCK_PROCESS_CPUTIME_ID)
