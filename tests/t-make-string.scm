;; -*- Mode: Irken -*-

(require "lib/core.scm")
(define (make-string n)
  ;;(%ensure-heap #f (string-tuple-length n))
  (%arm-c-call ("arm_make_string" (int -> string)) n)
  )

(let ((x "blah"))
  (make-string 17))

