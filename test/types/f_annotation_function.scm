;; -*- Mode: Irken -*-

;; a function whose type annotation disagrees with its body (#38), and that
;;  gets inlined, which used to lose the annotation (#24).

;; expect-error: f_annotation_function.scm:11:3: error: type error: this does

(include "lib/basis.scm")

(define (f x) : (int -> string)
  (+ x 1))
(printn (f 3))
