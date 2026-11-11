;; -*- Mode: Irken -*-

;; a typed let binding whose init has another type.

;; expect-error: f_annotation_let.scm:9:17: error: type error: this does

(include "lib/basis.scm")

(let ((x:string 3))
  (printn x))
