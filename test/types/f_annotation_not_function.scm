;; -*- Mode: Irken -*-

;; a function annotated with a type that is not a function type (#38).

;; expect-error: f_annotation_not_function.scm:10:28: error: the type annotation of `work_on_file` should be a function type

(include "lib/basis.scm")

(typealias file_descriptor int)
(define (work_on_file f) : (file_descriptor)  ;; should be (file_descriptor -> int)
   0 )
(work_on_file 1)
