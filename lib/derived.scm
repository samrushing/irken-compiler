;; -*- Mode: Irken -*-

;; derived expressions

(defmacro and
  (and)                 -> #t
  (and test)            -> test
  (and test1 test2 ...) -> (if test1 (and test2 ...) #f)
  )

;; *not* the same as a Scheme <or>, this returns a boolean.
(defmacro or
  (or) -> #f
  (or test) -> test
  (or test1 test2 ...) -> (if test1 #t (or test2 ...))
  )

(defmacro let
  ;; normal <let> here, we just rename it to our core
  ;; binding construct, <let_splat>
  (let ((name val) ...) body1 body2 ...)
  -> (let_splat ((name val) ...) body1 body2 ...)

  ;; named let
  (let tag ((name val) ...) body1 body2 ...)
  -> (letrec ((tag (lambda (name ...) body1 body2 ...)))
       (tag val ...))
  )

