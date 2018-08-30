;; -*- Mode: Irken -*-

(defmacro partial
  (partial (arg ...))
  -> (partial1 () (arg ...) ())
  )

;; scans for placeholders
(defmacro partial1
  (partial1 (formal ...) () (exp ...))
  -> (lambda (formal ...) (exp ...))
  (partial1 (formal ...) (<_> args ...) (exp ...))
  -> (partial1 (formal ... $x) (args ...) (exp ... $x))
  (partial1 (formal ...) (arg args ...) (exp ...))
  -> (partial1 (formal ...) (args ...) (exp ... arg))
  )

;; (partial (somefun _ 'arg _))
;; => (lambda ($x0 $x1) (somefun $x0 'arg $x1))
;; (map (partial (- _ 3)) (range 10))
;; => (-3 -2 -1 0 1 2 3 4 5 6)
;; (map (partial (- 3 _)) (range 10))
;; => (3 2 1 0 -1 -2 -3 -4 -5 -6)

;; would be nice, but I don't see a way to do it.
;; (partial (* _ (+ x _)))
;; we need a way to recursively walk the expression
;;   while accumulating into a flat list of formals.

(defmacro uncurry
  (uncurry (lambda (x ...) (lambda (y ...) body ...)))
  -> (lambda (x ... y ...) body ...)
  )
