;; -*- Mode: Irken -*-

(datatype bool (:true) (:false))

(define (< a b)
  (%%cexp (int int -> bool) "%s<%s" a b))

(defmacro zand
  (zand)                 -> #t
  (zand test)            -> test
  (zand test1 test2 ...) -> (if test1 (zand test2 ...) #f)
  )

(zand (< 1 2) (< 3 4) (< 5 6))
