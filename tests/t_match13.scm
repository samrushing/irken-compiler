;; -*- Mode: Irken -*-

(datatype bool (:true) (:false))
(datatype list (:nil) (:cons 'a (list 'a)))

(define demo
  () ys -> (:A ys)
  xs () -> (:B xs)
  (x . xs) (y . ys) -> (:C x xs y ys)
  )

(demo '(1 2 3) '(4 5 6))
