;; -*- Mode: Irken -*-

(define x (:thing 1 2 #\A))
(define y (:other "testing"))

(define myfun
  (:thing _ two _) -> two
  (:other _)       -> 19
  )

(:zorb
 (myfun x)
 (myfun y)
 (myfun (:thing 7 12 #\X))
 )


