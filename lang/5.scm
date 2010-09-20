
(define (fun x y)
  (match (+ 1 x) y with
     12 z -> (+ z 9)
     15 9 -> 0
     x  y -> (+ x y)
     ))

(match <exp0> <exp1> ... with
  <pat0> <pat1> ... -> <result0>
  <pat0> <pat1> ... -> <result1>
  )

