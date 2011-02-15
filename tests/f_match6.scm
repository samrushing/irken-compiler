
;; should trigger a error for incomplete match

(datatype bool (:true) (:false))

(datatype thing
  (:one)
  (:two)
  (:three)
  )

(define test
  (thing:one) -> #t
  (thing:two) -> #f
  )

(test (thing:three))
