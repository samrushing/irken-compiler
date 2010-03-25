(datatype T
  (:E)
  (:R (T 'a) 'a (T 'a))
  (:B (T 'a) 'a (T 'a))
  )

(define (thing s)
  (vcase T s
    ((:B _ _ _) 1)
    ((:R l k r) 2)
;    ((:E)       3)
    (else 3)
    ))

;(thing (T:B (T:E) 1 (T:E)))
(thing (T:E))