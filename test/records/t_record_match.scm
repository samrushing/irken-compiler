;; -*- Mode: Irken -*-

;; record patterns.

(include "lib/basis.scm")

(define thing
  {a=x b=2} -> x
  {a=3 b=y} -> y
  {a=m b=n} -> (+ m n))

(printn (list (thing {a=3 b=1}) (thing {a=3 b=2}) (thing {a=4 b=5})))
;; => (1 3 9)

;; a record pattern inside a datatype pattern
(datatype msg
  (:move {dx=int dy=int})
  (:say string))

(define handle
  (msg:move {dx=0 dy=0}) -> "stay"
  (msg:move {dx=dx dy=0}) -> (format "horizontal " (int dx))
  (msg:move {dx=dx dy=dy}) -> (format "move " (int dx) "," (int dy))
  (msg:say s) -> (format "say " s))

(printn (map handle (list (msg:move {dx=0 dy=0}) (msg:move {dx=5 dy=0})
                          (msg:move {dx=1 dy=2}) (msg:say "hi"))))
;; => ("stay" "horizontal 5" "move 1,2" "say hi")
