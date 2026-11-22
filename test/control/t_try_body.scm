;; -*- Mode: Irken -*-

;; a try body of several forms.  the try macro's pattern (begin body0 ...)
;;  had `begin` as a variable, so it matched any form, and
;;  (try (f a) (g b) except ...) became (f a (g b)).

(include "lib/basis.scm")

(try (printn 1) (printn 2) except (:X) -> #u)
(printn (try (+ 1 2) (* 10 2) except (:X) -> 0))
(printn (try (printn 3) (raise (:X)) 4 except (:X) -> 5))
(printn (try (+ 1 1) (+ 2 2) finally (printn 6)))

;; => 1
;; => 2
;; => 20
;; => 3
;; => 5
;; => 6
;; => 4
