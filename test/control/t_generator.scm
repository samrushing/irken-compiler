;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(define (count-to n)
  (makegen emit
    (for-range i n
      (emit i))))

(for x (count-to 4)
  (printn x))
;; => 0
;; => 1
;; => 2
;; => 3

(printn (generator->list (count-to 5)))
;; => (0 1 2 3 4)

;; generators over strings and lists, consumed in step
(define g1 (string-generator "abc"))
(define g2 (list-generator (list 1 2 3)))
(define (show-maybe m show)
  (match m with
    (maybe:yes x) -> (format "yes " (show x))
    (maybe:no)    -> "no"))
(printn (list (show-maybe (g1) char->string) (show-maybe (g1) char->string)))
(printn (map (lambda (_) (show-maybe (g2) int->string)) (range 4)))
;; => ("yes a" "yes b")
;; => ("yes 1" "yes 2" "yes 3" "no")

;; an infinite generator, cut short
(define (naturals)
  (makegen emit
    (let loop ((i 0))
      (emit i)
      (loop (+ i 1)))))
(printn (gen-take 6 (naturals)))   ;; gen-take returns a list
;; => (0 1 2 3 4 5)
