;; -*- Mode: Irken -*-

;; Note: since removing support for recursive types, this code
;;   no longer works.

;; See http://en.wikipedia.org/wiki/Fixed_point_combinator

(include "lib/core.scm")

(define (Y f)
  (lambda (x)
    ((f (Y f)) x)))

(define (factabs fact)
  (lambda (x)
    (if (= x 0)
        1
        (* x (fact (- x 1))))))

(printn ((Y factabs) 5))

;; and http://caml.inria.fr/pub/docs/u3-ocaml/ocaml-ml.html#toc5
;; let fix f' = let g f x = f' (f f) x in (g g);;
;; let fact = fix fact' in fact 5;;

(define (Y2 f1)
  ;; the '^' prefix tells the compiler not to inline this function
  ;;  which in this particular case would never terminate...
  (define (^g f)
    (lambda (x)
      (f1 (f f) x)))
  (^g ^g))

(define (fact1 fact x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

(printn ((Y2 fact1) 5))
