;; -*- Mode: Irken -*-

(include "lib/basis.scm")

;; catch a raised exception, with its payload
(printn
 (try
  (begin (raise (:Oops 42)) "not reached")
  except
  (:Oops n) -> (format "caught " (int n))))
;; => "caught 42"

;; the body's value when nothing is raised
(printn (try 'fine except (:Oops _) -> 'caught))
;; => fine

;; raised from deep inside calls
(define (dive n)
  (if (= n 0)
      (raise (:Bottom "deep"))
      (+ 1 (dive (- n 1)))))
(printn (try (dive 1000) except (:Bottom s) -> (string-length s)))
;; => 4

;; an exception the inner handler doesn't match goes to the outer one
(printn
 (try
  (try
   (if (> (length (list 1)) 5) (raise (:Inner 0)) (raise (:Outer 1)))
   except
   (:Inner _) -> "inner")
  except
  (:Outer n) -> (format "outer " (int n))))
;; => "outer 1"

;; handlers are restored: after a try finishes, a later raise goes to the
;;  enclosing handler, not the stale inner one.
(printn
 (try
  (begin
    ;; (:A is raised only in principle: irken rejects a handler for a
    ;;  variant that's never constructed.)
    (try (if (> (length (list 1)) 5) (raise (:A)) 'quiet) except (:A) -> 'wrong)
    (raise (:B)))
  except
  (:A) -> 'stale-handler
  (:B) -> 'right-handler))
;; => right-handler

;; raise from inside a handler
(printn
 (try
  (try (raise (:First)) except (:First) -> (raise (:Second)))
  except
  (:Second) -> 'second))
;; => second

;; exceptions in a loop: catch, continue
(define (safe-recip n)
  (try (if (= n 0) (raise (:DivZero)) (/ 100 n))
       except (:DivZero) -> -1))
(printn (map safe-recip (list 5 0 20 0)))
;; => (20 -1 5 -1)

;; library exceptions: string->int on bad input
(printn (try (string->int "12x") except (:String/BadInt s) -> (- (string-length s))))
;; => -3
