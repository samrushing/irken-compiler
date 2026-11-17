;; -*- Mode: Irken -*-

;; exceptions and generators (#73): the generator and its caller each have
;;  their own exception handler.

(include "lib/basis.scm")

;; 1. an exception the generator doesn't catch reaches the caller.
(define (gen1)
  (makegen emit
    (emit 1)
    (raise (:Oops 2))
    (emit 3)))

(printn
 (try
  (let ((r '()))
    (for x (gen1) (push! r x))
    r)
  except
  (:Oops n) -> (list -1 n)))
;; => (-1 2)

;; 2. a `try` in the generator, active when it yields, doesn't catch the
;;  caller's exception.  (it used to: the caller's exception disappeared
;;  into the generator.)
(define (gen2)
  (makegen emit
    (try
     (begin (emit 1) (emit 2))
     except
     (:Outer) -> (printn 'generator-caught-it))))

(printn
 (try
  (begin
    (for x (gen2)
      (printn x)
      (raise (:Outer)))
    'no-exception)
  except
  (:Outer) -> 'caller-caught-it))
;; => 1
;; => caller-caught-it

;; 3. the generator's exception goes to whoever called it *last*.
(define (gen3)
  (makegen emit
    (emit 1)
    (raise (:Late))))

(let ((g (gen3)))
  (printn (g))
  (printn
   (try
    (begin (g) 'no-exception)
    except
    (:Late) -> 'second-caller-caught-it)))
;; => {u0 1}
;; => second-caller-caught-it

;; 4. a generator can still catch its own exceptions.
(define (gen4)
  (makegen emit
    (emit
     (try
      (raise (:Mine 7))
      except
      (:Mine n) -> n))
    (emit 8)))

(for x (gen4) (printn x))
;; => 7
;; => 8
