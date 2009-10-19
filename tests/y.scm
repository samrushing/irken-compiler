
;; see "Y in Practical Programs" by Bruce McAdam.

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (- x y)
  (%%cexp (int int -> int) "%s-%s" x y))

(define (* x y)
  (%%cexp (int int -> int) "%s*%s" x y))

(define (= x y)
  (%%cexp (int int -> bool) "%s==%s" x y))

(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

;; ((<t43>, (<t37>,)), ((<t43>, (<t41 (<t43>, (<t37>,))>, <t37>)),))
;; t43=b t37=a
;; ((b, (a,)),         ((b,     (     (   b , (a,)), a)),))
;; ((b, (a,)), ((b,((b,(a,)),a)),))
(define (Y f)
  (lambda (x)
    (f (Y f) x)))

(define (fact0 fact x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

;(let ((fact (Y fact0)))
;  (printn (fact 5)))

;; boy, it sure is tricky untangling all that currying.
;; how do those ML guys do it?

(define (printer-wrapper f0)
  (lambda (f x)
    (let ((result (f0 f x)))
      (printn result)
      result)))

(let ((fact-print (Y (printer-wrapper fact0))))
  (fact-print 5))
