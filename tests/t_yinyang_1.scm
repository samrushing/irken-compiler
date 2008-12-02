(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define (id x) x)

;; (let ((yin #f)
;;       (yang #f))
;;   (set! yin  ((lambda (foo) (print-string "\n") foo) (^call/cc id)))
;;   (set! yang ((lambda (foo) (print-string "*") foo) (^call/cc id)))
;;   (yin yang))

(let ((yin ((lambda (foo) (print-string "\n") foo) (^call/cc id))))
  (let ((yang ((lambda (foo) (print-string "*") foo) (^call/cc id))))
    (yin yang)))

;;(let ((x xinit)) body) =>
;; ((lambda (x) body) xinit)

;(define (thing x y)
;  (+ x y))

;(thing 3 4)

;((lambda (yin)
;   ((lambda (yang) (yin yang))
;    ((lambda (foo) (print-string "*") foo) (^call/cc id))))
; ((lambda (foo) (print-string "\n") foo) (^call/cc id)))


