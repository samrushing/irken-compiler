


;; what are the rules for deciding when it's safe to inline an argument/function?

;; in this case:
;;(let ((y 5))
;;  ((lambda (x) (set! y 3) x) y))
;; we should *not* inline the <y> inside the function, because
;;  it is assigned to.  but will this transformation work?
;(let ((y 5))
;  (let ((y2 y))
;    (set! y 3)
;    y2))

;; two different vars to worry about.
;; ((lambda (x) ...) y)
;; assign to y (either inside or outside the fun)
;; assign to x
;;
;; assignment to *either* requires making it a <let>

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (error x)
  (printn x)
  (%%cexp (-> 'a) "goto Lreturn")
  ;; NOTREACHED
  ;; note: keep the 'a there... it allows a call to <error> to take any type...
  (%%cexp (-> 'a) "PXLL_UNDEFINED")
  )

(let ((stack (:empty)))

  (define (push item)
    (set! stack (:elem item stack)))

  (define (pop)
    (vcase stack
      ((:empty) (error "underflow"))
      ((:elem item rest)
       (set! stack rest)
       item)))
  
  (printn stack)
  (push 1)
  (printn stack)
  (push 2)
  (printn stack)  
  (printn (pop))
  (printn stack)
  (printn (pop))
  (printn stack)
  (printn (pop))
  (printn stack)
  )

