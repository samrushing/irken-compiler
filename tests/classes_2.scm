
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(class list (car cdr:list) )

;; type hack to get a 'nil' object,
(define (make-nil)
  ;; note that this is unsafe, since anyone can try to take its car or cdr...
  (%%cexp (-> list) "(object*)0x0a")
  ;; slightly more safe version...
  ;; (%%cexp (-> list) "allocate (0x20, 2)"))
  )

(let ((l1 (make-list 3 (make-list 5 (make-nil))))
      (l2 (make-list #t (make-list #f (make-nil)))))
  (printn l1.car)
  (printn l1.cdr.car)
  (printn l2.car)
  (printn l2.cdr.car)
  )
