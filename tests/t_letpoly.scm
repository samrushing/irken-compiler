
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

;; (let ((is (list:nil))
;;       (bs (list:nil)))
;;   (set! is (list:cons 10 is))
;;   (set! bs (list:cons #f bs))
;;   bs
;;   )

(define (make-stack)
  (let ((l (list:nil)))
    (define (push e)
      (set! l (list:cons e l)))
    push
    ))

(let ((is (make-stack))
      (bs (make-stack)))
  (is 10)
  (bs #f)
  )
