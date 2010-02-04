
(let ((stack (:nil)))
  
  (define (push x)
    (set! stack (:elem x stack)))

  (push "howdy")
  (push (:nt "howdy"))
  (push "there")
  stack)
