
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(let ((l (%dtcon/list/nil))
      (is (lambda (e)
	    (set! l (%dtcon/list/cons e l)))))
  (is 10)
  (is #f))