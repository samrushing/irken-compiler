
(include "lib/core.scm")
(include "lib/pair.scm")

;; tests TC_EMPTY_VECTOR by triggering gc with some empty vectors in the heap

(let ((z (n-of 100 (:testing 23 #f))))
  ;; z gives the gc something to do

  (let loop ((n 10000))
    (let ((v (%make-vector 0 0)))
      (if (= n 0)
	  v
	  (loop (- n 1)))))
  
  (let loop ((n 100000))
    (let ((x #(#() #() #() #() #() #(1 2 3))))
      (set! x[5] (%make-vector 0 3))
      (set! x[0] (%make-vector 0 4))
      )
    (if (= n 0)
	#t
	(loop (- n 1))))
  )

