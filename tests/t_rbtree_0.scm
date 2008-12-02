
(include "lib/core.scm")
(include "lib/vector.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/random.scm")
(include "lib/rbtree.scm")

(define (print-node k v)
  (%print (cons k v)))

(define (random-tree size)
  (let ((t (make-tree)))
    (let loop ((n size) (r (random)))
      (cond ((%gt? n 0)
	     (tree-set! t r 0)
	     (loop (%- n 1) (random)))))
    t))

(let ((t0 (make-tree)))
  (tree-set! t0 19 "nineteen")
  (tree-set! t0 12 "twelve")
  (tree-set! t0 0 "zero")
  (%printn (tree-get t0 19))
  (%printn (tree-get t0 0))
  (%printn (tree-get t0 15))
  )
  
(let ((t0 (random-tree 1000)))
  (let loop ((n 1000))
    (cond ((%gt? n 0)
	   (pop-tree-min! t0 print-node)
	   (loop (%- n 1))))))

