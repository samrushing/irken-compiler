
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (print x)
  (%%cexp (? -> undefined) "dump_object (%s, 0)" x))

(define (print-string s)
  (%%cexp (string -> int) "fputs (%s, stdout)" s))

(define (- x y)
  (%%cexp (int int -> int) "%s-%s" x y))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (< x y)
  (%%cexp (int int -> bool) "%s<%s" x y))

(define (> x y)
  (%%cexp (int int -> bool) "%s>%s" x y))

(define (== x y)
  (%%cexp (int int -> bool) "%s==%s" x y))

(define (random)
  (%%cexp (-> int) "random()"))

;; a quick translation of okasaki's pure functional red-black tree

;; A more 'natural' representation might be:
;; (datatype node (union (empty) (full bool node node ? ?)))
;; where the color is stored as a bool in each node.
;;
;; this saves space by encoding the color into the header of each node,
;;   but leads to some minor code duplication (due to the lack of real
;;   pattern matching).  another issue: the fact that I don't have proper
;;   type variables in this syntax means there's no way to expression that
;;   red and black nodes have the same key and value types.

(datatype node
  (union
     (empty)
     (red node node ? ?)
     (black node node ? ?)
     ))

(define (lbalance l r k v)
  (typecase node l
    ((red ll lr lk lv)
     (typecase node ll
       ((red lll llr llk llv)
	(node/red (node/black lll llr llk llv) (node/black lr r k v) lk lv))
       ((black _ _ _ _)
	(typecase node lr
           ((red lrl lrr lrk lrv)
	    (node/red (node/black ll lrl lk lv) (node/black lrr r k v) lrk lrv))
	   ((black _ _ _ _)
	    (node/black l r k v))
	   ((empty)
	    (node/black l r k v))))
       ((empty)
	(node/black l r k v))))
    ((black _ _ _ _)
     (node/black l r k v))
    ((empty)
     (node/black l r k v))))

(define (rbalance l r k v)
  (typecase node r
    ((red rl rr rk rv)
     (typecase node rr
       ((red rrl rrr rrk rrv)
	(node/red (node/black l rl k v) (node/black rrl rrr rrk rrv) rk rv))
       ((black _ _ _ _)
	(typecase node rl
          ((red rll rlr rlk rlv)
	   (node/red (node/black l rll k v) (node/black rlr rr rk rv) rlk rlv))
	  ((black _ _ _ _)
	   (node/black l r k v))
	  ((empty)
	   (node/black l r k v))))
       ((empty)
	(node/black l r k v))))
    ((black _ _ _ _)
     (node/black l r k v))
    ((empty)
     (node/black l r k v))))

(define (insert t < k v)
  (define (ins n)
    (typecase node n
      ((empty)
       (node/red (node/empty) (node/empty) k v))
      ((red l r k2 v2)
       (cond ((< k k2)
	      (node/red (ins l) r k2 v2))
	     ((< k2 k)
	      (node/red l (ins r) k2 v2))
	     (else n)))
      ((black l r k2 v2)
       (cond ((< k k2)
	      (lbalance (ins l) r k2 v2))
	     ((< k2 k)
	      (rbalance l (ins r) k2 v2))
	     (else n)))))
  (let ((s (ins t)))
    (typecase node s
      ((black _ _ _ _) s)
      ((red l r k v) (node/black l r k v))
      ((empty) s) ;; impossible, should raise something here?
      )))

(datatype maybe
   (union
    (no)
    (yes ?)
    ))

(define (member t < key)
  (let member0 ((n t))
    (typecase node n
       ((empty) (maybe/no))
       ((red l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (maybe/yes v))))
       ((black l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (maybe/yes v)))))))

(define (print-spaces n)
  (let loop ((n n))
    (cond ((> n 0)
	   (print-string "  ")
	   (loop (- n 1))))))

(define (print-item k v d)
  (print-spaces d)
  (print k)
  (print-string ":")
  (print v)
  (print-string "\n"))

(define (print-tree t)
  (let p ((n t) (d 0))
    (typecase node n
      ((empty) #u)
      ((red l r k v)   (p l (+ d 1)) (print-item k v d) (p r (+ d 1)))
      ((black l r k v) (p l (+ d 1)) (print-item k v d) (p r (+ d 1))))
    ))

(define (inorder t p)
  (let inorder0 ((n t))
    (typecase node n
      ((empty))
      ((red l r k v)   (inorder0 l) (p k v) (inorder0 r))
      ((black l r k v) (inorder0 l) (p k v) (inorder0 r))
      )))

(define (random-tree n)
  (let loop ((i n)
	     (t (node/empty)))
    (if (== i 0)
	t
	(loop (- i 1) (insert t < (random) i)))))

(let ((t (node/empty)))
  (print-string "testing\n")
  (set! t (insert t < 19 2000))
  (set! t (insert t < 12 1000))
  (set! t (insert t < 99 9900))
  (set! t (insert t < 8 800))
  (set! t (insert t < -30 -3000))
  (printn t)
  (print-tree t)
  (printn (member t < 12))
  (printn (member t < 20))
  (printn (member t < 19))
  (define (print-item k v)
    (print k)
    (print-string ":")
    (print v)
    (print-string ","))
  (let ((t2 (random-tree 20)))
    (print-tree t2)
    (inorder t2 print-item)
    )
  (print-string "\n")
  (printn (insert (insert (node/empty) < 0 "howdy!") < 1 "there"))
  )
