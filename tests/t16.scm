
(define (random)
  (%%cexp (-> int) "random()"))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (< a b)
  (%%cexp (int int -> bool) "%s<%s" a b))

(define (> a b)
  (%%cexp (int int -> bool) "%s>%s" a b))

(define (print x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0)" x))

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (print-string s)
  (%%cexp (string -> int) "fputs (%s, stdout)" s))

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

(define (lbalance l r k v)
  (vcase l
    ((:red ll lr lk lv)
     (vcase ll
       ((:red lll llr llk llv)
	(:red (:purple lll llr llk llv) (:purple lr r k v) lk lv))
       ((:purple _ _ _ _)
	(vcase lr
	  ((:red lrl lrr lrk lrv)
	   (:red (:purple ll lrl lk lv) (:purple lrr r k v) lrk lrv))
	  ((:purple _ _ _ _)
	   (:purple l r k v))
	  ((:empty)
	   (:purple l r k v))))
       ((:empty)
	(:purple l r k v))))
    ((:purple _ _ _ _)
     (:purple l r k v))
    ((:empty)
     (:purple l r k v))))

(define (rbalance l r k v)
  (vcase r
    ((:red rl rr rk rv)
     (vcase rr
       ((:red rrl rrr rrk rrv)
	(:red (:purple l rl k v) (:purple rrl rrr rrk rrv) rk rv))
       ((:purple _ _ _ _)
	(vcase rl
          ((:red rll rlr rlk rlv)
	   (:red (:purple l rll k v) (:purple rlr rr rk rv) rlk rlv))
	  ((:purple _ _ _ _)
	   (:purple l r k v))
	  ((:empty)
	   (:purple l r k v))))
       ((:empty)
	(:purple l r k v))))
    ((:purple _ _ _ _)
     (:purple l r k v))
    ((:empty)
     (:purple l r k v))))

(define (insert root < k v)
  (define (ins n)
    (vcase n
      ((:empty)
       (:red (:empty) (:empty) k v))
      ((:red l r k2 v2)
       (cond ((< k k2)
	      (:red (ins l) r k2 v2))
	     ((< k2 k)
	      (:red l (ins r) k2 v2))
	     (else n)))
      ((:purple l r k2 v2)
       (cond ((< k k2)
	      (lbalance (ins l) r k2 v2))
	     ((< k2 k)
	      (rbalance l (ins r) k2 v2))
	     (else n)))))
  (let ((s (ins root)))
    (vcase s
      ((:purple _ _ _ _) s)
      ((:red l r k v) (:purple l r k v))
      ((:empty) s) ;; impossible, should raise something here?
      )))

(define (member root < key)
  (let member0 ((n root))
    (vcase n
       ((:empty) (:no))
       ((:red l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (:yes v))))
       ((:purple l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (:yes v)))))))

(define (inorder t p)
  (let inorder0 ((n t))
    (vcase n
      ((:empty) #f)
      ((:red l r k v)    (inorder0 l) (p k v) (inorder0 r) #f)
      ((:purple l r k v) (inorder0 l) (p k v) (inorder0 r) #f)
      )))

(define (reverse n p)
  (let reverse0 ((n n))
    (vcase n
      ((:empty) #f)
      ((:red l r k v)    (reverse0 r) (p k v) (reverse0 l) #f)
      ((:purple l r k v) (reverse0 r) (p k v) (reverse0 l) #f)
      )))

(define (n-random n)
  (let loop ((n n)
	     (t (:empty)))
    (if (= n 0)
	t
	(loop (- n 1) (insert t < (random) (random))))))

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

(define (print-tree n)
  (let p ((n n) (d 0))
    (vcase n
      ((:empty) #u)
      ((:red l r k v)    (begin (p l (+ d 1)) (print-item k v d) (p r (+ d 1))))
      ((:purple l r k v) (begin (p l (+ d 1)) (print-item k v d) (p r (+ d 1)))))
    ))

(define (print-kv k v)
  (print k)
  (print-string " ")
  (print v)
  (print-string "\n"))

(let ((t (n-random 20)))
  (print-string "inorder:\n")
  (inorder t print-kv)
  (print-string "reverse:\n")
  (reverse t print-kv)
  (set! t (insert t < 1234 5000))
  (printn (member t < 1234))
  (printn (member t < 9999))
  (print-tree t)
  )
