
(define (< a b)
  (%%cexp (int int -> bool) "%s<%s" a b))

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

(let ((t (:empty)))
  (let ((t (insert t < 19 2000)))
    (let ((t (insert t < 43 1000)))
      t)))
