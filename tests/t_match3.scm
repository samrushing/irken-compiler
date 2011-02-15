;; -*- Mode: Irken -*-

;; repro a bug in something, somewhere: <hd> not getting the let_subst treatment.

(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%0==%1" a b))

(datatype list (:nil) (:cons 'a (list 'a)))
(datatype color (:red) (:green))

;; bug is in wrapped let_subst somehow, probably need to be performed
;;   in a particular direction, and we're doing it wrong.

(define (thing exps)
  (define recur
    ()	  -> 0
    (xx . tl) -> (match xx with
		   0 -> (recur tl)
		   yy -> (recur (list:cons yy tl))
		   ))
  (recur exps))
