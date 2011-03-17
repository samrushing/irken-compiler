;; -*- Mode: Irken -*-

;; lisp 'association list'

(datatype alist
  (:nil)
  (:entry 'a 'b (alist 'a 'b))
  )

(define alist/lookup
  (alist:nil)            k -> (maybe:no)
  (alist:entry k0 v0 tl) k -> (if (eq? k0 k)
				  (maybe:yes v0)
				  (alist/lookup tl k))
  )

(define (alist/lookup* l k default)
  (match (alist/lookup l k) with
    (maybe:yes v) -> v
    (maybe:no)    -> default
    ))

(define (alist/get l k err)
  (match (alist/lookup l k) with
    (maybe:yes v) -> v
    (maybe:no)    -> (error1 err k)))

(defmacro alist/make
  (alist/make)                     -> (alist:nil)
  (alist/make (k0 v0) (k1 v1) ...) -> (alist:entry k0 v0 (alist/make (k1 v1) ...))
  )

(defmacro alist/push
  (alist/push a k v) -> (set! a (alist:entry k v a))
  )

(define alist/iterate
  p (alist:nil) -> #u
  p (alist:entry k v tl) -> (begin (p k v) (alist/iterate p tl))
  )

(define alist/map
  p (alist:nil) -> (list:nil)
  p (alist:entry k v tl) -> (list:cons (p k v) (alist/map p tl))
  )

(define alist->keys
  (alist:nil) -> (list:nil)
  (alist:entry k _ tl) -> (list:cons k (alist->keys tl)))

(define alist->values
  (alist:nil) -> (list:nil)
  (alist:entry _ v tl) -> (list:cons v (alist->keys tl)))

(define alist/length
  (alist:nil) -> 0
  (alist:entry _ _ tl) -> (+ 1 (alist/length tl)))

;; imperative alist object
;;
;; XXX: when I originally wrote this, these 'methods' merely used
;;  the functions defined above.  This may have led to a nasty bug
;;  wherein the program typed differently depending on the phase of
;;  the moon.  I suspect/hope that this is a problem with using records
;;  this way - I seem to remember something about such restrictions on
;;  ocaml's object classes... rewriting this with the code inside the
;;  closure seems to have fixed it... fingers crossed...

(define (make-alist)
  (let ((alist (alist:nil)))
    (define (add k v)
      (set! alist (alist:entry k v alist)))
    (define (lookup k0)
      (let loop ((l alist))
	(match l with
	  (alist:nil) -> (maybe:no)
	  (alist:entry k1 v1 tl) -> (if (eq? k0 k1)
					(maybe:yes v1)
					(loop tl)))))
    (define (lookup* k default)
      (match (lookup k) with
	(maybe:no) -> default
	(maybe:yes v) -> v))
    (define (iterate p)
      (let loop ((l alist))
	(match l with
	  (alist:nil) -> #u
	  (alist:entry k v tl) -> (begin (p k v) (loop tl)))))
    (define (map p)
      (let loop ((acc '())
		 (l alist))
	(match l with
	  (alist:nil) -> (reverse acc)
	  (alist:entry k v tl) -> (loop (list:cons (p k v) acc) tl))))
    (define (keys)
      (let loop ((acc '())
		 (l alist))
	(match l with
	  (alist:nil) -> (reverse acc)
	  (alist:entry k _ tl) -> (loop (list:cons k acc) tl))))
    (define (values)
      (let loop ((acc '())
		 (l alist))
	(match l with
	  (alist:nil) -> (reverse acc)
	  (alist:entry _ v tl) -> (loop (list:cons v acc) tl))))
    {add=add
     get=lookup
     get-default=lookup*
     iterate=iterate
     map=map
     keys=keys
     values=values}
    ))
