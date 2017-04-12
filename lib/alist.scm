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

;; XXX this should return a new alist?
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

;; create a inverse map of an existing alist.
(define (alist/inverse al)
  (let ((r (alist:nil)))
    (alist/iterate
     (lambda (k v)
       (alist/push r v k))
     al
     )
    r))

;; imperative alist 'object'.

(define (alist-class)

  (define (add self k v)
    ;; should we check for it first?
    (set! self.alist (alist:entry k v self.alist)))

  (define (lookup self k0)
    (let loop ((l self.alist))
      (match l with
	(alist:nil) -> (maybe:no)
	(alist:entry k1 v1 tl) -> (if (eq? k0 k1)
				      (maybe:yes v1)
				      (loop tl)))))

  (define (lookup* self k default)
    (match (lookup self k) with
      (maybe:no) -> default
      (maybe:yes v) -> v))

  (define (get-error self k errstring)
    (match (lookup self k) with
      (maybe:no) -> (error1 errstring k)
      (maybe:yes v) -> v))

  (define (iterate self p)
    (let loop ((l self.alist))
      (match l with
	(alist:nil) -> #u
	(alist:entry k v tl) -> (begin (p k v) (loop tl)))))

  ;; XXX this should return a new alist?
  (define (map self p)
    (let loop ((acc '())
	       (l self.alist))
      (match l with
	(alist:nil) -> (reverse acc)
	(alist:entry k v tl) -> (loop (list:cons (p k v) acc) tl))))

  (define (keys self)
    (let loop ((acc '())
	       (l self.alist))
      (match l with
	(alist:nil) -> (reverse acc)
	(alist:entry k _ tl) -> (loop (list:cons k acc) tl))))

  (define (values self)
    (let loop ((acc '())
	       (l self.alist))
      (match l with
	(alist:nil) -> (reverse acc)
	(alist:entry _ v tl) -> (loop (list:cons v acc) tl))))

  (let ((methods
	 {add         = add
	  get         = lookup
	  get-default = lookup*
	  get-err     = get-error
	  iterate     = iterate
	  map         = map
	  keys        = keys
	  values      = values}))
    ;; new method
    (lambda () {o=methods self={alist=(alist:nil)}})))

(define alist-maker (alist-class))
