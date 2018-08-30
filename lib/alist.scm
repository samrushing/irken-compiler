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

(define alist->keys*
  (alist:nil)          acc -> acc
  (alist:entry k _ tl) acc -> (alist->keys* tl (list:cons k acc)))

(define (alist->keys al)
  (alist->keys* al (list:nil)))

(define alist->values*
  (alist:nil)          acc -> acc
  (alist:entry _ v tl) acc -> (alist->values* tl (list:cons v acc)))

(define (alist->values al)
  (alist->values* al (list:nil)))

(define alist/length*
  (alist:nil)          acc -> acc
  (alist:entry _ _ tl) acc -> (alist/length* tl (+ 1 acc)))

(define (alist/length al)
  (alist/length* al 0))

;; create a inverse map of an existing alist.
(define (alist/inverse al)
  (let ((r (alist:nil)))
    (alist/iterate
     (lambda (k v)
       (alist/push r v k))
     al
     )
    r))

(define alist/map*
  p acc (alist:nil) -> acc
  p acc (alist:entry k v tl) -> (alist/map* p (list:cons (p k v) acc) tl)
  )

(define (alist/map p al)
  (alist/map* p '() al))

(defmacro for-alist
  (for-alist k v alist body ...)
  -> (alist/iterate (lambda (k v) body ...) alist)
  )

;; imperative alist 'object'.

(datatype alist-ob
  (:t {alist=(alist 'a 'b)})
  )

(define (alist-maker)

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

  (define un (alist-ob:t self) -> self)

  (let ((methods
	 {add         = add
	  get         = lookup
	  get-default = lookup*
	  get-err     = get-error
	  iterate     = iterate
	  map         = map
	  keys        = keys
	  values      = values
          un          = un}))
    ;; new method
    {o=methods self=(alist-ob:t {alist=(alist:nil)})}
    ))
