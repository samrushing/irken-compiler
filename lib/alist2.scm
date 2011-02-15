;; -*- Mode: Irken -*-

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
	 {add=add
	  get=lookup
	  get-default=lookup*
	  get-err=get-error
	  iterate=iterate
	  map=map
	  keys=keys
	  values=values}))
    ;; new method
    (lambda () {o=methods alist=(alist:nil)})))

(define alist-maker (alist-class))
