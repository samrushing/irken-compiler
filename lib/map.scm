;; -*- Mode: Irken -*-

;; OO/imperative map and set interfaces for frb.scm

;; XXX now that delete is available, add delete methods.

(define (map-class)

  (define (add self k v)
    (match (tree/member self.t self.cmp k) with
      ;;(maybe:yes _) -> (raise (:KeyError "key already present in map" k))
      (maybe:yes _) -> (error1 "key already present in map" k)
      (maybe:no) -> (set! self.t (tree/insert self.t self.cmp k v))))

  (define (maybe-add self k v)
    (match (tree/member self.t self.cmp k) with
      (maybe:yes _) -> #u
      (maybe:no) -> (set! self.t (tree/insert self.t self.cmp k v))))

  (define (lookup self k)
    (tree/member self.t self.cmp k))

  (define (lookup* self k default)
    (match (tree/member self.t self.cmp k) with
      (maybe:yes v) -> v
      (maybe:no) -> default))

  (define (get-error self k errstring)
    (match (tree/member self.t self.cmp k) with
      (maybe:yes v) -> v
      (maybe:no) -> (error1 errstring k)))

  (define (iterate self p)
    (tree/inorder p self.t))

  (define (riterate self p)
    (tree/reverse p self.t))

  (define (map self p)
    (let ((r '()))
      (tree/reverse (lambda (k v) (PUSH r (p k v))) self.t)
      r))

  (define (keys self)
    (tree/keys self.t))

  (define (values self)
    (tree/values self.t))

  (let ((methods
	 {add=add
	  maybe-add=maybe-add
	  get=lookup
	  get-default=lookup*
	  get-err=get-error
	  iterate=iterate
	  riterate=riterate
	  map=map
	  keys=keys
	  values=values}))
    ;; new method
    (lambda (cmp)
      ;; don't modify the cmp function slot, you dolt.
      {o=methods self={t=(tree/empty) cmp=cmp}}
      )))

(define map-maker (map-class))

;; set class using 'tree' and ignoring values.
(define (set2-class)

  (define (add self k)
    (match (tree/member self.t self.cmp k) with
      (maybe:yes _) -> #u
      (maybe:no) -> (set! self.t (tree/insert self.t self.cmp k #u))))

  (define (member self k)
    (match (tree/member self.t self.cmp k) with
      (maybe:yes _) -> #t
      (maybe:no)    -> #f
      ))

  (define (iterate self p)
    (tree/inorder
     (lambda (k v) (p k))
     self.t))

  (define (keys self)
    (tree/keys self.t))

  (let ((methods
	 {add=add
	  member=member
	  iterate=iterate
	  keys=keys}))
    ;; new method
    (lambda (cmp)
      ;; don't modify the cmp function slot, you dolt.
      {o=methods self={t=(tree/empty) cmp=cmp}}
      )))

(define set2-maker (set2-class))
