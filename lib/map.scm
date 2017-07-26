;; -*- Mode: Irken -*-

;; OO/imperative map and set interfaces for frb.scm

;; XXX now that delete is available, add delete methods.

(datatype map-ob
  (:t {t=(tree 'a 'b) cmp=('a 'a -> cmp)})
  )

(define (map-maker cmp)

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

  (define (union self other)
    (other::iterate
     (lambda (k v)
       (maybe-add self k v))))

  (define un (map-ob:t self) -> self)

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
	  values=values
          union=union
          un=un
          }))
    ;; don't modify the cmp function slot, you dolt.
    {o=methods self=(map-ob:t {t=(tree/empty) cmp=cmp})}
    ))

;; set class using 'tree' and ignoring values.

(datatype set2-ob
  (:t {t=(tree 'a 'b) cmp=('a 'a -> cmp)})
  )

(define (set2-maker cmp)

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

  (define un (set2-ob:t self) -> self)

  (let ((methods
	 {add=add
	  member=member
	  iterate=iterate
	  keys=keys
          un=un
          }))
    ;; don't modify the cmp function slot, you dolt.
    {o=methods self=(set2-ob:t {t=(tree/empty) cmp=cmp})}
    ))
