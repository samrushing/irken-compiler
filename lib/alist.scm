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

(defmacro alist/make
  (alist/make)                     -> (alist:nil)
  (alist/make (k0 v0) (k1 v1) ...) -> (alist:entry k0 v0 (alist/make (k1 v1) ...))
  )

(defmacro alist/push
  (alist/push a k v) -> (set! a (alist:entry k v a))
  )
