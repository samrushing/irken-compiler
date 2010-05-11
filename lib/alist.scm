;; -*- Mode: Scheme -*-

;; lisp 'association list'

(datatype alist
  (:nil)
  (:entry symbol 'a (alist 'a))
  )

(define (alist/add l key val)
  (alist:entry key val l))

(define (alist/new)
  (alist:nil))

(define alist/lookup
  (alist:nil) key
  -> (maybe:no)
  (alist:entry key0 val0 rest) key
  -> (if (eq? key0 key)
	 (maybe:yes val0)
	 (alist/lookup rest key)))
