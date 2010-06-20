;; -*- Mode: Irken -*-

;; lisp 'association list' - keyed by symbol

(datatype alist
  (:nil)
  (:entry symbol 'a (alist 'a))
  )

(define (make-alist)
  (let ((l (alist:nil)))

    (define (add key val)
      (set! l (alist:entry key val l)))

    (define (lookup key val)
      (let loop ((l l))
	(match l with
	  (alist:nil)                  -> (maybe:no)
	  (alist:entry key0 val0 rest) -> (if (eq? key0 key)
					      (maybe:yes val0)
					      (loop rest)))))
    {add=add lookup=lookup}
    ))
