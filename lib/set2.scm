;; -*- Mode: Irken -*-

(define (set-class)

  (define (in self x)
    (let loop ((l self.list))
      (match l with
	() -> #f
	(hd . tl) -> (if (eq? hd x)
			 #t
			 (loop tl)))))
  (define (add self x)
    (if (self::in x) ;; note inline
	#u
	(set! self.list (list:cons x self.list))))
  
  (define (get self)
    self.list)

  (define (iterate self p)
    (let loop ((l self.list))
      (match l with
	() -> #u
	(hd . tl) -> (begin (p hd) (loop tl)))))

  (let ((methods {in=in add=add get=get iterate=iterate}))
    (define (new l)
      {o=methods list=l})
    new
    ))

(define set-maker (set-class))
