;; -*- Mode: Irken -*-

(datatype set-ob
  (:t {list=(list 'a)})
  )

(define (set-class)

  (define (in self x)
    (let loop ((l self.list))
      (match l with
	() -> #f
	(hd . tl) -> (if (eq? hd x)
			 #t
			 (loop tl)))))
  (define (add self x)
    (if (in self x)
	#u
	(set! self.list (list:cons x self.list))))
  
  (define (get self)
    self.list)

  (define (iterate self p)
    (let loop ((l self.list))
      (match l with
	() -> #u
	(hd . tl) -> (begin (p hd) (loop tl)))))

  (define un (set-ob:t self) -> self)

  (let ((methods {in=in add=add get=get iterate=iterate un=un}))
    (define (new l)
      {o=methods self=(set-ob:t {list=l})})
    new
    ))

(define set-maker (set-class))
