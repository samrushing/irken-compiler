;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

;; ok, so the object needs to look like this:
;; {o=methods ivar0=ival0 ivar1=ival1 ...}
;; so what would a method call look like?
;; (x::add 12)
;; => (x.o.add x 12)
;; and the definition of add will take
;; a self arg...

(define (set-class)

  (define (in self x)
    (let loop ((l self.list))
      (match l with
	() -> #f
	(hd . tl) -> (if (eq? hd x)
			 #t
			 (loop tl)))))
  (define (add self x)
    (if (self::in x)
	#u
	(set! self.list (list:cons x self.list))))
  
  (define (get self)
    self.list)

  (let ((methods {in=in add=add get=get}))
    (define (new l)
      {o=methods list=l})
    new
    ))

(define set-maker (set-class))

(let ((s0 (set-maker '()))
      (s1 (set-maker '()))
      )
  (s0::add 12)
  (s0::add 12)
  (s1::add #t)
  (s0::add 10)
  (s0::add 8)
  (printn (s0::get))
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (s1::add #f)
  (s1::add #t)
  (printn (s1::get))
  )
