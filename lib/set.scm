;; -*- Mode: Irken -*-

;; simple set datatype using lists.

(define (make-set l =)

  (define (in x)
    (let loop ((l l))
      (match l with
	() -> #f
	(hd . tl) -> (if (= hd x)
			 #t
			 (loop tl)))))

  (define (add! x)
    (if (in x)
	#u
	(set! l (list:cons x l))))

  (define (get) l)

  ;; unfort the record field name syntax is limited to A-Z0-9_
  {add=add! in=in get=get}

  )
