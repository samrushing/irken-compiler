(include "lib/core.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")
(include "lib/list.scm")

(define (nqueens n)

  (define (one-to n)
    (let loop ((i n) (l '()))
      (if (= i 0) l (loop (- i 1) (cons i l)))))

  (define (try x y z)
    (typecase list x
      ((nil) (if (null? y) 1 0))
      ((cons hd tl)
       (+ (if (ok? hd 1 z)
	      (try (append tl y) '() (cons hd z))
	      0)
	  (try tl (cons hd y) z)))))

  (define (ok? row dist placed)
    (typecase list placed
      ((nil) #t)
      ((cons hd tl)
       (and (not (= hd (+ row dist)))
	    (not (= hd (- row dist)))
	    (ok? row (+ dist 1) tl)))))

  (printn (one-to 10))
  (try (one-to n) '() '())
  )

(printn (nqueens 12))
