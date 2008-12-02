(include "lib/core.scm")
(include "lib/pair.scm")

;; this one explains the strange defn of letrec in r5rs
;; where the tag is returned then applied.  the bound name
;; is technically supposed to be invisible in the <inits>.
(%printn
 (let - ((n (- 0 1))) n))

;; incorrect:
;851  primapp [8] %printn
;850    fix [7] [{-_124}]
;845      function [2] ['-_124', [{n_125}], False]
;844        varref [1] n_125
;849      application [4]
;846        varref [1] -_124
;847        literal [1] ('int', 0)
;848        literal [1] ('int', 1)

;;; correct:
;875  primapp [12] %printn
;874    application [11]
;864      fix [4] [{-_124}]
;862        function [2] ['-_124', [{n_125}], False]
;861          varref [1] n_125
;863        varref [1] -_124
;873      sequence [6]
;867        verify [1] ('TC_INT', 1)
;866          literal [1] ('int', 0)
;869        verify [1] ('TC_INT', 1)
;868          literal [1] ('int', 1)
;872        primapp [3] %-
;870          literal [1] ('int', 0)
;871          literal [1] ('int', 1)

;; our append is binary only
;(%printn
; (let ((ls (list 1 2 3 4)))
;   (append ls ls '(5))))

;; hmm... this one actually seems to work!
(%printn
(let ((r #f)
      (a #f)
      (b #f)
      (c #f)
      (i 0))
    (set! r (+ 1
	       (+ (+ 2 (+ 3 (^call/cc (lambda (k) (set! a k) 4))))
		  (+ 5 (+ 6 (^call/cc (lambda (k) (set! b k) 7))))))
	  )
    (if (not c) 
        (set! c a))
    (set! i (+ i 1))
    (case i
      ((1) (a 8))
      ((2) (b 8))
      ((3) (a 7))
      ((4) (c 4)))
    r))

(%printn
(let ((cont #f))
    (letrec ((x (^call/cc (lambda (c) (set! cont c) 0)))
             (y (^call/cc (lambda (c) (set! cont c) 0))))
      (if cont
          (let ((c cont))
            (set! cont #f)
            (set! x 1)
            (set! y 1)
            (c 0))
          (+ x y)))))
