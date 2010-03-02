
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (> a b)
  (%%cexp (int int -> bool) "%s>%s" a b))

(define (< a b)
  (%%cexp (int int -> bool) "%s<%s" a b))

;; #t > #f
(define (bool->? a b)
  (if a
      (if b #f #t)
      #t))

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

;; (define (list:insert x l >)
;;   (define (ins l)
;;     (vcase list l
;;        ((:nil) (list:cons x l))
;;        ((:cons hd tl)
;; 	(if (> hd x)
;; 	    (list:cons x l)
;; 	    (list:cons hd (ins tl))))))
;;   (ins l))

(define (list:insert x l >)
  (let loop ((l l))
    (vcase list l
       ((:nil) (list:cons x l))
       ((:cons hd tl)
	(if (> hd x)
	    (list:cons x l)
	    (list:cons hd (loop tl)))))))

(let ((l0 (list:insert -20 (list:nil) bool->?))
      (l1 (list:nil))
      )
   (set! l0 (list:insert 1 l0 >))
   (printn l0)
   (set! l0 (list:insert 5 l0 >))
   (printn l0)
   (set! l1 (list:insert #f l1 bool->?))
   (printn l1)
   (set! l1 (list:insert #t l1 bool->?))
   (printn l1)
  )

