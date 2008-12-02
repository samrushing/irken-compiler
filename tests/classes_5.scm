
;; now trying a recursive record type

(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (- x y)
  (%%cexp (int int -> int) "%s-%s" x y))

(define (= x y)
  (%%cexp (int int -> bool) "%s==%s" x y))

(class list (head next:list))

;; ugh, this is such a hack...
(define (make-nil)
  (%%cexp (-> list) "(object*)0x0a"))
(define (nil? t:list)
  (%%cexp (list -> bool) "%s==(object*)0x0a" t))

(define (range n)
  (let loop ((i n)
	     (r (make-nil))
	     )
    (if (= i 0)
	r
	(loop (- i 1) (list i r)))))

(define (range2 n p)
  (let loop ((i n)
	     (r (make-nil))
	     )
    (if (= i 0)
	r
	(loop (- i 1) (list (p i) r)))))

(define (length l:list)
  (let loop ((r 0)
	     (l l))
    (if (nil? l)
	r
	(loop (+ r 1) l.next))))

(let ((o1 (list 19 (list 34 (make-nil))))
      (o2 (range 100))
      (o3 (range2 20 (lambda (x) "x")))
      )
  (if (nil? o1)
      (printn "is nil")
      (printn "not nil"))
  (printn o1)
  (printn o2)
  (printn o3)
  (printn (length o1))
  (printn (length o2))
  (printn (length o3))
  (printn o3.next.next.next.head)
  )
