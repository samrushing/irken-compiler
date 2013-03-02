;; -*- Mode: Irken; coding: utf-8 -*-

(include "lib/set.scm")

(datatype type
  (:tvar int                {parent=(maybe type) pending=bool moo=(maybe type)})
  (:pred symbol (list type) {parent=(maybe type) pending=bool moo=(maybe type)})
  )

(define (pred name subs)
  (type:pred name subs {parent=(maybe:no) pending=#f moo=(maybe:no)}))

(define (arrow result-type arg-types)
  (pred 'arrow (list:cons result-type arg-types)))

(define tvar-counter (make-counter 0))

(define (new-tvar)
  (type:tvar (tvar-counter.inc) {parent=(maybe:no) pending=#f moo=(maybe:no)}))

(define (is-pred? t name)
  (match t with
    (type:pred name0 _ _)
    -> (eq? name0 name)
    _ -> #f))

;; singleton base types
(define int-type	(pred 'int '()))
(define char-type	(pred 'char '()))
(define string-type	(pred 'string '()))
(define undefined-type	(pred 'undefined '()))
(define bool-type	(pred 'bool '()))
(define symbol-type	(pred 'symbol '()))

(define base-types
  (alist/make
   ('int int-type)
   ('char char-type)
   ('string string-type)
   ('undefined undefined-type)
   ('symbol symbol-type)
   ))

;; row types
(define (rproduct row)          (pred 'rproduct (LIST row)))
(define (rsum row)              (pred 'rsum (LIST row)))
(define (rdefault arg)          (pred 'rdefault (LIST arg)))
(define (rlabel name type rest) (pred 'rlabel (LIST name type rest)))
(define (rabs)                  (pred 'abs '()))
(define (rpre t)                (pred 'pre (LIST t)))
(define (make-label sym)        (pred sym '()))

(define type-repr
  (type:tvar id _)                    -> (format "t" (int id))
  (type:pred 'arrow (rtype atype) _)  -> (format "(" (type-repr atype) "->" (type-repr rtype) ")")
  (type:pred 'arrow (rtype . args) _) -> (format "(" (join type-repr ", " args) ")->" (type-repr rtype))
  (type:pred pred () _)               -> (format (sym pred))
  (type:pred pred args _)             -> (format (sym pred) "(" (join type-repr ", " args) ")")
  )

(define (get-tvars t)
  (let ((tvars (make-set '() eq?)))
    (define recur
      (type:pred sym types _) -> (for-each recur types)
      x			      -> (tvars.add x))
    (recur t)
    (tvars.get)))

(define type->trec
  (type:tvar _ r) -> r
  (type:pred _ _ r) -> r)

(define (type-find t)
  (let ((trec (type->trec t)))
    (match trec.parent with
      (maybe:no)    -> t
      (maybe:yes t0) -> (let ((p (type-find t0)))
			 (set! trec.parent (maybe:yes p)) ;; path compression
			 p))))

(define (type-union a b)
  (let ((pa (type-find a))
	(pb (type-find b)))
    (match pa pb with
      (type:tvar _ ra) (type:pred _ _ _)    -> (set! ra.parent (maybe:yes pb))
      (type:pred _ _ _) (type:tvar _ rb)    -> (set! rb.parent (maybe:yes pa))
      (type:tvar _ ra) (type:tvar _ rb)     -> (set! rb.parent (maybe:yes pa))
      (type:pred _ _ ra) (type:pred _ _ rb) -> (set! rb.parent (maybe:yes pa)))))

(define (parse-type* exp tvars)

  (define (get-tvar sym)
    (match (tvars::get sym) with
      (maybe:yes tv) -> tv
      (maybe:no) -> (let ((r (new-tvar)))
		      (tvars::add sym r)
		      r)))

  (define arrow-type?
    ()                      -> #f
    ((sexp:symbol '->) . _) -> #t
    (_ . tl)                -> (arrow-type? tl)
    )
  
  (define (parse-arrow-type t)
    (let loop ((args '()) (t0 t))
      (match t0 with
	((sexp:symbol '->) result-type) -> (arrow (parse result-type) (reverse args))
	(arg . rest) -> (loop (list:cons (parse arg) args) rest)
	() -> (error1 "bad arrow type" t)
	)))
  
  (define parse-predicate
    ((sexp:symbol p) . rest) -> (pred p (map parse rest))
    x -> (error1 "malformed predicate" x))

  (define parse-list
    ((sexp:symbol 'quote) (sexp:symbol tvar)) -> (get-tvar tvar)
    ((sexp:symbol 'quote) . x)  -> (error1 "malformed type?" x)
    l -> (if (arrow-type? l)
	     (parse-arrow-type l)
	     (parse-predicate l))
    )

  (define parse-field-list
    ((field:t '... _))  -> (new-tvar)
    ((field:t name type) . tl) -> (rlabel (make-label name) (rpre (parse type)) (parse-field-list tl))
    () -> (rdefault (rabs)))

  (define (parse t)
    (match t with
      (sexp:symbol sym)
      -> (match (alist/lookup base-types sym) with
	   ;; known base type
	   (maybe:yes t) -> t
	   ;; allow nullary predicates
	   (maybe:no) -> (pred sym '()))
      (sexp:list l) -> (parse-list l)
      (sexp:record fl) -> (rproduct (parse-field-list fl))
      _ -> (error1 "bad type" exp)))

  (parse exp)
  )

(define (parse-type exp)
  (parse-type* exp (alist-maker)))

;; I think this is a bug: the moo() in there should be inside the pre(),
;;  otherwise it's a malformed row! [hey, maybe this is where 'kinding' comes in. 8^)]
;;
;; moo(t20093, rproduct(rlabel(level, pre(int), 
;;                      rlabel(key, pre(t908),
;;                      rlabel(val, pre(t909),
;;                      rlabel(left, moo(t20068, pre(t20092)),
;;                      rlabel(right, moo(t20073, pre(t20093)),
;;                      rdefault(abs))))))))

;; rproduct(rlabel(x, pre(bool), rlabel(y, pre(int), rdefault(abs))))
;; => '(x y)
(define get-record-sig
  (type:pred 'moo (tv arg) _)
  -> (get-record-sig arg)
  (type:pred 'rproduct (row) _)
  -> (let ((sig
	    (let loop ((row row)
		       (labels '()))
	      (match row with
;; 		(type:pred 'rlabel ((type:pred label _ _) (type:pred 'pre _ _) rest) _)
;; 		-> (loop rest (list:cons label labels))
;; 		(type:pred 'moo (tv arg) _) -> (loop arg labels)
		(type:pred 'rlabel ((type:pred label _ _) (type:pred 'abs _ _) rest) _)
		-> (loop rest labels)
		(type:pred 'rlabel ((type:pred label _ _) _ rest) _)
		-> (loop rest (list:cons label labels))
		(type:pred 'rdefault _ _)   -> labels
		(type:tvar _ _)		    -> (list:cons '... labels)
		_			    -> '()))))
       (sort symbol<? sig))
  ;; type solver should guarantee this
  x -> (begin (print-string (format "unable to get record sig: " (type-repr x) "\n"))
	      (error1 "bad record sig" x))
  )

;; as an sexp it can be attached to a primapp as a parameter.
(define (get-record-sig-sexp t)
  (let ((sig (get-record-sig t)))
    (sexp:list (map sexp:symbol sig))))


;; (define (test-types)
;;   (let ((t0 (parse-type (car (read-string "((list sexp) -> int)"))))
;; 	(t1 (parse-type (car (read-string "(thing 'a 'b (list 'a) (list 'b))"))))
;; 	(t2 (parse-type (car (read-string "'a"))))
;; 	(t3 (parse-type (car (read-string "{x=int ...}"))))
;; 	(t4 (parse-type (car (read-string "(((continuation 'a) -> 'a) -> 'a)"))))
;; 	)
    
;;     (printn t0)
;;     (print-string (type-repr t0))
;;     (newline)
;;     (print-string (type-repr t1))
;;     (newline)
;;     (print-string (type-repr t2))
;;     (newline)
;;     (print-string (type-repr t3))
;;     (newline)
;;     (print-string (type-repr t4))
;;     (newline)
;;     (printn (get-tvars t1))
;;     ))

;; uncomment to test
;; (include "self/lisp_reader.scm")
;; (include "lib/counter.scm")
;; (include "lib/alist2.scm")
;; (test-types)
