;; -*- Mode: Irken; coding: utf-8 -*-

(include "lib/set.scm")

;; re-doing this now for Huet's algorithm.

;; consider rank?

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
   ('bool bool-type)
   ('symbol symbol-type)
   ))

;; row types
(define (rproduct tl) (pred 'rproduct tl))
(define (rsum row) (pred 'rsum (LIST row)))
(define (rdefault arg) (pred 'rdefault (LIST arg)))
(define (rlabel name type rest) (pred 'rlabel (LIST name type rest)))

(define type-repr
  (type:tvar id _)                    -> (format "t" (int id))
  (type:pred 'arrow (rtype atype) _)  -> (format (type-repr atype) "->" (type-repr rtype))
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
  (print-string "parse-type*:\n")
  (pp 1 exp) (newline)
  (printn tvars)
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
    (let loop ((args '()) (t t))
      (match t with
	((sexp:symbol '->) result-type) -> (arrow (parse result-type) args)
	(arg . rest) -> (loop (list:cons (parse arg) args) rest)
	() -> (impossible)
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

  (define (parse t)
    (match t with
      (sexp:symbol sym)
      -> (match (alist/lookup base-types sym) with
	   ;; known base type
	   (maybe:yes t) -> t
	   ;; allow nullary predicates
	   (maybe:no) -> (pred sym '()))
      (sexp:list l) -> (parse-list l)
      _ -> (error1 "bad type" exp)))

  (parse exp)
  )

(define (parse-type exp)
  (print-string (format "parsing type: " (p repr exp) "\n"))
  (parse-type* exp (alist-maker)))

(define (test-types)
  (let ((t0 (parse-type (car (read-string "((list sexp) -> int)"))))
	(t1 (parse-type (car (read-string "(thing 'a 'b (list 'a) (list 'b))"))))
	(t2 (parse-type (car (read-string "'a"))))
	)
    
    (printn t0)
    (print-string (type-repr t0))
    (newline)
    (print-string (type-repr t1))
    (newline)
    (print-string (type-repr t2))
    (newline)
    (printn (get-tvars t1))
    ))

;; uncomment to test
;; (include "self/lisp_reader.scm")
;; (include "lib/counter.scm")
;; (include "lib/alist2.scm")
;; (test-types)
