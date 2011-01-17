;; -*- Mode: Irken; coding: utf-8 -*-

(include "lib/set.scm")

(datatype type
  (:base symbol)
  (:tvar symbol)
  (:pred symbol (list type))
;; maybe leave this for the typing module.
;;  (:forall (list symbol) type)
  )

(define (arrow result-type arg-types)
  (type:pred 'arrow (list:cons result-type arg-types)))

(define base-types
  (alist/make
   ('int	   (type:base 'int))
   ('char	   (type:base 'char))
   ('string	   (type:base 'string))
   ('undefined	   (type:base 'undefined))
   ('unit	   (type:base 'unit))
   ('continutation (type:base 'continutation))
   ))

(define print-type
  (type:base name)	-> (print name)
  (type:tvar sym)	-> (begin (print-string "'") (print sym))
  ;; XXX special case the arrow predicate
  (type:pred pred ())	-> (print pred)
  (type:pred pred args) -> (begin
			     (print pred)
			     (print-string "(")
			     (print-sep print-type ", " args)
			     (print-string ")")
			     #u
			     )
;;   (type:forall vars t)    -> (begin
;; 			     (print "∀")
;; 			     (print-sep print "," vars)
;; 			     (print-string ".")
;; 			     (print-type t)
;; 			     )
  )

(define type-repr
  (type:base name)                  -> (format (sym name))
  (type:tvar name)                  -> (format "'" (sym name))
  (type:pred 'arrow (rtype . args)) -> (format "(" (join type-repr ", " args) ")->" (type-repr rtype))
  (type:pred pred ())               -> (format (sym pred))
  (type:pred pred args)             -> (format (sym pred) "(" (join type-repr ", " args) ")")
  )

(define (get-tvars t)
  (let ((tvars (make-set '() eq?)))
    (define recur
      (type:base _) -> #u
      (type:tvar tv) -> (tvars.add tv)
      (type:pred sym types) -> (for-each recur types))
    (recur t)
    (tvars.get)))

(define (parse-type type)

  (let ((tvars (alist:nil)))

    (define (get-tvar tv)
      (match (alist/lookup tvars tv) with
	(maybe:yes tv) -> tv
	(maybe:no) -> (let ((r (type:tvar tv)))
			(alist/push tvars tv r)
			r)))

    (define arrow-type?
      ()                      -> #f
      ((sexp:symbol '->) . _) -> #t
      (_ . tl)                -> (arrow-type? tl)
      )
  
    (define (parse-arrow-type t)
      (let loop ((args '()) (t t))
	(match t with
	  ((sexp:symbol '->) result-type) -> (arrow (parse-type result-type) args)
	  (arg . rest) -> (loop (list:cons (parse-type arg) args) rest)
	  () -> (impossible)
	  )))
  
    (define parse-predicate
      ((sexp:symbol pred) . rest) -> (type:pred pred (map parse-type rest))
      x -> (error1 "malformed predicate" x))

    (define parse-list
      ((sexp:symbol 'quote) (sexp:symbol tvar)) -> (get-tvar tvar)
      ((sexp:symbol 'quote) . x)  -> (error1 "malformed type?" x)
      l -> (if (arrow-type? l)
	       (parse-arrow-type l)
	       (parse-predicate l))
      )

    (match type with
      (sexp:symbol sym)
      -> (match (alist/lookup base-types sym) with
	   ;; known base type
	   (maybe:yes t) -> t
	   ;; allow nullary predicates
	   (maybe:no) -> (type:pred sym '()))
      (sexp:list l) -> (parse-list l)
      _ -> (error1 "bad type" type))
  
    ))

(define (test-types)
  (print-type (parse-type (car (read-string "((list sexp) -> int)"))))
  (newline)
  (printn (get-tvars (parse-type (car (read-string "(thing 'a 'b (list 'a) (list 'b))")))))
  )

;(test-types)
