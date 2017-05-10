;; -*- Mode: Irken; coding: utf-8 -*-

(include "lib/set.scm")

(datatype type
  (:tvar int                {parent=(maybe type) pending=bool})
  (:pred symbol (list type) {parent=(maybe type) pending=bool})
  )

(define (pred name subs)
  (type:pred name subs {parent=(maybe:no) pending=#f}))

(define (arrow result-type arg-types)
  (pred 'arrow (list:cons result-type arg-types)))

(define tvar-counter (make-counter 0))

(define (new-tvar)
  (type:tvar (tvar-counter.inc) {parent=(maybe:no) pending=#f}))

(define (is-pred? t name)
  (match t with
    (type:pred name0 _ _)
    -> (eq? name0 name)
    _ -> #f))

(define type<? magic<?)

;; singleton base types
(define int-type        (pred 'int '()))
(define char-type       (pred 'char '()))
(define string-type     (pred 'string '()))
(define undefined-type  (pred 'undefined '()))
(define bool-type       (pred 'bool '()))
(define symbol-type     (pred 'symbol '()))
(define sexp-type       (pred 'sexp '()))

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


;; put a row type into canonical order (sorted by field name).

(define (row-canon* kind row rt)

  (define rlabel<
    (:tuple la _ _) (:tuple lb _ _)
    -> (symbol<? la lb)
    )

  (define (collect row labels)
    (match row with
      (type:pred 'rlabel ((type:pred label _ _) ltype rest) _)
      -> (collect rest (list:cons (:pair label ltype) labels))
      _
      -> (let loop ((result row)
		    (labels (reverse (sort rlabel< labels))))
	   (match labels with
	     () -> result
	     ((:pair label ltype) . tl)
	     -> (loop (rlabel (make-label label) (row-canon ltype) result) tl)
	     ))
      ))

  (type:pred kind (LIST (collect row '())) rt)
  )

(define (row-canon t)
  (match t with
    (type:tvar _ _) -> t
    (type:pred 'rproduct (row) rt)
    -> (row-canon* 'rproduct row rt)
    (type:pred 'rsum (row) rt)
    -> (row-canon* 'rsum row rt)
    (type:pred pred args rt)
    -> (type:pred pred (map row-canon args) rt)
    ))


(define (find-tvars t)
  (let ((tvars (cmap/make magic-cmp)))
    (define recur
      (type:pred sym types _) -> (for-each recur types)
      (type:tvar id _)        -> (begin (cmap/add tvars id) #u))
    (recur t)
    tvars))

(define (type-repr* t pretty?)

  (let ((tvar-cmap (find-tvars t)))

    (define (tvar-repr id)
      (if pretty?
          (format "'" (base26 (cmap->index tvar-cmap id)))
          (format "t" (int id))))

    (define rlabel-repr
      (type:pred label () _) (type:pred 'pre (t) _) -> (format (sym label) "=" (trep t))
      (type:pred label () _) (type:pred 'abs () _)  -> (format (sym label) "=#f")
      (type:pred label () _) x                      -> (format (sym label) "=" (trep x))
      x y -> (error1 "bad row type" (LIST x y))
      )

    (define row-repr
      (type:pred 'rlabel (label type rest) _)         -> (format (rlabel-repr label type) " " (row-repr rest))
      (type:pred 'rdefault ((type:pred 'abs () _)) _) -> ""
      (type:tvar id _)                                -> "..."
      x                                               -> (format "<confused:" (trep x) ">")
      )

    (define trep
      (type:tvar id _)                    -> (tvar-repr id)
      (type:pred 'arrow (rtype atype) _)  -> (format "(" (trep atype) " -> " (trep rtype) ")")
      (type:pred 'arrow (rtype . args) _) -> (format "(" (join trep " " args) " -> " (trep rtype) ")")
      (type:pred 'rproduct (row) _)       -> (format "{" (row-repr row) "}")
      (type:pred 'rsum (row) _)           -> (format "|" (row-repr row) "|")
      (type:pred pred () _)               -> (format (sym pred))
      (type:pred pred args _)             -> (format "(" (sym pred) " " (join trep " " args) ")")
      )
    (trep t)
    ))

(define (base26 num)

  (define (digit n)
    (ascii->char (+ (char->ascii #\a) n)))

  (let loop ((q (/ num 26))
             (r (remainder num 26))
             (acc '()))
    (if (= q 0)
        (list->string (cons (digit r) acc))
        (loop (/ q 26) (remainder q 26) (cons (digit r) acc)))
    ))

(define (type-repr t)
  (type-repr* t #f))

(define (type->sexp* tvar-cmap t)
  (define recur
    (type:tvar index _)
    -> (sexp (sym 'quote) (sym (string->symbol (base26 (cmap->index tvar-cmap index)))))
    (type:pred name subs _)
    -> (sexp:list (cons (sexp:symbol name) (map recur subs)))
    )
  (recur t))

(define (type->sexp t)
  (type->sexp* (find-tvars t) t))

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
      (type:tvar na ra) (type:tvar nb rb)   -> (if (< na nb)
						   (set! rb.parent (maybe:yes pa))
						   (set! ra.parent (maybe:yes pb)))
      (type:pred _ _ ra) (type:pred _ _ rb) -> (set! rb.parent (maybe:yes pa)))))

(define (parse-type* exp tvars)

  (define (gen-tvar)
    (let ((sym (string->symbol (format "r" (int (tvar-counter.inc))))))
      (get-tvar sym)))

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
    ;; without alias check
    ((sexp:symbol p) . rest) -> (pred p (map parse rest))
    ;; ;; with alias check...
    ;; ((sexp:symbol p) . rest)
    ;; -> (match (alist/lookup the-context.aliases p) with
    ;; 	 (maybe:yes alias) -> (apply-alias p alias rest)
    ;; 	 (maybe:no) -> (pred p (map parse rest)))
    ((sexp:cons 'nil pvar) . rest)
    -> (begin
         (printf "pvar in type: (:" (sym pvar) " " (join repr " " rest) ")\n")
         ;; (rsum (rlabel plabel (rpre T0) T1))
         (rsum (rlabel (make-label pvar)
                       (rpre (pred 'product (map parse-type rest)))
                       (rdefault (rabs)))))
    x -> (error1 "malformed predicate" x))

  ;; ;; similarities here with typing.scm/instantiate-type-scheme
  ;; (define (apply-alias name alias pred-args)
  ;;   ;; here we have an alias:sexp, which we can parse into a scheme,
  ;;   ;;  and then substitute pred-args for the tvars in the scheme.
  ;;   (printf "apply-alias: " (sym name) "\n")
  ;;   (let ((tvars (alist-maker))
  ;; 	  (type (parse-type* alias tvars))
  ;; 	  (tvmap (alist-maker))
  ;; 	  )
  ;;     ;; build a map from tvar.id->arg
  ;;     (for-each2
  ;;      (lambda (k v)
  ;; 	 (tvmap::add k (parse-type v)))
  ;;      (reverse (tvars::values)) pred-args)
  ;;     (when the-context.options.debugtyping
  ;; 	    (printf "apply-alias " (repr alias) "\n")
  ;; 	    (printf "         to " (join repr "," pred-args) "\n")
  ;; 	    (printf "parsed type " (type-repr type) "\n")
  ;; 	    (printf "      tvars " (join type-repr " " (tvars::values)) "\n"))
  ;;     ;; walk the type, replacing each member of tvars
  ;;     (let ((moovar (gen-tvar))
  ;; 	    (recursive? #f)
  ;; 	    (result
  ;; 	     (let walk ((t type))
  ;; 	       (match t with
  ;; 		 (type:pred name0 args _)
  ;; 		 -> (begin
  ;; 		      (printf "  pred compare " (sym name0) " = " (sym name) "\n")
  ;; 		      (if (eq? name name0)
  ;; 			  ;; a recursive type alias.  moooo.
  ;; 			  (begin
  ;; 			    (printf "found recursion in type alias\n")
  ;; 			    (set! recursive? #t)
  ;; 			    moovar
  ;; 			    )
  ;; 			  (pred name0 (map walk args)))
  ;; 		      )
  ;; 		 _ -> (match (tvmap::get t) with
  ;; 			(maybe:yes val) -> val
  ;; 			(maybe:no) -> t)))))
  ;; 	(let ((result0
  ;; 	       (if recursive?
  ;; 		   (pred 'moo (LIST moovar result))
  ;; 		   result)))
  ;; 	  (when the-context.options.debugtyping
  ;; 		(printf "result   " (type-repr result0) "\n"))
  ;; 	  result0))
  ;;     ))

  (define parse-list
    ((sexp:symbol 'quote) (sexp:symbol tvar)) -> (get-tvar tvar)
    ((sexp:symbol 'quote) . x)  -> (error1 "malformed type?" x)
    l -> (if (arrow-type? l)
             (parse-arrow-type l)
             (parse-predicate l))
    )

  (define parse-field-list
    ((field:t '... _))  -> (gen-tvar)
    ((field:t name type) . tl) -> (rlabel (make-label name) (rpre (parse type)) (parse-field-list tl))
    () -> (rdefault (rabs)))

  (define (parse t)
    (match t with
      (sexp:symbol sym)
      -> (match (alist/lookup base-types sym) with
	   ;; known base type
	   (maybe:yes t) -> t
	   ;;(maybe:no) -> (pred sym '()))
	   (maybe:no) -> (match (alist/lookup the-context.aliases sym) with
			   ;; textual substitution, nullary predicate leaves
			   ;;   all tvars untouched (thus forced to be equal in each use).
			   (maybe:yes (:scheme gens atype))
			   -> atype ;; (apply-alias sym alias '())
			   ;; allow nullary predicates
			   (maybe:no) -> (pred sym '())))
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
  (type:pred 'rproduct (row) _)
  -> (let ((sig
            (let loop ((row row)
                       (labels '()))
              (match row with
                (type:pred 'rlabel ((type:pred label _ _) (type:pred 'abs _ _) rest) _)
                -> (loop rest labels)
                (type:pred 'rlabel ((type:pred label _ _) _ rest) _)
                -> (loop rest (list:cons label labels))
                (type:pred 'rdefault _ _)   -> labels
                (type:tvar _ _)             -> (list:cons '... labels)
                _                           -> '()))))
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
;;      (t1 (parse-type (car (read-string "(thing 'a 'b (list 'a) (list 'b))"))))
;;      (t2 (parse-type (car (read-string "'a"))))
;;      (t3 (parse-type (car (read-string "{x=int ...}"))))
;;      (t4 (parse-type (car (read-string "(((continuation 'a) -> 'a) -> 'a)"))))
;;      (t5 (parse-type (car (read-string "{z=int x=int a=int b=int george=int}"))))
;;      )

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
;;     (printf "t5 0: " (type-repr t5) "\n")
;;     (printf "t5 1: " (type-repr (row-canon t5)) "\n")
;;     (printn (get-tvars t1))
;;     ))
