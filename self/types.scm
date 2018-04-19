;; -*- Mode: Irken; coding: utf-8 -*-

(typealias typerec {parent=(maybe type) pending=bool})

(datatype type
  (:tvar int                typerec)
  (:pred symbol (list type) typerec)
  )

(define (pred name subs)
  (type:pred name subs {parent=(maybe:no) pending=#f}))

(define (pred1 name)
  (pred name '()))

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

(define cint-types
  (alist/make
   ('char      (pred1 'char))
   ('uchar     (pred1 'uchar))
   ('short     (pred1 'short))
   ('ushort    (pred1 'ushort))
   ('int       (pred1 'int))
   ('uint      (pred1 'uint))
   ('long      (pred1 'long))
   ('ulong     (pred1 'ulong))
   ('longlong  (pred1 'longlong))
   ('ulonglong (pred1 'ulonglong))
   ('i8        (pred1 'i8))
   ('u8        (pred1 'u8))
   ('i16       (pred1 'i16))
   ('u16       (pred1 'u16))
   ('i32       (pred1 'i32))
   ('u32       (pred1 'u32))
   ('i64       (pred1 'i64))
   ('u64       (pred1 'u64))
   ('i128      (pred1 'i128))
   ('u128      (pred1 'u128))
   ('i256      (pred1 'i256))
   ('u256      (pred1 'u256))
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
      (type:tvar id _)                                -> (format "...t" (int id))
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
    ;; ;; without alias check
    ;; ((sexp:symbol p) . rest) -> (pred p (map parse rest))
    ;; alias check...
    ((sexp:symbol p) . rest)
    -> (match (alist/lookup the-context.aliases p) with
    	 (maybe:yes scheme) -> (apply-alias p scheme rest)
    	 (maybe:no) -> (pred p (map parse rest)))
    ((sexp:cons 'nil pvar) . rest)
    -> (begin
         (printf "pvar in type: (:" (sym pvar) " " (join repr " " rest) ")\n")
         ;; (rsum (rlabel plabel (rpre T0) T1))
         (rsum (rlabel (make-label pvar)
                       (rpre (pred 'product (map parse-type rest)))
                       (rdefault (rabs)))))
    x -> (error1 "malformed predicate" x))

  (define (apply-scheme scheme type-args)
    (match scheme with
      (:scheme gens type)
      -> (if (not (= (length type-args) (length gens)))
             (begin
               (printf "apply-scheme: arity mismatch in type scheme:\n")
               (printf "  predicate args = " (join type-repr ", " type-args) "\n")
               (printf "      alias args = " (join type-repr ", " gens) "\n")
               (error "apply-scheme: arity mistmatch"))
             (let ((tmap (alist:nil)))
               ;; build map from tvar => predicate arg.
               (for-range i (length gens)
                 (match (nth gens i) with
                   (type:tvar id _)
                   -> (alist/push tmap id (nth type-args i))
                   _ -> (impossible)))
               ;; walk the type, replacing.
               (let walk ((t type))
                 (match t with
                   (type:pred name args _) -> (pred name (map walk args))
                   (type:tvar id _) -> (match (alist/lookup tmap id) with
                                         (maybe:yes type-arg) -> type-arg
                                         (maybe:no) -> t)
                   )))
             )))

  (define (apply-alias name scheme pred-args)
    (apply-scheme scheme (map parse-type pred-args)))

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
			   -> atype
			   ;; allow nullary predicates
			   (maybe:no) -> (pred sym '())))
      (sexp:list l) -> (parse-list l)
      (sexp:record fl) -> (rproduct (parse-field-list fl))
      _ -> (error1 "bad type" exp)))

  (parse exp)
  )

(define (parse-type exp)
  (let ((result (parse-type* exp (alist-maker))))
    (when the-context.options.debugtyping
      (printf "parse-type: sexp = " (repr exp) "\n")
      (printf "          parsed = " (type-repr result) "\n"))
    result))

(define (parse-cexp-sig sig)
  (let ((generic-tvars (alist-maker))
        (result (parse-type* sig generic-tvars)))
    (:scheme (generic-tvars::values) result)))

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

(define c-int-types
  ;; XXX distinguish between signed and unsigned!
  ;; XXX also need to handle 64-bit types on a 32-bit platform.
  '(uint8_t uint16_t uint32_t uint64_t
    int8_t int16_t int32_t int64_t
    uint long ulong longlong ulonglong
    ))

(define (int-ctype->itype cint signed?)
  (pred
   (match cint signed? with
     (cint:char)  #t     -> 'char
     (cint:char)  #f     -> 'uchar
     (cint:int)   #t     -> 'int
     (cint:int)   #f     -> 'uint
     (cint:short) #t     -> 'short
     (cint:short) #f     -> 'ushort
     (cint:long)  #t     -> 'long
     (cint:long)  #f     -> 'ulong
     (cint:longlong) #t  -> 'longlong
     (cint:longlong) #f  -> 'ulonglong
     (cint:width w) _ -> (string->symbol (format (if signed? "i" "u") (int (* w 8))))
     _ _              -> (error1 "unsupported cint type"
                                 (format "cint=" (cint-repr cint signed?)
                                         " signed=" (bool signed?)))
     )
   '()))

(define ctype->irken-type*
  (ctype:name name)  -> (pred name '())
  (ctype:int cint s) -> (int-ctype->itype cint s)
  (ctype:array _ t)  -> (pred 'array (LIST (ctype->irken-type* t)))
  (ctype:pointer t)  -> (pred '* (LIST (ctype->irken-type* t)))
  (ctype:struct n)   -> (pred 'struct (LIST (pred n '())))
  (ctype:union n)    -> (pred 'union (LIST (pred n '())))
  )

;; we want only the outermost layer of 'pointer' converted to cref,
;;  leave the internal types alone.
(define ctype->irken-type
  (ctype:pointer t)  -> (pred 'cref (LIST (ctype->irken-type* t)))
  (ctype:array _ t)  -> (pred 'cref (LIST (ctype->irken-type* t)))
  t                  -> (ctype->irken-type* t)
  )

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
