;; -*- Mode: Irken -*-

;; based on my python translation of Dorai Sitaram's common lisp implementation
;;   of scheme's <syntax-rules>.
;;
;; see http://www.ccs.neu.edu/home/dorai/mbe/mbe-imps.html
;;     http://www.ccs.neu.edu/home/dorai/mbe/mbe-lsp.html
;;
;; XXX add back in the ability to specify keywords
;; XXX look at the various scheme versions that do hygiene

;; for macros, s-expressions are mostly lists & symbols.

;; a useful pattern when matching against s-expressions
;;  is to have a helper function for matching against
;;  lists.

(define matches-list?
  ;; empty lists match
  () ()
  -> #t
  ;; ellipsis pattern
  (p0 (sexp:symbol '...)) el
  -> (every (lambda (x) (matches-pattern? p0 x)) el)
  ;; any other list
  (p0 . pl) (e0 . el)
  -> (and (matches-pattern? p0 e0)
	  (matches-list? pl el))
  _ _ -> #f
  )

(define matches-pattern?
  ;; symbol pattern matches anything
  (sexp:symbol _) _		-> #t
  ;; list pattern
  (sexp:list pl) (sexp:list el) -> (matches-list? pl el)
  ;; now what other objects should we support in patterns?
  ;; this is probably incorrect, we really need an equal? function
  p e				-> (eq? p e)
  )

;; Very confusing.  With *no* ellipses, this returns a flat list of
;;  every symbol.  If an ellipsis is present, the symbols that it
;;  'repeats' are embedded in a list to a depth equal to the level of
;;  ellipsis nesting.  Yeah.  Theoretically this could be replaced
;;  with a list of symbol/depth pairs, but then I'd have to understand
;;  what the callers are doing as well. 8^)
;;
;; examples:
;; (a b c d) => (a b c d)
;; (a (b c) d) => (a b c d)
;; (a b c ...) => (a b (c))
;; (a b (c ...) ...) => (a b ((c)))


(define (get-ellipsis-nestings p) ;; (sexp) -> (list sexp)
  (define dolist ;; (list sexp) -> (list sexp)
    (p0 (sexp:symbol '...)) -> (LIST (sexp:list (get-ellipsis-nestings p0)))
    (hd . tl)		    -> (append (get-ellipsis-nestings hd) (dolist tl))
    _			    -> '())
  (match p with
    (sexp:list pl)    -> (dolist pl)
    (sexp:symbol sym) -> (LIST p)
    _		      -> '()))

(define intersect?
  (sexp:symbol v) (sexp:symbol y) -> (eq? v y)
  (sexp:list vl)  (sexp:list yl)
  -> (some (lambda (vi)
	     (some (lambda (yj)
		     (intersect? vi yj))
		   yl))
	   vl)
  _ _ -> #f
  )

(define ellipsis-sub-envs ;; (list sexp), (list sexp) -> sexp
  nestings ()                       -> (sexp:list '())
  nestings ((sexp:list (k v)) . tl) -> (if (intersect? (sexp:list nestings) k)
					   v
					   (ellipsis-sub-envs nestings tl))
  _ _ -> (error "unexpected args to ellipsis-sub-envs")
  )

;; get-bindings (sexp, sexp) -> (list sexp)
;; returns the bindings in the form of embedded sets of sexp's.
;; at first glance I thought it could be an alist, but the levels
;; of embedding preclude that.

;; p = (((x y) ...) ...)
;; e = (((1 2) (3 4)) ((5 6) (7 8)))
;; =>  ((((x y)) ((((x y) (((x 1) (y 2)) ((x 3) (y 4)))))
;;                (((x y) (((x 5) (y 6)) ((x 7) (y 8))))))))

(define (get-bindings p e) ;; -> (list sexp)
  (define dolist
    (p (sexp:symbol '...)) e
    -> (LIST (sexp (sexp:list (get-ellipsis-nestings p))
		   (sexp:list (map (lambda (ei)
				      (sexp:list (get-bindings p ei)))
				    e))))
    (hdp . tlp) (hde . tle)
    -> (append (get-bindings hdp hde)
	       (dolist tlp tle))
    _ _ -> '()
    )
  (match p e with 
     (sexp:symbol k) e           -> (LIST (sexp p e))
     (sexp:list p) (sexp:list e) -> (dolist p e)
     _ _ -> '()
     )
  )

;; look up a binding
(define (mbe/assoc k0 r)
  (let loop ((r r))
    (match r with
      () -> (maybe:no)
      ((sexp:list ((sexp:symbol k1) v1)) . tl)
      -> (if (eq? k0 k1)
	     (maybe:yes v1)
	     (loop tl))
      (hd . tl) -> (loop tl)
      )))

(define (expand-pattern p r) ;; sexp, (list sexp) -> sexp
  (match p with
    (sexp:list pl)    -> (sexp:list (expand-list pl r))
    (sexp:symbol sym) -> (match (mbe/assoc sym r) with
			    (maybe:yes v) -> v
			    (maybe:no)    -> p)
    x                 -> x
    ))

(define (expand-list p r) ;; (list sexp), (list sexp) -> (list sexp)
  (define sexp/append
    (sexp:list al) bl -> (append al bl)
    _ _ -> (error "expected list"))
  (match p with
    (p (sexp:symbol '...) . tl)
    -> (let ((nestings (get-ellipsis-nestings p))
	     (rr0 (ellipsis-sub-envs nestings r))
	     ;; rr0 is always a list of lists
	     (rr1 (match rr0 with
		    (sexp:list rrl) -> (map
					(lambda (ri)
					  ;; so ri is always a list
					  (expand-pattern p (sexp/append ri r)))
					rrl)
		    x -> (error1 "expected sexp:list" x)
		    )))
	 (append rr1 (expand-list tl r)))
    (hd . tl)
    -> (list:cons (expand-pattern hd r) (expand-list tl r))
    p -> p
    )
  )

(define (make-macro name patterns)
  (define (apply exp)
    (let loop ((l patterns))
      (match l with
	((:pair in-pat out-pat) . tl)
	-> (if (matches-pattern? in-pat exp)
	       (let ((r (expand-pattern out-pat (get-bindings in-pat exp))))
		 (print-string "expanded => ") (unread r) (newline)
		 r)
	       (loop tl))
	()
	-> (error1 "no matching clause for macro" name))))
  (define (unread-macro)
    (print-string "(macro <")
    (print name)
    (print-string ">\n\t")
    (for-each
     (lambda (x)
       (match x with
	 (:pair in-pat out-pat)
	 -> (begin (unread in-pat) (print-string " ") (unread out-pat) (print-string "\n\t"))))
     patterns)
    (print-string ")\n"))
  { name     = name
    patterns = patterns
    apply    = apply
    unread   = unread-macro
    }
  )
       
(define (test-mbe)
  (let ((tests (read-file "/tmp/b.scm")))
    (for-each
     (lambda (x)
       (match x with
	 (sexp:list (inp outp e))
	 -> (let ((macro (make-macro 'test (LIST (:pair inp outp)))))
	      (printn macro)
	      (unread e)
	      (newline)
	      (unread (macro.apply e))
	      (newline)
	      0)
	 _ -> 0
	 ))
     tests))
  )

;;(test-mbe)
