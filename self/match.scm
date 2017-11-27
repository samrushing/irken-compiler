;; -*- Mode: Irken -*-


;; See "The Implementation of Functional Programming Languages",
;; Chapter 5: "Efficient Compilation of Pattern-Matching".
;; http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/
;;
;; Thanks for the hint, OCaml people! (Xavier Leroy?) They were kind
;;   enough to put this reference in their source code
;;   (ocaml/bytecomp/matching.ml), otherwise I may have never found
;;   out about this book.  And thanks to Simon Peyton-Jones for
;;   putting his book online.

(datatype fieldpair
  (:t symbol pattern)
  )

(define fieldpair-repr
  (fieldpair:t name pat)
  -> (format (sym name) "=" (pattern-repr pat)))

(datatype pattern
  (:literal sexp)
  (:variable symbol)
  (:constructor symbol symbol (list pattern))
  (:record (list fieldpair) bool) ;; fields open-record?
  )

(define pattern-repr
  (pattern:literal exp) -> (repr exp)
  (pattern:variable sym) -> (symbol->string sym)
  (pattern:constructor dt alt subs)
  -> (format "(" (sym dt) ":" (sym alt) " " (join pattern-repr " " subs) ")")
  (pattern:record pairs open?)
  -> (format "{" (join fieldpair-repr " " pairs) (if open? "..." "") "}")
  )

(datatype rule
  (:t (list pattern) sexp))

(define rule-repr
  (rule:t pats exp)
  -> (format (join pattern-repr " " pats) " -> " (repr exp))
  )

(define rule->code (rule:t _ code) -> code)
(define rule->pats (rule:t pats _) -> pats)

(define match-error (sexp (sexp:symbol '%match-error) (sexp:bool #f)))
(define match-fail  (sexp (sexp:symbol '%fail) (sexp:bool #f)))

(define match-counter (make-counter 0))
(define (new-match-var)
  (string->symbol (format "m" (int (match-counter.inc)))))

(define (compile-pattern expander vars exp)

  (define (parse-pattern exp)

    (define parse-record-fields
      acc ()                        -> (pattern:record acc #f)
      acc ((field:t '... _))        -> (pattern:record acc #t)
      acc ((field:t '... _) . tl)   -> (error1 "malformed record pattern" (format "... " (join repr-field " " tl) "}"))
      acc ((field:t name pat) . tl) -> (parse-record-fields (list:cons (fieldpair:t name (kind pat)) acc) tl)
      )

    (define kind
      (sexp:symbol s)	   -> (pattern:variable s)
      (sexp:record fields) -> (parse-record-fields '() fields)
      (sexp:bool b)	   -> (pattern:constructor 'bool (if b 'true 'false) '())
      (sexp:list l)
      -> (match l with
	   () -> (pattern:constructor 'list 'nil '())
	   ((sexp:symbol 'quote) (sexp:symbol s)) -> (pattern:literal (sexp:symbol s))
	   ((sexp:cons dt alt) . args) -> (pattern:constructor dt alt (map kind args))
	   ((sexp:symbol '.) last) -> (kind last)
	   (hd . tl) -> (pattern:constructor 'list 'cons (LIST (kind hd) (kind (sexp:list tl))))
	   _ -> (error1 "malformed pattern" (format (join repr " " l))))
      x -> (pattern:literal x))
    (kind exp))

  ;; (p0 p1 p2 -> r0 ...)
  (define (parse-match expander body)
    (let loop ((patterns '())
	       (rules '())
	       (l body))
      (match l with
	() -> (reverse rules)
	((sexp:symbol '->) code . tl)
	-> (loop '() (list:cons (rule:t (reverse patterns) (expander code)) rules) tl)
	(pat . tl)
	-> (loop (list:cons (parse-pattern pat) patterns) rules tl))))

  (define pattern->kind
    (pattern:literal _)		 -> 'literal
    (pattern:variable _)	 -> 'variable
    (pattern:constructor _ _ _ ) -> 'constructor
    (pattern:record _ _)         -> 'record
    )

  ;; pull the first pattern out of each rule
  (define remove-first-pat
    (rule:t (pat . pats) code)
    -> (rule:t pats code)
    _ -> (error "remove-first-pat: empty pats?"))

  (define first-pattern-kind
    (rule:t (pat0 . pats) _) -> (pattern->kind pat0)
    x -> (error1 "empty pattern list?" x))
  
  (define (compare-first-patterns a b)
    (eq? (first-pattern-kind a)
	 (first-pattern-kind b)))
  
  (define (compile-match vars rules default)
    (match vars rules with
      ;; the 'empty rule'
      () ()         -> default
      () (rule . _) -> (rule->code rule)
      _ _ ->
      ;; group the rules by kind of first pattern
      (let ((groups (pack rules compare-first-patterns)))
	(if (= (length groups) 1)
	    ;; one of the standard rules
	    (compile-group vars (car groups) default)
	    (begin
              ;; mixture rule
              ;; the python code iteratively calls pop(), which
              ;;   is equivalent to iterating over it in reverse.
              (for-list group (reverse groups)
                (set! default (compile-group vars group default)))
	      default)
	    ))))
    
  ;; we know the rules are of identical kind
  (define (compile-group vars rules default)
    (match (first-pattern-kind (car rules)) with
      'literal     -> (constant-rule vars rules default)
      'variable    -> (variable-rule vars rules default)
      'constructor -> (constructor-rule vars rules default)
      'record      -> (record-rule vars rules default)
      _            -> (impossible)
      ))

  (define (fatbar e1 e2)
    (cond ((eq? e1 match-fail) e2)
	  ((eq? e2 match-fail) e1)
	  (else
	   (sexp1 '%fatbar (LIST (sexp:bool #f) e1 e2)))))

  (define (subst var0 pat code)
    (match pat with
      (pattern:variable var1)
      ;; record a subst to be applied during node building (unless it's a wildcard pattern)
      -> (if (not (eq? var1 '_))
	     (sexp (sexp:symbol 'let-subst)
		   (sexp (sexp:symbol var1)
			 (sexp:symbol var0)) code)
	     code)
      _ -> (impossible)
      ))
  
  ;; if every rule begins with a variable, we can remove that column
  ;;  from the set of patterns and substitute the var within each body
  (define (variable-rule vars rules default)
    (let ((var0 (car vars))
	  (rules0 (map (lambda (rule)
			 (match rule with
			   (rule:t pats code)
			   -> (rule:t (cdr pats) (subst var0 (car pats) code))))
		       rules)))
      (compile-match (cdr vars) rules0 default)))

  (define fieldpair->label
    (fieldpair:t label _) -> label)

  (define fieldpair->pattern
    (fieldpair:t _ pattern) -> pattern)

  (define pattern->fieldpairs
    (pattern:record fields _) -> fields
    _ -> (error "not a record pattern"))

  (define (pattern->record-sig p)
    (map fieldpair->label (pattern->fieldpairs p)))

  (define (equal-sigs? a b)
    (every2? eq? a b))

  (define (record-rule vars rules default)
    ;; first - sanity check, make sure each sig matches.
    (let ((sig0 (pattern->record-sig (car (rule->pats (car rules))))))
      (for-list rule (cdr rules)
        (when (not (equal-sigs? sig0 (pattern->record-sig (car (rule->pats rule)))))
          (printf "record pattern signature mismatch:\n\t" (join rule-repr "\n\t" rules))
          (error "record pattern mismatch")))
      ;; translate
      (let ((var0 (nth vars 0))
	    (vars0 (map (lambda (field) (string->symbol (format (sym var0) "_" (sym field)))) sig0))
	    (rules0
	     (map (lambda (rule)
		    (let ((pats (rule->pats rule))
			  (pats0 (map fieldpair->pattern (pattern->fieldpairs (car pats)))))
		      (rule:t (append pats0 (cdr pats)) (rule->code rule))))
		  rules))
	    (bindings
	     (map-range
		 i (length vars0)
		 (sexp (sexp:symbol (nth vars0 i))
		       (sexp:attr (sexp:symbol var0) (nth sig0 i))))))
	(sexp (sexp:symbol 'let)
	      (sexp:list bindings)
	      (compile-match (append vars0 (cdr vars))
			     rules0
			     default)))))

  (define pattern->literal
    (pattern:literal exp) -> exp
    _ -> (error "not a literal pattern"))
  
  (define (first-literal=? r0 r1)
    (match r0 r1 with
      (rule:t pats0 _) (rule:t pats1 _)
      -> (sexp=? (pattern->literal (car pats0))
		 (pattern->literal (car pats1)))))
  
  (define (constant-rule vars rules default0)
    ;; group runs of the same literal together
    (let loop ((groups (pack rules first-literal=?))
	       (default default0))
      (match groups with
	() -> default
	(rules0 . groups)
	-> (let ((lit (pattern->literal (car (rule->pats (car rules0)))))
		 (comp-fun
		  (match lit with
		    (sexp:string _) -> (sexp:symbol 'string=?)
		    _ -> (sexp:symbol 'eq?))))
	     (loop groups
		   (fatbar (sexp (sexp:symbol 'if)
				 (sexp comp-fun (sexp:symbol (car vars)) (sexp1 'quote (LIST lit)))
				 (compile-match (cdr vars) (map remove-first-pat rules0) match-fail)
				 match-fail)
			   default))))))

  ;; sort a collection <l> into lists with matching <p>
  ;; <p> must return an eq?-compatible object.  returns an alist of stacks.
  (define (collect p l)
    (let loop ((acc (alist/make))
	       (l l))
      (match l with
	() -> acc
	(hd . tl)
	-> (let ((key (p hd)))
	     (match (alist/lookup acc key) with
	       (maybe:no) -> (let ((stack (make-stack)))
			       (stack.push hd)
			       (loop (alist:entry key stack acc) tl))
	       (maybe:yes stack) -> (begin (stack.push hd) (loop acc tl)))))))

  (define pattern->dt
    (pattern:constructor dt _ _) -> dt
    _ -> (error "not a constructor pattern"))

  (define pattern->alt
    (pattern:constructor _ alt _) -> alt
    _ -> (error "not a constructor pattern"))
  
  (define pattern->subs
    (pattern:constructor _ _ subs) -> subs
    _ -> (error "not a constructor pattern"))

  (define rule->constructor-dt
    (rule:t pats _)
    -> (pattern->dt (car pats)))
  
  (define rule->constructor-alt
    (rule:t pats _)
    -> (pattern->alt (car pats)))

  (define (sort-constructor-rules rules)
    ;; first, make sure we're all on the same datatype
    (let ((by-dt (collect rule->constructor-dt rules))
	  (keys (alist->keys by-dt)))
      (if (not (= (length keys) 1))
	  (error1 "more than one datatype in pattern match" keys)
	  (collect rule->constructor-alt rules))))

  ;; this handles normal constructors *and* polymorphic variants.
  (define (constructor-rule vars rules default)
    (let ((dtname (rule->constructor-dt (car rules)))
	  (alts (sort-constructor-rules rules))
	  (nalts 0)
	  (mdt (alist/lookup the-context.datatypes
			 (rule->constructor-dt (car rules))))
	  (default0 (if (sexp=? default match-error) default match-fail))
	  (cases '())
	  )
      (alist/iterate
       (lambda (tag rules-stack)
	 (let ((arity (match mdt with
			(maybe:no) -> (length (pattern->subs (car (rule->pats (rules-stack.top)))))
			(maybe:yes dt) -> (let ((alt (dt.get tag)))
					    alt.arity)))
	       (vars0 (nthunk arity new-match-var))
	       (wild (make-vector arity #t))
	       (rules1 '()))
	   (set! nalts (+ nalts 1))
	   (define frob-rule
	     (rule:t pats code)
	     -> (let ((subs (pattern->subs (car pats))))
		  (when (not (= (length subs) arity))
                    (printf "arity mismatch in constructor pattern:\n\t" (rule-repr (rule:t pats code)))
                    (error "arity mismatch in constructor pattern"))
		  (PUSH rules1 (rule:t (append (pattern->subs (car pats)) (cdr pats)) code))
		  (for-range i arity
                    (match (nth subs i) with
                      (pattern:variable '_) -> #u
                      _ -> (set! wild[i] #f))
                    )))
	   (for-each frob-rule (rules-stack.get))
	   ;; if every pattern has a wildcard for this arg of the constructor,
	   ;;  then use '_' rather than the symbol we generated.
	   (let ((vars1 (map-range i arity (if wild[i] '_ (nth vars0 i)))))
	     (PUSH cases
		   ;; ((:tag var0 var1 ...) (match ...))
		   (sexp
		    (sexp:list
		     (list:cons (sexp:cons 'nil tag) (map sexp:symbol vars1)))
		    ;; we don't reverse rules1 because we popped it off a reversed stack already
		    (compile-match (append vars0 (cdr vars)) rules1 default0))))))
       alts)
      (let ((result
	     (match mdt with
	       (maybe:yes dt)
	       -> (begin (if (< nalts (dt.get-nalts))
			     (PUSH cases (sexp (sexp:symbol 'else) default0)))
			 (sexp:list (append (LIST (sexp:symbol 'vcase) (sexp:symbol dt.name) (sexp:symbol (car vars)))
					    (reverse cases))))
	       (maybe:no)
	       -> (begin (if (not (eq? default match-error))
			     (PUSH cases (sexp (sexp:symbol 'else) match-fail)))
			 (sexp:list (append (LIST (sexp:symbol 'vcase) (sexp:symbol (car vars)))
					    (reverse cases)))))
	     ))
	(if (not (eq? default match-error))
	    (fatbar result default)
	    result))))

  (define nthunk
    0 p -> '()
    n p -> (list:cons (p) (nthunk (- n 1) p))
    )

  (define (arity-check rules)
    (let ((lengths (map (lambda (rule) (length (rule->pats rule))) rules))
          (arity (car lengths)))
      (when (not (all (lambda (x) (= x arity)) (cdr lengths)))
        (printf "arity mismatch in patterns:\n\t"
                (join rule-repr "\n\t" rules))
        (error "arity mismatch in patterns"))
      arity))

  (let ((rules (parse-match expander exp)))
    (arity-check rules)
    (let ((npats (arity-check rules))
	  (vars (if (null? vars)
		    (nthunk npats new-match-var)
		    vars))
	  (result (compile-match vars rules match-error)))
      (:tuple vars result)))
  )
