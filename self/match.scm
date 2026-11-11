;; -*- Mode: Irken -*-

;; XXX get rid of this
(require "lib/stack.scm")
(require "lib/counter.scm")

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
  ;;(:vector (list pattern))
  )

(define pattern-repr
  (pattern:literal exp) -> (repr exp)
  (pattern:variable sym) -> (symbol->string sym)
  (pattern:constructor dt alt subs)
  -> (format "(" (sym dt) ":" (sym alt) " " (join pattern-repr " " subs) ")")
  (pattern:record pairs open?)
  -> (format "{" (join fieldpair-repr " " pairs) (if open? "..." "") "}")
  ;; (pattern:vector subs)
  ;; -> (format "#(" (join pattern-repr " " subs) ")")
  )

;; patterns -> code, and where the rule is (at its first pattern)
(datatype rule
  (:t (list pattern) sexp src))

(define rule-repr
  (rule:t pats exp _)
  -> (format (join pattern-repr " " pats) " -> " (repr exp))
  )

(define rule->code (rule:t _ code _) -> code)
(define rule->pats (rule:t pats _ _) -> pats)
(define rule->src  (rule:t _ _ src) -> src)

(define match-error (sexp (sym '%match-error) (sexp:bool #f null-src)))
(define match-fail  (sexp (sym '%fail) (sexp:bool #f null-src)))

(define match-counter (make-counter 0))
(define (new-match-var)
  (string->symbol (format "m" (int (match-counter.inc)))))

(define (compile-pattern expander vars exp)

  (define (m-error msg)
    (raise (:MatchCompilerError msg)))

  (define (m-error1 m1 m2)
    (raise (:MatchCompilerError (format m1 " : " m2))))

  ;; a mistake in the user's patterns (reported by the transformer).
  (define (rule-error rule msg)
    (raise (:CompileError msg (rule->src rule))))

  (define (parse-pattern exp)

    (define parse-record-fields
      acc ()                        -> (pattern:record acc #f)
      acc ((field:t '... _ _))        -> (pattern:record acc #t)
      acc ((field:t '... _ _) . tl)   -> (m-error1 "malformed record pattern" (format "... " (join repr-field " " tl) "}"))
      acc ((field:t name pat src) . tl)
      -> (if (any? (lambda (fp) (eq? name (fieldpair->label fp))) acc)
             (raise (:CompileError (format "field `" (sym name) "` appears twice in this record pattern") src))
             (parse-record-fields (list:cons (fieldpair:t name (kind pat)) acc) tl))
      )

    (define kind
      (sexp:symbol s _)    -> (pattern:variable s)
      (sexp:record fields _) -> (parse-record-fields '() fields)
      ;;(sexp:vector subs)   -> (pattern:vector (map kind subs))
      ;; we prefer this over pattern:literal because it is a complete datatype.
      (sexp:bool b _)	   -> (pattern:constructor 'bool (if b 'true 'false) '())
      (sexp:list l _)
      -> (match l with
	   () -> (pattern:constructor 'list 'nil '())
	   ((sexp:symbol 'quote _) (sexp:symbol s src)) -> (pattern:literal (sexp:symbol s src))
	   ((sexp:cons dt alt _) . args) -> (pattern:constructor dt alt (map kind args))
	   ((sexp:symbol '. _) last) -> (kind last)
	   (hd . tl) -> (pattern:constructor 'list 'cons (list (kind hd) (kind (sexp:list tl null-src))))
	   ;;_ -> (m-error1 "malformed pattern" (format (join repr " " l)))
           )
      x -> (pattern:literal x))
    (kind exp))

  ;; (p0 p1 p2 -> r0 ...)
  (define (parse-match expander body)
    (let loop ((patterns '())
	       (rules '())
               (src (src:nil))
	       (l body))
      (match l with
	() -> (reverse rules)
	((sexp:symbol '-> asrc) code . tl)
	-> (let ((src0 (match patterns with () -> asrc _ -> src)))
             (loop '() (list:cons (rule:t (reverse patterns) (expander code) src0) rules) (src:nil) tl))
	(pat . tl)
	-> (loop (list:cons (parse-pattern pat) patterns)
                 rules
                 (match patterns with () -> (guess-location pat) _ -> src)
                 tl))))

  (define pattern->kind
    (pattern:literal _)		 -> 'literal
    (pattern:variable _)	 -> 'variable
    (pattern:constructor _ _ _ ) -> 'constructor
    (pattern:record _ _)         -> 'record
    ;;(pattern:vector _)           -> 'vector
    )

  ;; pull the first pattern out of each rule
  (define remove-first-pat
    (rule:t (pat . pats) code src)
    -> (rule:t pats code src)
    _ -> (m-error "remove-first-pat: empty pats?"))

  (define first-pattern-kind
    (rule:t (pat0 . pats) _ _) -> (pattern->kind pat0)
    x -> (m-error "empty pattern list?"))

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
      ;;'vector      -> (vector-rule vars rules default)
      _            -> (impossible)
      ))

  (define (fatbar e1 e2)
    (cond ((eq? e1 match-fail) e2)
	  ((eq? e2 match-fail) e1)
	  (else
	   (sexp1 '%fatbar (list (sexp:bool #f null-src) e1 e2)))))

  (define (subst var0 pat code)
    (match pat with
      (pattern:variable var1)
      ;; record a subst to be applied during node building (unless it's a wildcard pattern)
      -> (if (not (eq? var1 '_))
	     (sexp (sym 'let-subst)
		   (sexp (sym var1) (sym var0))
                   code)
	     code)
      _ -> (impossible)
      ))

  ;; if every rule begins with a variable, we can remove that column
  ;;  from the set of patterns and substitute the var within each body
  (define (variable-rule vars rules default)
    (let ((var0 (car vars))
	  (rules0 (map (lambda (rule)
			 (match rule with
			   (rule:t pats code src)
			   -> (rule:t (cdr pats) (subst var0 (car pats) code) src)))
		       rules)))
      (compile-match (cdr vars) rules0 default)))

  (define fieldpair->label
    (fieldpair:t label _) -> label)

  (define fieldpair->pattern
    (fieldpair:t _ pattern) -> pattern)

  (define pattern->fieldpairs
    (pattern:record fields _) -> fields
    _ -> (m-error "not a record pattern"))

  (define (pattern->record-sig p)
    (map fieldpair->label (pattern->fieldpairs p)))

  (define (equal-sigs? a b)
    (every2? eq? a b))

  (define (record-rule vars rules default)
    ;; first - sanity check, make sure each sig matches.
    (let ((sig0 (pattern->record-sig (car (rule->pats (car rules))))))
      (for-list rule (cdr rules)
        (when (not (equal-sigs? sig0 (pattern->record-sig (car (rule->pats rule)))))
          (rule-error rule (format "this record pattern has fields {" (join symbol->string " " (reverse (pattern->record-sig (car (rule->pats rule)))))
                                   "}, but the first one has {" (join symbol->string " " (reverse sig0)) "}"))))
      ;; translate
      (let ((var0 (nth vars 0))
	    (vars0 (map (lambda (field) (string->symbol (format (sym var0) "_" (sym field)))) sig0))
	    (rules0
	     (map (lambda (rule)
		    (let ((pats (rule->pats rule))
			  (pats0 (map fieldpair->pattern (pattern->fieldpairs (car pats)))))
		      (rule:t (append pats0 (cdr pats)) (rule->code rule) (rule->src rule))))
		  rules))
	    (bindings
	     (map-range
		 i (length vars0)
		 (sexp (sym (nth vars0 i))
		       (sexp:attr (sexp:symbol var0 null-src) (nth sig0 i) null-src)))))
	(sexp (sym 'let)
	      (sexp:list bindings null-src)
	      (compile-match (append vars0 (cdr vars))
			     rules0
			     default)))))

  ;; assuming we only support exact-arity matches, then we'll need
  ;;   to gate each match with an arity match, then recurse to the sub-matches.
  ;; theoretically we could group matches of the same arity?  [no, because they
  ;;   must be ordered as in the match]
  ;; so I think the next step is to figure out how to use fatbar to implement the
  ;;   arity check.
  ;; (define (vector-rule vars rules default)
  ;;   )

  (define pattern->literal
    (pattern:literal exp) -> exp
    _ -> (m-error "not a literal pattern"))

  (define (first-literal=? r0 r1)
    (match r0 r1 with
      (rule:t pats0 _ _) (rule:t pats1 _ _)
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
		    (sexp:string _ _) -> (sexp:symbol 'string=? null-src)
		    _ -> (sexp:symbol 'eq? null-src))))
	     (loop groups
		   (fatbar (sexp (sym 'if)
				 (sexp comp-fun (sym (car vars)) (sexp1 'quote (list lit)))
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
    _ -> (m-error "not a constructor pattern"))

  (define pattern->alt
    (pattern:constructor _ alt _) -> alt
    _ -> (m-error "not a constructor pattern"))

  (define pattern->subs
    (pattern:constructor _ _ subs) -> subs
    _ -> (m-error "not a constructor pattern"))

  (define rule->constructor-dt
    (rule:t pats _ _)
    -> (pattern->dt (car pats)))

  (define rule->constructor-alt
    (rule:t pats _ _)
    -> (pattern->alt (car pats)))

  (define (sort-constructor-rules rules)
    ;; first, make sure we're all on the same datatype
    (let ((by-dt (collect rule->constructor-dt rules))
	  (keys (alist->keys by-dt)))
      (if (not (= (length keys) 1))
          (let ((dt0 (rule->constructor-dt (car rules))))
            (for-list rule rules
              (let ((dt1 (rule->constructor-dt rule)))
                (when (not (eq? dt0 dt1))
                  (rule-error rule (format "this pattern is a `" (sym dt1) "`, but the first one is a `" (sym dt0) "`")))))
            (impossible))
	  (collect rule->constructor-alt rules))))

  ;; this handles normal constructors *and* polymorphic variants.
  (define (constructor-rule vars rules default)
    (let ((dtname (rule->constructor-dt (car rules)))
	  (alts (sort-constructor-rules rules))
	  (nalts 0)
	  (mdt (alist/lookup the-context.datatypes (rule->constructor-dt (car rules))))
	  (default0 (if (sexp=? default match-error) default match-fail))
	  (cases '())
	  )
      (for-alist tag rules-stack alts
        (let ((arity (match mdt with
                       (maybe:no) -> (length (pattern->subs (car (rule->pats (rules-stack.top)))))
                       (maybe:yes dt)
                       -> (if (member-eq? tag (dt.get-alt-names))
                              (let ((alt (dt.get tag)))
                                alt.arity)
                              (rule-error (rules-stack.top)
                                          (format "datatype `" (sym dt.name) "` has no constructor `" (sym tag)
                                                  "` (it has: " (join symbol->string ", " (reverse (dt.get-alt-names))) ")")))))
              (vars0 (nthunk arity new-match-var))
              (wild (make-vector arity #t))
              (rules1 '()))
          (set! nalts (+ nalts 1))
          (define frob-rule
            (rule:t pats code src)
            -> (let ((subs (pattern->subs (car pats))))
                 (when (not (= (length subs) arity))
                   (rule-error (rule:t pats code src)
                               (format "`" (sym tag) "` has " (int arity) " field" (if (= arity 1) "" "s")
                                       ", but this pattern has " (int (length subs)))))
                 (push! rules1 (rule:t (append (pattern->subs (car pats)) (cdr pats)) code src))
                 (for-range i arity
                   (match (nth subs i) with
                     (pattern:variable '_) -> #u
                     _ -> (set! wild[i] #f))
                   )))
          (for-each frob-rule (rules-stack.get))
          ;; if every pattern has a wildcard for this arg of the constructor,
          ;;  then use '_' rather than the symbol we generated.
          (let ((vars1 (map-range i arity (if wild[i] '_ (nth vars0 i)))))
            (push! cases
                  ;; ((:tag var0 var1 ...) (match ...))
                  (sexp
                   (sexp:list
                    (list:cons (sexp:cons 'nil tag null-src) (map make-sexp-symbol vars1)) null-src)
                   ;; we don't reverse rules1 because we popped it off a reversed stack already
                   (compile-match (append vars0 (cdr vars)) rules1 default0))))))
      (let ((result
	     (match mdt with
	       (maybe:yes dt)
	       -> (begin (if (< nalts (dt.get-nalts))
			     (push! cases (sexp (sym 'else) default0)))
			 (sexp:list (append (list (sexp:symbol 'vcase null-src)
                                                  (sexp:symbol dt.name null-src)
                                                  (sexp:symbol (car vars) null-src))
					    (reverse cases)) null-src))
	       (maybe:no)
	       -> (begin (if (not (eq? default match-error))
			     (push! cases (sexp (sexp:symbol 'else null-src) match-fail)))
			 (sexp:list (append (list (sexp:symbol 'vcase null-src)
                                                  (sexp:symbol (car vars) null-src))
					    (reverse cases)) null-src)))
	     ))
	(if (not (eq? default match-error))
	    (fatbar result default)
	    result))))

  (define nthunk
    0 p -> '()
    n p -> (list:cons (p) (nthunk (- n 1) p))
    )

  ;; every rule must have the same number of patterns.  the odd one out
  ;;  is the first that differs from the most common number.
  (define (arity-check rules)
    (let ((lengths (map (lambda (rule) (length (rule->pats rule))) rules))
          (count (lambda (n) (length (filter (lambda (x) (= x n)) lengths))))
          (arity (fold (lambda (n best) (if (> (count n) (count best)) n best)) (car lengths) lengths)))
      (for-list rule rules
        (let ((n (length (rule->pats rule))))
          (when (not (= n arity))
            (rule-error rule (format "this rule has " (int n) " pattern" (if (= n 1) "" "s")
                                     ", but the others have " (int arity))))))
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
