;; -*- Mode: Irken -*-

;; --- literals ---

;; quote a string for the assembler's .ascii directive.  we can't use
;;   repr-string: gas reads *every* hex digit after \x, so "\xfc" followed
;;   by "c" is taken as the single escape \xfcc, the string comes out one
;;   byte short, and everything after it in .data is misaligned.  octal
;;   escapes stop after three digits.
(define (asm-string-repr s)
  (define (asm-char ch)
    (let ((n (char->ascii ch)))
      (cond ((eq? ch #\") "\\\"")
            ((eq? ch #\\) "\\\\")
            ((and (>= n 32) (< n 127)) (char->string ch))
            (else (format "\\" (zpad 3 (oct n)))))))
  (format "\"" (join (map asm-char (string->list s))) "\""))

(define (asm-emit-constructed o)
  (let ((lits the-context.literals)
	(nlits lits.count)
	(output '())
        (oindex 0)
	(symbol-counter 0))

    (define (push-val n)
      (push! output n)
      (inc! oindex))

    ;; hacks for datatypes known by the runtime
    (define (otag dtname altname index)
      (match dtname altname with
	'list 'cons -> TC_PAIR
	'symbol 't  -> TC_SYMBOL
	_ _         -> (+ TC_USEROBJ (<< index 2))))

    (define (itag dtname altname index)
      (match dtname altname with
	'list 'nil   -> TC_NIL
	'bool 'true  -> immediate-true
	'bool 'false -> immediate-false
	_ _          -> (+ TC_USERIMM (<< index TAGSIZE))))

    (define (ohead tag len)
      ;; more readable lits, but slightly larger, not worth it.
      ;;(format "0x" (hex (+ tag (<< len TAGSIZE))))
      (int->string (+ tag (<< len TAGSIZE)))
      )

    (define (uohead tag len)
      (ohead (+ TC_USEROBJ (<< tag 2)) len))

    (define get-dtcon-tag
      'nil label -> (alist/get the-context.variant-labels label "unknown variant label")
      dt variant -> (let ((dtob (alist/get the-context.datatypes dt "no such datatype"))
                          (alt (dtob.get variant)))
                      alt.index))

    (define (litref num index)
      ;; we use a 'local symbol', which makes large literals more compact.
      (format "0b+" (int (* 8 index)))
      )

    (define (litref0 num)
      (format "lit_" (int num)))

    (define (compute-string-pad slen tlen)
      (- (* 8 tlen) (+ 4 slen)))

    (define (walk exp litnum)
      (match exp with
	;; data constructor
	(literal:cons dt variant args)
	-> (let ((tag (get-dtcon-tag dt variant))
		 (nargs (length args)))
	     (if (> nargs 0)
		 ;; constructor with args
		 (let ((args0 (map (partial (walk _ litnum)) args))
                       (oindex0 oindex))
                   (push-val (ohead (otag dt variant tag) nargs))
                   (for-each push-val args0)
                   (litref litnum oindex0))
		 ;; nullary constructor - immediate
                 (format (int (itag dt variant tag)))))
        (literal:vector ())
        -> (format (int (encode-immediate exp)))
	(literal:vector args)
	-> (let ((args0 (map (partial (walk _ litnum)) args))
		 (nargs (length args))
                 (oindex0 oindex))
             (push-val (ohead TC_VECTOR nargs))
	     (for-each push-val args0)
             (litref litnum oindex0))
        (literal:record tag fields)
        -> (let ((args0 (map (lambda (field)
                               (match field with
                                 (litfield:t name val)
                                 -> (walk val litnum)))
                             fields))
                 (nargs (length args0))
                 (oindex0 oindex))
             (push-val (uohead tag nargs))
             (for-each push-val args0)
             (litref litnum oindex0))
	(literal:symbol sym)
        -> (let ((index (tree/get the-context.symbols symbol-index-cmp sym)))
             (litref0 index))
	(literal:string s)
        -> (litref0 (cmap->index lits exp))
        ;; NOTE: sexp is missing from here.  without that, no sexp literals.
        ;;    also also: sexps now have records embedded with them, so...
        _ -> (format (int (encode-immediate exp)))
	))
    (o.indent)
    (for-map i lit lits.rev
      (set! output '())
      (set! oindex 0)
      (match lit with
        (literal:string s)
        -> (let ((slen (string-length s))
                 (tlen (string-tuple-length slen))
                 (npad (compute-string-pad slen tlen)))
             (o.dedent) (oformat "lit_" (int i) ":") (o.indent)
             (oformat ".quad " (ohead TC_STRING tlen))
             (oformat ".long " (int slen))
             (oformat ".ascii " (asm-string-repr s))
             (when (not (zero? npad))
               (oformat ".space " (int npad))))
        (literal:symbol s)
        -> (let ((sindex (cmap->index lits (literal:string (symbol->string s)))))
             (oformat "# symbol " (sym s))
             (o.dedent) (oformat "lit_" (int i) ":") (o.indent)
             (oformat ".quad " (ohead TC_SYMBOL 2)
                      "," (litref0 sindex)
                      ", " (int (encode-immediate (literal:int symbol-counter))))
             (set! symbol-counter (+ 1 symbol-counter)))
        _ -> (let ((val (walk lit i))
                   (rout (reverse output)))
               ;; local symbol '0' will be back-referenced in the literal to follow.
               (o.dedent) (oformat "0:") (o.indent)
               (emit-nice-quads o rout)
               (o.dedent) (oformat "lit_" (int i) ":") (o.indent)
               (oformat ".quad " val)
               )
        ))
    (let ((symptrs (map litref0 (tree/values the-context.symbols))))
      (o.dedent) (oformat (clabel "irk_internal_symbols:")) (o.indent)
      (if (null? symptrs)
          (oformat ".quad " (int TC_EMPTY_VECTOR))
          (emit-nice-quads o (list:cons (ohead TC_VECTOR (length symptrs)) symptrs))
          ))
    (o.dedent)
    ))

;; format longer data lines nicely.
(define (emit-nice-quads o items)
  (let ((line (list:nil))
        (len 0))
    (for-list item items
      (when (> len 120)
        (oformat ".quad " (join "," (reverse line)))
        (set! line (list:nil))
        (set! len 0))
      (push! line item)
      (inc! len (+ 1 (string-length item))))
    (when (> len 0)
      (oformat ".quad " (join "," (reverse line))))))

(define (clabel name)
  (if the-context.options.asm-underscores
      (format "_" name)
      name))

;; --- perfect hash tables ---

(define (emit-asm-lookup-field-hashtables o)
  (let ((ambig (build-ambig-table))
        (size (tree/size ambig))
        (table (make-vector size {k0=0 k1=0 v=0}))
        (i 0))
    (tree/inorder
     (lambda (k v)
       (match k with
         (:tuple tag label)
         -> (set! table[i] {k0= tag k1=label v=v}))
       (set! i (+ i 1)))
     ambig)
    (let (((G V) (create-minimal-perfect-hash table)))
      (o.indent)
      (oformat ".data")
      (oformat ".global " (clabel "irk_ambig_size"))
      (o.dedent) (oformat (clabel "irk_ambig_size") ":") (o.indent)
      (oformat ".4byte " (int size))
      (oformat ".global " (clabel "G"))
      (o.dedent) (oformat (clabel "G") ":") (o.indent)
      (oformat ".4byte " (join int->string "," (vector->list G)))
      (oformat ".global "(clabel "V"))
      (o.dedent) (oformat (clabel "V") ":") (o.indent)
      (oformat ".4byte " (join int->string "," (vector->list V)))
      (o.dedent)
      )))

