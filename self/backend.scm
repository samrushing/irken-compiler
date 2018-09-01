;; -*- Mode: Irken -*-

(require "self/cps.scm")
(require "lib/partial.scm")

(define (make-writer file)
  (let ((level 0)
        (buffer '())
        (nbytes 0)
        (total 0))
    (define (push s)
      (push! buffer s)
      (set! nbytes (+ nbytes (string-length s)))
      (if (>= nbytes 16384)
          (flush)))
    (define (write-string s)
      (push (format (repeat level "  ") s "\n")))
    (define (flush)
      (when (> nbytes 0)
        (write file.fd (string-concat (reverse buffer)))
        (set! buffer '())
        (set! total (+ total nbytes))
        (set! nbytes 0)))
    (define (indent) (set! level (+ level 1)))
    (define (dedent) (set! level (- level 1)))
    (define (close-file)
      (flush)
      (close file.fd)
      )
    (define (get-total)
      total)
    {write=write-string indent=indent dedent=dedent copy=push close=close-file get-total=get-total flush=flush}
    ))

(define (make-name-frobber)
  (define safe-name-map
    (literal
     (alist/make
      (#\! "_bang")
      (#\* "_splat")
      (#\? "_question")
      (#\- "_")
      (#\+ "_plus")
      (#\% "_percent")
      )))
  (define c-legal? (char-class (string->list "abcdefghijklmnopqrstuvwxyz_0123456789")))
  (define (frob-name name)
    (define (frob)
      (let loop ((i 0) (r '()))
	(if (= i (string-length name))
	    r
	    (let ((ch (string-ref name i)))
	      (loop (+ i 1)
		    (list:cons
		     (if (c-legal? ch)
			 (char->string ch)
			 (match (alist/lookup safe-name-map ch) with
			   (maybe:yes sub) -> sub
			   (maybe:no)      -> (format "_" (hex (char->ascii ch)))))
		     r))))))
    (let ((r (string-concat (reverse (frob)))))
      (if (string=? r "_")
	  ;; special-case
	  "minus"
	  r)))
  frob-name)

(define frob-name (make-name-frobber))

(define (gen-function-cname sym n)
  (format "FUN_" (frob-name (symbol->string sym)) "_" (int n)))

(define label-maker
  (let ((counter (make-counter 0)))
    (lambda ()
      (format "L" (int (counter.inc))))))

(define (encode-user-immediate dtname altname)
  (match (alist/lookup the-context.datatypes dtname) with
    (maybe:no) -> (raise (:NoSuchDatatype "NoSuchDatatype" dtname))
    (maybe:yes dt)
    -> (let ((alt (dt.get altname)))
         (+ TC_USERIMM (<< alt.index 8)))))

(define encode-immediate
  (literal:int n)              -> (logior 1 (<< n 1))
  (literal:char ch)            -> (logior 2 (<< (char->ascii ch) 8))
  (literal:undef)              -> #x0e
  (literal:bool #t)            -> #x106
  (literal:bool #f)            -> #x006
  (literal:vector ())          -> #x12
  (literal:cons 'list 'nil ()) -> TC_NIL
  (literal:cons dt alt ())     -> (encode-user-immediate dt alt)
  x -> (error1 "expected immediate literal " x))

(define immediate-true  (encode-immediate (literal:bool #t)))
(define immediate-false (encode-immediate (literal:bool #f)))

;; immediate types (multiples of 2 (but not 4!))
(define TC_CHAR         (<<  1 1)) ;; 00000010 02
(define TC_BOOL         (<<  3 1)) ;; 00000110 06
(define TC_NIL          (<<  5 1)) ;; 00001010 0a
(define TC_UNDEFINED    (<<  7 1)) ;; 00001110 0e
(define TC_EMPTY_VECTOR (<<  9 1)) ;; 00010010 12
(define TC_USERIMM      (<< 11 1)) ;; 00010110 16

;; pointer types (multiples of 4)
(define TC_SAVE         (<<  1 2)) ;; 00000100  04
(define TC_CLOSURE      (<<  2 2)) ;; 00001000  08
(define TC_TUPLE        (<<  3 2)) ;; 00001100  0c
(define TC_ENV          (<<  3 2)) ;; 00001100  0c alias
(define TC_STRING       (<<  4 2)) ;; 00010000  10
(define TC_VECTOR       (<<  5 2)) ;; 00010100  14
(define TC_PAIR         (<<  6 2)) ;; 00011000  18
(define TC_SYMBOL       (<<  7 2)) ;; 00011100  1c
(define TC_BUFFER       (<<  8 2)) ;; 00100000  20
(define TC_FOREIGN      (<<  9 2)) ;; 00100100  24
(define TC_USEROBJ      (<< 10 2)) ;; 00100100  24

(define (find-jumps insns)
  (let ((used (map-maker int-cmp)))
    (walk-insns
     (lambda (insn _)
       (match insn with
	 (insn:jump reg target num free)
	 -> (match (used::get num) with
	      (maybe:yes _) -> #u
	      (maybe:no)    -> (used::add num (if (= target -1)
						  free.val
						  (list:cons target free.val)
						  )))
         (insn:fail num npop free)
         -> (match (used::get num) with
              (maybe:yes _) -> #u
              (maybe:no)    -> (used::add num free.val))
	 _ -> #u))
     insns)
    used))

(define (subset? a b)
  (every? (lambda (x) (member-eq? x b)) a))

(define (guess-record-type sig)
  ;; can we disambiguate this record signature?
  (let ((sig (map (lambda (x) ;; remove sexp wrapping
		    (match x with
		      (sexp:symbol field) -> field
		      _ -> (impossible))) sig))
	(sig (filter (lambda (x) (not (eq? x '...))) sig)))
    (let ((candidates '()))
      (for-each
       (lambda (sig0)
         (if (subset? sig sig0)
             (push! candidates sig0)))
       (cmap/keys the-context.records))
      (if (= 1 (length candidates))
	  ;; unambiguous - there's only one possible match.
	  (maybe:yes (nth candidates 0))
	  ;; this sig is ambiguous given the set of known records.
	  (maybe:no)
          ))))

;; profiler currently works only on the C backend.

(define (emit-profile-0 o)
  (o.write "
void prof_dump (void)
{
 int i=0;
 fprintf (stderr, \"%15s\\t%15s\\t%15s\\t%15s\\t%s\\n\", \"calls\", \"ticks\", \"allocs\", \"words\", \"name\");
 for (i=0; prof_funs[i].name; i++) {
   fprintf (stderr, \"%15d\\t%15\" PRIu64 \"%15\" PRIu64 \"%15\" PRIu64 \"\\t%s\\n\", prof_funs[i].calls, prof_funs[i].ticks, prof_funs[i].allocs, prof_funs[i].alloc_words, prof_funs[i].name);
 }
}
"))

(define (emit-profile-1 o)
  (o.write "static irk_prof prof_funs[] = \n  {{0, 0, 0, 0, \"top\"},")
  (for-map k v the-context.profile-funs
    (let ((name (cdr (reverse v.names)))) ;; strip 'top' off
      (o.write (format "   {0, 0, 0, 0, \"" (join symbol->string "." name) "\"},"))))
  (o.write "   {0, 0, 0, 0, NULL}};"))

;; with large programs, the constructed initializers can get *really*
;;  long... long enough to cause problems even for emacs.
(define (sprinkle-newlines parts)
  (let ((n 0)
        (r '()))
    (for-list part parts
      (cond ((= n 10)
             (push! r (string-append "\n\t" part))
             (set! n 0))
            (else
             (push! r part)))
      (inc! n))
    (reverse r)))

;; we support three types of non-immediate literals:
;;
;; 1) strings.  identical strings are merged. do not modify literal strings.
;; 2) symbols.  this emits a string followed by a symbol tuple.
;;      these are collected so each is unique.  any runtime
;;      symbol table should be populated with these first.
;; 3) constructed.  trees of literals made of constructors
;;      (e.g. lists formed with QUOTE), and vectors.  each tree
;;      is rendered into a single C array where the first value
;;      in the array points to the beginning of the top-level
;;      object.

(define (emit-constructed o)
  (let ((lits the-context.literals)
	(nlits lits.count)
	(output '())
        (oindex 0)
	(symbol-counter 0)
	)

    (define (push-val n)
      (push! output n)
      (inc! oindex))

    ;; emit UOHEAD and UITAG macros, special-casing the builtin datatypes
    (define (uohead nargs dt variant index)
      (match dt variant with
	'list 'cons -> "CONS_HEADER"
	_ _ -> (format "UOHEAD(" (int nargs) "," (int index) ")")))

    (define (uitag dt variant index)
      (match dt variant with
	'list 'nil -> "TC_NIL"
	_ _ -> (format "UITAG(" (int index) ")")))

    (define get-dtcon-tag
      'nil label -> (alist/get the-context.variant-labels label "unknown variant label")
      dt variant -> (let ((dtob (alist/get the-context.datatypes dt "no such datatype"))
                          (alt (dtob.get variant)))
                      alt.index))

    (define (walk exp litnum)
      (match exp with
	;; data constructor
	(literal:cons dt variant args)
	-> (let ((tag (get-dtcon-tag dt variant))
		 (nargs (length args)))
	     (if (> nargs 0)
		 ;; constructor with args
		 (let ((args0 (map (partial (walk _ litnum)) args))
                       (oindex0 (+ 1 oindex)))
		   (push-val (uohead nargs dt variant tag))
                   (for-each push-val args0)
		   (format "UPTR(" (int litnum) "," (int oindex0) ")"))
		 ;; nullary constructor - immediate
		 (uitag dt variant tag)))
        (literal:vector ())
        -> (int->string (encode-immediate exp))
	(literal:vector args)
	-> (let ((args0 (map (partial (walk _ litnum)) args))
		 (nargs (length args))
                 (oindex0 (+ 1 oindex)))
	     (push-val (format "(" (int nargs) "<<8)|TC_VECTOR"))
	     (for-each push-val args0)
	     (format "UPTR(" (int litnum) "," (int oindex0) ")"))
        (literal:record tag fields)
        -> (let ((args0 (map (lambda (field)
                               (match field with
                                 (litfield:t name val)
                                 -> (walk val litnum)))
                             fields))
                 (nargs (length args0))
                 (oindex0 (+ 1 oindex)))
             (push-val (format "(" (int nargs) "<<8)|UOTAG(" (int tag) ")"))
             (for-each push-val args0)
             (format "UPTR(" (int litnum) "," (int oindex0) ")"))
	(literal:symbol sym)
        -> (let ((index (tree/get the-context.symbols symbol-index-cmp sym)))
	     (format "UPTR(" (int index) ",1)"))
	(literal:string s)
	->  (format "UPTR0(" (int (cmap->index lits exp)) ")")
        ;; NOTE: sexp is missing from here.  without that, no sexp literals.
	_ -> (int->string (encode-immediate exp))
	))
    (o.dedent) ;; XXX fix this by defaulting to zero indent
    (for-map i lit lits.rev
      (set! output '())
      (set! oindex 0)
      (match lit with
        ;; strings are a special case here because they have a non-uniform structure: the existence of
        ;;   the uint32_t <length> field means it's hard for us to put a UPTR in the front.
        (literal:string s)
        -> (let ((slen (string-length s)))
             ;; this works because we want strings compared for eq? identity...
             (o.write (format "irk_string constructed_" (int i)
                              " = {STRING_HEADER(" (int slen)
                              "), " (int slen)
                              ", \"" (c-string s) "\" };")))
        ;; there's a temptation to skip the extra pointer at the front, but that would require additional smarts
        ;;   in insn_constructed (as already exist for strings).
        (literal:symbol s)
        -> (begin
             (o.write (format "// symbol " (sym s)))
             (o.write (format "irk_int constructed_" (int i)
                              "[] = {UPTR(" (int i)
                              ",1), SYMBOL_HEADER, UPTR0("
                              (int (cmap->index lits (literal:string (symbol->string s))))
                              "), INTCON(" (int symbol-counter) ")};"))
             (set! symbol-counter (+ 1 symbol-counter))
             )
        _ -> (let ((val (walk lit i))
                   (rout (list:cons val (sprinkle-newlines (reverse output)))))
               (o.write (format "irk_int constructed_" (int i) "[] = {" (join "," rout) "};")))
        ))
    (let ((symptrs '()))
      (tree/inorder
       (lambda (symbol index)
	 (push! symptrs (format "UPTR(" (int index) ",1)")))
       the-context.symbols)
      ;; XXX NOTE: this does not properly use TC_EMPTY_VECTOR
      (o.write (format "irk_int irk_internal_symbols[] = {("
                       (int (length symptrs)) "<<8)|TC_VECTOR,\n\t" (join "," (sprinkle-newlines symptrs)) "};"))
      (o.write (format "object * irk_internal_symbols_p = (object*) &irk_internal_symbols;"))
      )
    (o.indent)
    ))

;; XXX quote and backslash probably don't belong here.
(define c-string-safe?
  (char-class
   (string->list
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ ")))

(define (char->oct-encoding ch)
  (let ((in-oct (format (oct (char->ascii ch)))))
    (format
     "\\"
     ;; can't use zpad here because we want to catch out-of-range chars.
     (match (string-length in-oct) with
       0 -> "000"
       1 -> "00"
       2 -> "0"
       3 -> ""
       _ -> (error1 "unable to oct-encode character" in-oct)
       )
     in-oct)))

(define (c-string s)
  (let loop ((r '())
	     (s (string->list s)))
    (match s with
      () -> (string-concat (reverse r))
      (ch . rest)
      -> (loop
	  (list:cons
	   (match ch with
	     #\return  -> "\\r"
	     #\newline -> "\\n"
	     #\tab     -> "\\t"
	     #\\       -> "\\\\"
	     #\"       -> "\\\""
	     _ -> (if (c-string-safe? ch)
		      (char->string ch)
		      (char->oct-encoding ch)))
	   r)
	  rest))))

;; perfect hash function for ambiguous record field lookups.
;; some field-index lookups are unable to be resolved at compile time.
;; we use a map (size N) of (tag, label)->index at runtime.
;; this map is implemented using a two-stage hash,
;; vectoring into two tables of length N.

;; based directly on Steve Hanov's python code.
;; http://stevehanov.ca/blog/index.php?id=119

;; Note: this is obviously a bit overkill, but previously I used a
;;  large generated function that was going to really complicate
;;  independent LLVM code generation (e.g. for JIT) and I really
;;  wanted something table-based.  This generates a really small
;;  table!

;; unrolled - our keys are always of length 2
;; NOTE: this obviously must match the hash function in include/header1.c:p_hash().
;; XXX: this code is not compatible with 32-bit irkvm.  we should probably tweak
;;   this slightly to avoid the 32-bit mask.  the given magic number is a prime, I'm
;;   sure the exact size of the mask is of little concern.
(define (hash2 d k0 k1)
  (logand #xffffffff
    (logxor k0
      (* (logand #xffffffff
            (logxor k1 (* d #x01000193)))
         #x01000193))))

(define (hash-item d item size)
  (mod (hash2 d item.k0 item.k1) size))

(define (create-minimal-perfect-hash table)

  (define not-there 500000)

  (let ((size (vector-length table))
        (buckets (make-vector size (list:nil)))
        (G (make-vector size 0))
        (V (make-vector size not-there))
        (split 0))
    ;; Step 1: Place all of the keys into buckets.
    (for-vector elem table
      (let ((index (hash-item #x01000193 elem size)))
        (set! buckets[index] (list:cons elem buckets[index]))))
    ;; Step 2: Sort the buckets and process the ones with the most items first.
    (set! buckets (sort-vector (lambda (a b) (> (length a) (length b))) buckets))
    (let/cc break
      (for-range i size
        (let ((bucket buckets[i])
              (blen (length bucket))
              (d 1)
              (item 0)
              (slots (list:nil)))
          (when (<= blen 1)
            (set! split i)
            (break #u))
          ;; Repeatedly try different values of d until we find a hash function
          ;; that places all items in the bucket into free slots
          (while (< item blen)
            (let ((slot (hash-item d (nth bucket item) size)))
              (if (or (not (= V[slot] not-there))
                      (member-eq? slot slots))
                  (begin
                    (set! d (+ 1 d))
                    (set! item 0)
                    (set! slots (list:nil)))
                  (begin
                    (push! slots slot)
                    (set! item (+ item 1))))))
          (set! slots (reverse slots))
          (set! G[(hash-item #x01000193 (nth bucket 0) size)] d)
          (for-range j blen
            (let ((elem (nth bucket j)))
              (set! V[(nth slots j)] elem.v)))
          )))
    ;; Only buckets with 1 item remain. Process them more quickly by directly
    ;; placing them into a free slot. Use a negative value of d to indicate
    ;; this.
    (let ((freelist '()))
      (for-range i size
        (if (= V[i] not-there)
            (push! freelist i)))
      (for-range i size
        (let ((bucket buckets[i])
              (blen (length bucket)))
          (when (= blen 1)
            (let ((slot (pop! freelist))
                  (elem (nth bucket 0)))
              ;; we subtract one to ensure it's negative even if the zeroeth slot was
              ;; used.
              (set! G[(hash-item #x01000193 elem size)] (- 0 slot 1))
              (set! V[slot] elem.v)))))
      )
    (:tuple G V)))

;; the backend takes note whenever a field label's index cannot be
;;  unambiguously resolved. rather than generating a table of all
;;  tag,label->index tuples, we generate only the entries that resulted
;;  in a call to `lookup_field()`.

(define (build-ambig-table)
  ;; we insert one bogus entry to handle a problem with the perfect hash algorithm
  ;;  when there are exactly two ambiguous pairs.
  (let ((table (tree/insert (tree/empty) magic-cmp (:tuple 1013 1013) 1013)))
    (for-map sig index the-context.records.map
      (let ((ambs '()))
        (for-range i (length sig)
          (let ((label-code (lookup-label-code (nth sig i))))
            (match (tree/member the-context.ambig-rec int-cmp label-code) with
              (maybe:yes _)
              -> (push! ambs (:tuple i label-code))
              (maybe:no)
              -> #u)))
        (when (> (length ambs) 0)
          (for-list item ambs
            (match item with
              (:tuple i label-code)
              -> (tree/insert! table magic-cmp (:tuple index label-code) i))))
        ))
    table))

(define (emit-datatype-table o)
  (o.write (format "// datatype table"))
  (for-alist name dt the-context.datatypes
    (o.write (format "// name: " (sym name)))
    (dt.iterate
     (lambda (tag alt)
       (o.write (format "//  (:" (sym tag) " " (join type-repr " " alt.types) ")"))))))

(define (get-file-contents path)
  (let (((path0 file) (find-file the-context.options.include-dirs path)))
    (read-file-contents file)))

(define (copy-file-contents ofile path)
  (let ((ifile (file/open-read path)))
    (for block (make-file-generator ifile)
         (ofile.copy block))
    (file/close ifile)))

;; give a unique index to each function.
(define (number-profile-funs)
  (let ((i 0))
    (for-map k v the-context.profile-funs
      (set! v.index i)
      (set! i (+ i 1)))
    ))

;; generate metadata for this program.  It is made available as a literal,
;; accessible via the 'irk_get_metadata'.
(define (generate-metadata)
  (match (alist/lookup the-context.datatypes 'sexp) with
    (maybe:yes _)
    -> (let ((r '()))
         (push! r (sexp1 'variants
                        (alist/map
                         (lambda (name index) (sexp (sym name) (int index)))
                         the-context.variant-labels)))
         (push! r (sexp1 'exceptions
                        (alist/map
                         (lambda (name type) (sexp (sym name) (type->sexp (apply-subst type))))
                         the-context.exceptions)))
         (push! r (sexp1 'datatypes
                        (alist/map
                         (lambda (name dt) (dt.to-sexp))
                         the-context.datatypes)))
         (let ((lit (unsexp (sexp1 'metadata r))))
           (scan-literals (list lit))
           (cmap/add the-context.literals lit))
         )
    ;; if the program doesn't even include lib/sexp, then it cannot make
    ;;   use of metadata regardless. skip it.
    (maybe:no)
    -> (cmap/add the-context.literals (literal:vector (list (literal:int 0))))
    ))
