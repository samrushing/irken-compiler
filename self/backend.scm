;; -*- Mode: Irken -*-

(include "self/cps.scm")
(include "self/typing.scm")
(include "self/graph.scm")
(include "self/analyze.scm")

;; XXX add buffering!
(define (make-writer file)
  (let ((level 0))
    (define (write-string s)
      (write file.fd
	     (format (repeat level "  ") s "\n"))
      #u)
    (define (copy s)
      (write file.fd s)
      #u)
    (define (indent) (set! level (+ level 1)))
    (define (dedent) (set! level (- level 1)))
    (define (close-file) (close file.fd))
    {write=write-string indent=indent dedent=dedent copy=copy close=close-file}
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

(define encode-immediate
  (literal:int n)   -> (logior 1 (<< n 1))
  (literal:char ch) -> (logior 2 (<< (char->ascii ch) 8))
  (literal:undef)   -> #x0e
  (literal:cons 'bool 'true _) -> #x106
  (literal:cons 'bool 'false _) -> #x006
  x -> (error1 "expected immediate literal " x))


(define immediate-true  (encode-immediate (literal:cons 'bool 'true '())))
(define immediate-false (encode-immediate (literal:cons 'bool 'false '())))

;; any way to pull these from the include file?

;; immediate types (multiples of 2 (but not 4!))
(define TC_CHAR 2)           ;; (1<<1) // 00000010 02
(define TC_BOOL 6)           ;; (3<<1) // 00000110 06
(define TC_NIL 10)           ;; (5<<1) // 00001010 0a
(define TC_UNDEFINED 14)     ;; (7<<1) // 00001110 0e
(define TC_EMPTY_VECTOR 18)  ;; (9<<1) // 00010010 12
(define TC_USERIMM 22)       ;; (11<<1) // 00010110 16

;; pointer types (multiples of 4)
(define TC_SAVE 4)     ;; (1<<2) // 00000100  04
(define TC_CLOSURE 8)  ;; (2<<2) // 00001000  08
(define TC_TUPLE 12)   ;; (3<<2) // 00001100  0c
(define TC_ENV 12)     ;; (3<<2) // 00001100  0c alias
(define TC_STRING 16)  ;; (4<<2) // 00010000  10
(define TC_VECTOR 20)  ;; (5<<2) // 00010100  14
(define TC_PAIR 24)    ;; (6<<2) // 00011000  18
(define TC_SYMBOL 28)  ;; (7<<2) // 00011100  1c
(define TC_VEC16 32)   ;; (8<<2) // 00100000  20
(define TC_BUFFER 36)  ;; (9<<2) // 00100100  24
(define TC_USEROBJ 40) ;; (10<<2)// 00101000  28


(define (find-jumps insns)
  (let ((used (map-maker <)))
    (walk-insns
     (lambda (insn _)
       (match insn with
	 (insn:jump reg target num free)
	 -> (match (used::get num) with
	      (maybe:yes _) -> #u
	      (maybe:no)    -> (used::add num (if (= target -1)
						  free
						  (list:cons target free)
						  )))
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
       (lambda (x)
	 (match x with
	   (:pair sig0 index0)
	   -> (if (subset? sig sig0)
		  (PUSH candidates sig0))))
       the-context.records)
      (if (= 1 (length candidates))
	  ;; unambiguous - there's only one possible match.
	  (maybe:yes (nth candidates 0))
	  ;; this sig is ambiguous given the set of known records.
	  (maybe:no)))))

;; XXX: profiler is currently unimplemented (it was originally
;;  written for the all-in-one-function C back end).

(define (emit-profile-0 o)
  (o.write "
static int64_t prof_mark0;
static int64_t prof_mark1;
typedef struct {
  int calls;
  int64_t ticks;
  char * name;
} pxll_prof;
static pxll_prof prof_funs[];
static int prof_current_fun;
static int prof_num_funs;
static void prof_dump (void)
{
 int i=0;
 fprintf (stderr, \"%20s\\t%20s\\t%s\\n\", \"calls\", \"ticks\", \"name\");
 for (i=0; prof_funs[i].name; i++) {
   fprintf (stderr, \"%20d\\t%20\" PRIu64 \"\\t%s\\n\", prof_funs[i].calls, prof_funs[i].ticks, prof_funs[i].name);
 }
}
"))

(define (emit-profile-1 o)
  (o.write "static pxll_prof prof_funs[] = \n  {{0, 0, \"top\"},")
  (for-map k v the-context.profile-funs
    (let ((name (cdr (reverse v.names)))) ;; strip 'top' off
      (o.write (format "   {0, 0, \"" (join symbol->string "." name) "\"},"))))
  (o.write "   {0, 0, NULL}};"))

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
	(symbol-counter 0)
	)

    ;; emit UOHEAD and UITAG macros, special-casing the builtin datatypes
    (define (uohead nargs dt variant index)
      (match dt variant with
	'list 'cons -> "CONS_HEADER"
	_ _ -> (format "UOHEAD(" (int nargs) "," (int index) ")")))

    (define (uitag dt variant index)
      (match dt variant with
	'list 'nil -> "TC_NIL"
	_ _ -> (format "UITAG(" (int index) ")")))

    (define (walk exp litnum)
      (match exp with
	;; data constructor
	(literal:cons dt variant args)
	-> (let ((dto (alist/get the-context.datatypes dt "no such datatype"))
		 (alt (dto.get variant))
		 (nargs (length args)))
	     (if (> nargs 0)
		 ;; constructor with args
		 (let ((args0 (map (lambda (arg) (walk arg litnum)) args))
		       (addr (+ 1 (length output))))
		   (PUSH output (uohead nargs dt variant alt.index))
		   (for-each (lambda (x) (PUSH output x)) args0)
		   (format "UPTR(" (int litnum) "," (int addr) ")"))
		 ;; nullary constructor - immediate
		 (uitag dt variant alt.index)))
	(literal:vector args)
	-> (let ((args0 (map (lambda (arg) (walk arg litnum)) args))
		 (nargs (length args))
		 (addr (+ 1 (length output))))
	     (PUSH output (format "(" (int nargs) "<<8)|TC_VECTOR"))
	     (for-each (lambda (x) (PUSH output x)) args0)
	     (format "UPTR(" (int litnum) "," (int addr) ")"))
	(literal:symbol sym)
	-> (let ((index (alist/get the-context.symbols sym "unknown symbol?")))
	     (format "UPTR(" (int index) ",1)"))
	(literal:string s)
	->  (format "UPTR0(" (int (cmap->index lits exp)) ")")
        ;; NOTE: sexp is missing from here.  without that, no sexp literals.
	_ -> (int->string (encode-immediate exp))
	))
    (o.dedent) ;; XXX fix this by defaulting to zero indent
    (for-map i lit lits.rev
	(set! output '())
        (match lit with
          ;; strings are a special case here because they have a non-uniform structure: the existence of
          ;;   the uint32_t <length> field means it's hard for us to put a UPTR in the front.
          (literal:string s)
          -> (let ((slen (string-length s)))
               ;; this works because we want strings compared for eq? identity...
               (o.write (format "pxll_string constructed_" (int i)
                                " = {STRING_HEADER(" (int slen)
                                "), " (int slen)
                                ", \"" (c-string s) "\" };")))
          ;; there's a temptation to skip the extra pointer at the front, but that would require additional smarts
          ;;   in insn_constructed (as already exist for strings).
          (literal:symbol s)
          -> (begin
               (o.write (format "// symbol " (sym s)))
               (o.write (format "pxll_int constructed_" (int i)
                                "[] = {UPTR(" (int i)
                                ",1), SYMBOL_HEADER, UPTR0("
                                (int (cmap->index lits (literal:string (symbol->string s))))
                                "), INTCON(" (int symbol-counter) ")};"))
               (set! symbol-counter (+ 1 symbol-counter))
               )
          _ -> (let ((val (walk lit i))
                     (rout (list:cons val (reverse output))))
                 (o.write (format "pxll_int constructed_" (int i) "[] = {" (join "," rout) "};")))
          ))
    (let ((symptrs '()))
      (alist/iterate
       (lambda (symbol index)
	 (PUSH symptrs (format "UPTR(" (int index) ",1)")))
       the-context.symbols)
      ;; XXX NOTE: this does not properly use TC_EMPTY_VECTOR
      (o.write (format "pxll_int pxll_internal_symbols[] = {(" (int (length symptrs)) "<<8)|TC_VECTOR, " (join ", " symptrs) "};"))
      )
    (o.indent)
    ))

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

(define (emit-lookup-field o)
  (if (> (length the-context.records) 0)
      (begin
	(o.write "static int lookup_field (int tag, int label)")
	(o.write "{ switch (tag) {")
	(for-each
	 (lambda (pair)
	   (match pair with
		  (:pair sig index)
	      -> (begin (o.write (format "  // {" (join symbol->string " " sig) "}"))
			(o.write (format "  case " (int index) ":"))
			(o.write "  switch (label) {")
			(for-range
			 i (length sig)
			 (o.write (format "     case "
					  (int (lookup-label-code (nth sig i)))
					  ": return " (int i) "; break;")))
			(o.write "  } break;"))))
	 (reverse the-context.records))
	(o.write "} return 0; }"))
      ;; record_fetch/record_store refer to this function even when it's not needed.
      (o.write "static int lookup_field (int tag, int label) { return 0; }")
      ))

(define (emit-datatype-table o)
  (o.write (format "// datatype table"))
  (alist/iterate
   (lambda (name dt)
     (o.write (format "// name: " (sym name)))
     (dt.iterate
      (lambda (tag alt)
	(o.write (format "//  (:" (sym tag) " " (join type-repr " " alt.types) ")")))))
   the-context.datatypes))

(define (get-file-contents path)
  (read-file-contents
   (find-file the-context.options.include-dirs path)))

;; give a unique index to each function.
(define (number-profile-funs)
  (let ((i 0))
    (for-map k v the-context.profile-funs
      (set! v.index i)
      (set! i (+ i 1)))
    ))

(include "self/c.scm")
(include "self/llvm.scm")
(include "self/bytecode.scm")

(define (compile-with-backend base cps)
  (match the-context.options.backend with
    (backend:bytecode)
    -> (compile-to-bytecode base cps)
    _ -> (compile-with-backend0 base cps)
    ))

;; note: the llvm backend relies on the C backend to some extent.

(define (compile-with-backend0 base cps)

  (let ((opath (string-append base ".c"))
	(ofile (file/open-write opath #t #o644))
	(o (make-writer ofile))
	(tmp-path (format base ".tmp.c"))
	(tfile (file/open-write tmp-path #t #o644))
	(o0 (make-writer tfile))
	(llvm? (eq? the-context.options.backend (backend:llvm)))
	(sources (LIST opath))
	)
    (printf "\n-- C output --\n")
    (printf " : " opath "\n")
    (for-each
     (lambda (path) (o.write (format "#include <" path ">")))
     (reverse the-context.cincludes))
    (for-each
     (lambda (path) (o.write (format "#include \"" path "\"")))
     (reverse the-context.lincludes))
    (for-each o.write (reverse the-context.cverbatim))
    (o.copy (get-file-contents "include/header1.c"))
    (emit-constructed o)
    (emit-lookup-field o)
    (emit-datatype-table o)
    (number-profile-funs)
    (if the-context.options.profile
	(emit-profile-0 o)
	(o.write "static void prof_dump (void) {}"))
    (if llvm?
	(let ((llpath (format base ".ll"))
	      (llvm-file (file/open-write llpath #t #o644))
	      (ollvm (make-writer llvm-file)))
	  (printf " : " llpath "\n")
	  (ollvm.copy
	   (get-file-contents "include/preamble.ll"))
	  (emit-llvm o ollvm "toplevel" cps)
	  (ollvm.close)
	  (PUSH sources llpath))
	(emit-c o0 o cps))
    (if the-context.options.profile (emit-profile-1 o))
    (print-string "done.\n")
    (o0.close)
    ;; copy code after declarations
    (o.copy (read-file-contents (file/open-read tmp-path)))
    (o.close)
    (unlink tmp-path)
    (when (not the-context.options.nocompile)
	  (print-string "compiling...\n")
	  (invoke-cc base sources the-context.options (if llvm? "-flto" ""))
	  #u
	  )
    )
  )
