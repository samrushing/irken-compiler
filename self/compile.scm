;; -*- Mode: Irken -*-

(include "self/backend.scm")

(define (find-base path)
  (let ((parts (string-split path #\.))
	(rparts (reverse parts)))
    (if (not (string=? (first rparts) "scm"))
	(error1 "find-base" path)
	(string-join (reverse (cdr rparts)) "."))))

(define (read-template)
  (let ((ifile (file/open-read "header.c")))
    (let loop ((buf (file/read-buffer ifile))
	       (l '()))
      (cond ((= (string-length buf) 0) (string-concat (reverse l)))
	    (else (loop (file/read-buffer ifile)
			(list:cons buf l)))))))

(define sentinel0 "// REGISTER_DECLARATIONS //\n")
(define sentinel1 "// CONSTRUCTED LITERALS //\n")

(define (get-header-parts)
  (let ((header (read-template))
	(pos0 (string-find sentinel0 header))
	(pos1 (string-find sentinel1 header)))
    (if (or (= pos0 -1) (= pos1 -1))
	(error1 "template strings not found in header.c?" (:pair pos0 pos1))
	(let ((pos0 (+ pos0 (string-length sentinel0)))
	      (pos1 (+ pos1 (string-length sentinel1)))
	      (part0 (substring header 0 pos0))
	      (part1 (substring header pos0 pos1))
	      (part2 (substring header pos1 (string-length header))))
	  (:header part0 part1 part2)))))

(define (getenv-or var default)
  (let ((val (getenv var)))
    (if (= 0 (string-length val))
	default
	val)))

(include "self/flags.scm")

(define (gcc base options)
  (let ((cc (getenv-or "CC" CC))
	(cflags (getenv-or "CFLAGS" CFLAGS))
	(cflags (format cflags " " (if options.optimize "-O" "") " " options.extra-cflags))
	(cmd (format cc " " cflags " " base ".c -o " base)))
    (print-string (format "system: " cmd "\n"))
    (system cmd)))

(define (get-options argv options)
  (for-range
      i (vector-length argv)
      (match sys.argv[i] with
	"-c" -> (set! options.nocompile #t)
	"-v" -> (set! options.verbose #t)
	"-t" -> (set! options.trace #t)
	"-f" -> (begin (set! i (+ i 1))
		       (set! options.extra-cflags argv[i]))
	"-m" -> (set! options.debugmacroexpansion #t)
	;; this option only applies to the C compilation phase.
	"-O" -> (set! options.optimize #t)
	"-p" -> (set! options.profile #t)
	_ -> #u)))

(define (usage)
  (print-string "
Usage: compile <irken-src-file> [options]
 -c : don't compile .c file
 -v : verbose (very!) output
 -t : generate trace-printing code
 -f : set CFLAGS for C compiler
 -m : debug macro expansion
 -O : tell gcc to optimize
 -p : generate profile-printing code
"))

(defmacro verbose
  (verbose item ...) -> (if context.options.verbose (begin item ... #u)))

(define (main)
  (when (< sys.argc 2)
	(usage)
	(raise (:args)))
  (let ((context (make-context))
	(_ (get-options sys.argv context.options))
	(transform (transformer context))
	(path sys.argv[1])
	(base (find-base path))
	(opath (string-append base ".c"))
	(forms0 (read-file path))
	(forms1 (prepend-standard-macros forms0 context))
	(exp0 (sexp:list forms1))
	(_ (verbose (pp 0 exp0) (newline)))
	(exp1 (transform exp0))
	(_ (verbose (pp 0 exp1) (newline)))
	(node0 (walk exp1))
	(node1 (apply-substs node0))
	;; clear some memory usage
	(_ (set! exp0 (sexp:int 0)))
	(_ (set! exp1 (sexp:int 0)))
	(_ (set! forms0 '()))
	(_ (set! forms1 '()))
	;;(_ (begin (print-string "after subst:\n") (pp-node node0)))
	(_ (rename-variables node1))
	;;(_ (begin (pp-node node0) (newline)))
	(node2 (do-one-round node1 context))
	;;(_ (begin (print-string "after first round:\n") (pp-node node1)))
	(noden (do-one-round node2 context))
	;; try to free up some memory
	(_ (set! node0 (node/sequence '())))
	(_ (set! node1 (node/sequence '())))
	(_ (set! node2 (node/sequence '())))	
	(_ (set! context.funs (tree/empty)))
	(_ (find-leaves noden))
	(_ (verbose (print-string "after second round:\n") (pp-node noden)))
	;; rebuild the graph yet again, so strongly will work.
	(_ (build-dependency-graph noden context))
	;;(_ (print-graph context.dep-graph))
	;; strongly-connected components is needed by the typing phase
	(_ (print-string "strongly-connected components:\n"))
	(strong (strongly context.dep-graph))
	(_ (verbose (printn strong)))
	(_ (set! context.scc-graph strong))
	(_ (print-string "typing...\n"))
	(type0 (type-program noden context))
	(_ (verbose (print-string "\n-- after typing --\n") (pp-node noden) (newline)))
	(_ (print-string "cps...\n"))
	(cps (compile noden context))
	(_ (set! noden (node/sequence '()))) ;; go easier on memory
	(ofile (file/open-write opath #t #o644))
	(o (make-writer ofile)))
    (verbose
     (print-string "\n-- RTL --\n")
     (print-insn cps 0)
     (newline)
     (print-string "\n-- datatypes --\n")
     (alist/iterate
      (lambda (name dt)
	(print-datatype dt))
      context.datatypes)
     ;;(print-string "\n-- variables --\n")
     ;;(print-vars context)
     (print-string "\n-- labels --\n")
     (printn context.labels)
     (print-string "\n-- records --\n")
     (printn context.records)
     (print-string "\n-- symbols --\n")
     (alist/iterate
      (lambda (sym index)
	(print-string (format "  " (int index) " : " (sym sym) "\n")))
      context.symbols)
     (print-string "\n-- variant labels --\n")
     (alist/iterate
      (lambda (sym index)
	(print-string (format "  " (int index) " : " (sym sym) "\n")))
      context.variant-labels)
     (print-string "\n-- exceptions --\n")
     (alist/iterate
      (lambda (name type)
	(print-string (format "  " (sym name) " : " (type-repr (apply-subst type)) "\n")))
	context.exceptions)
     )
    (print-string "\n-- C output --\n")
    (print-string " : ") (print-string opath) (newline)
    (for-each (lambda (path)
		(o.write (format "#include <" path ">")))
	      (reverse context.cincludes))
    (for-each o.write (reverse context.cverbatim))
    (match (get-header-parts) with
      (:header part0 part1 part2)
      -> (begin (o.copy part0)
		(emit-constructed o context)
		(if context.options.profile (emit-profile-0 o context))
		(emit-registers o context)
		(o.copy part1)
		(o.copy part2)
		(emit o cps context)))
    (emit-lookup-field o context)
    (if context.options.profile (emit-profile-1 o context))
    (print-string "done.\n")
    (o.close)
    (cond ((not context.options.nocompile)
	   (print-string "compiling...\n")
	   (gcc base context.options)
	   #u
	   )
	  )
    )
  )
  
(main)
