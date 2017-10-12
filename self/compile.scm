;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/symbol.scm")
(include "lib/io.scm")
(include "lib/os.scm")
(include "lib/alist.scm")
(include "lib/frb.scm")
(include "lib/lisp_reader.scm")
(include "lib/cmap.scm")
(include "lib/counter.scm")
(include "lib/stack.scm")
(include "lib/set.scm")

(include "self/backend.scm")
(include "self/autoffi.scm")

(define (find-base path)
  (let ((parts (string-split path #\.))
	(rparts (reverse parts)))
    (if (not (string=? (first rparts) "scm"))
	(error1 "find-base" path)
	(string-join (reverse (cdr rparts)) "."))))

(define (read-file-contents ifile)
  (let loop ((buf (file/read-buffer ifile))
	     (l '()))
    (cond ((= (string-length buf) 0) (string-concat (reverse l)))
	  (else (loop (file/read-buffer ifile)
		      (list:cons buf l))))))

(define (join-paths a b)
  (let ((alen (string-length a)))
    (if (char=? #\/ (string-ref a (- alen 1)))
	(format a b)
	(format a "/" b))))

(define find-file
  () name
  -> (raise (:FileNotFound "file not found" name))
  (dir . dirs) name
  -> (try
      (let ((name0 (join-paths dir name)))
	(when the-context.options.verbose
	      (printf "trying " name0 "\n"))
	(file/open-read name0))
      except
      (:OSError _) -> (find-file dirs name)
      ))

(define (read-file path)
  (let ((file (file/open-read path))
	(result (reader path (lambda () (file/read-char file)))))
    result))

(define (find-and-read-file path)
  ;;(printf "reading file '" path "'\n")
  (let ((file (find-file the-context.options.include-dirs path)))
    (reader path (lambda () (file/read-char file)))))

(define (getenv-or var default)
  (let ((val (getenv var)))
    (if (= 0 (string-length val))
	default
	val)))

(include "self/flags.scm")

(define (invoke-cc base paths options extra)
  (let ((cc (getenv-or "CC" CC))
	(cflags (getenv-or "CFLAGS" CFLAGS))
	(cflags (format cflags " " (if options.optimize "-O" "") " " options.extra-cflags))
        (libs (format (join " " (map (lambda (lib) (format "-l" lib)) options.libraries))))
	(cmd (format cc " " cflags " " (join " " paths) " " extra " " libs " -o " base)))
    (notquiet (print-string (format "system: " cmd "\n")))
    (if (not (= 0 (system cmd)))
        (raise (:CCFailed cmd))
        #u)))

(define (get-options argv options)
  (let ((filename-index 1))
    (for-range
	i (vector-length argv)
	(match sys.argv[i] with
	  "-c" -> (set! options.nocompile #t)
	  "-v" -> (set! options.verbose #t)
	  "-t" -> (set! options.trace #t)
	  "-f" -> (begin
		    (set! i (+ i 1))
		    (set! options.extra-cflags argv[i]))
	  "-I" -> (begin
		    (set! i (+ i 1))
		    (PUSH options.include-dirs argv[i]))
          "-l" -> (begin
                    (set! i (+ i 1))
                    (PUSH options.libraries argv[i]))
          "-O" -> (begin
                    (set! i (+ i 1))
                    (set! options.opt-rounds (string->int argv[i])))
	  "-m" -> (set! options.debugmacroexpansion #t)
	  "-dt" -> (set! options.debugtyping #t)
	  "-ni" -> (set! options.noinline #t)
	  "-p" -> (set! options.profile #t)
	  "-n" -> (set! options.noletreg #t)
	  "-q" -> (set! options.quiet #t)
	  "-nr" -> (set! options.no-range-check #t)
          ;; XXX make these mutually exclusive?
	  "-llvm" -> (set! options.backend (backend:llvm))
          "-b" -> (set! options.backend (backend:bytecode))
          "-h"  -> (usage)
          "-help" -> (usage)
	  x -> (if (char=? #\- (string-ref x 0) )
		   (raise (:UnknownOption "Unknown option" x))
		   (set! filename-index i))
	  ))
    (set-verbose-gc (not options.quiet))
    filename-index))

(define (usage)
  (printf "
Usage: compile <irken-src-file> [options]
 -c : don't compile .c file
 -v : verbose (very!) output
 -t : generate trace-printing code (currently unimplemented)
 -f : set CFLAGS for C compiler
 -I : add include search directory
 -l : add a link library
 -m : debug macro expansion
 -dt : debug typing
 -ni : no inlining
 -p : generate profile-printing code
 -n : disable letreg optimization
 -O : rounds of optimization (default: 3)
 -q : quiet the compiler
 -nr : no range check (e.g. vector access)
 -h : display this usage
 -llvm : compile using the LLVM backend.
 -b : compile using the bytecode backend.

default flags:
  CC='" CC "'
  CFLAGS='" CFLAGS "'
"
))

(define (warning msg)
  (printf "warning: " msg "\n"))

(defmacro verbose
  (verbose item ...) -> (if the-context.options.verbose (begin item ... #u)))

(defmacro notquiet
  (notquiet item ...) -> (if (not the-context.options.quiet) (begin item ... #u)))

(define the-context (make-context))

(define (main)
  (when (< sys.argc 2)
	(usage)
	(raise (:NotEnoughArgs)))
  (let ((filearg (get-options sys.argv the-context.options))
	(transform (transformer))
	(path sys.argv[filearg])
	(base (find-base path))
        (_ (notquiet (printf "read...\n")))
	(forms0 (read-file path))
	(forms1 (prepend-standard-macros forms0))
	(exp0 (sexp:list forms1))
	(_ (verbose (pp exp0 80) (newline)))
        (_ (notquiet (printf "transform...\n")))
	(exp1 (transform exp0))
	(_ (verbose (pp exp1 80) (newline)))
	(node0 (walk exp1))
	(node1 (apply-substs node0))
	;; clear some memory usage
	(_ (set! exp0 (sexp:int 0)))
	(_ (set! exp1 (sexp:int 0)))
	(_ (set! forms0 '()))
	(_ (set! forms1 '()))
	(_ (rename-variables node1))
	(_ (optimize-nvcase node1))
        (noden (do-n-rounds node1 the-context.options.opt-rounds))
	;; try to free up some memory
	(_ (set! node0 (node/sequence '())))
	(_ (set! node1 (node/sequence '())))
	(_ (set! the-context.funs (tree/empty)))
	(_ (find-tail noden))
	(_ (find-leaves noden))
	(_ (find-free-refs noden))
	(_ (verbose (print-string "after opt:\n") (pp-node noden)))
	;; rebuild the graph yet again, so strongly will work.
        (_ (notquiet (printf "depgraph...\n")))
	(_ (build-dependency-graph noden))
	;;(_ (print-graph the-context.dep-graph))
	;; strongly-connected components is needed by the typing phase
	;;(_ (print-string "strongly-connected components:\n"))
	(strong (strongly the-context.dep-graph))
	(_ (verbose (printn strong)))
	(_ (set! the-context.scc-graph strong))
        (_ (notquiet (printf "typing...\n")))
	(type0 (type-program noden))
	;;(type-map (collect-all-types noden))
	;;(_ (print-type-tree noden))
	(_ (verbose (print-string "\n-- after typing --\n") (pp-node noden) (newline)))
	(_ (remove-onearmed-nvcase noden)) ;; safe after typing
        (_ (notquiet (printf "cps...\n")))
	(cps (compile noden))
	(_ (set! noden (node/sequence '()))) ;; go easier on memory
	)
    (verbose
     (print-string "\n-- RTL --\n")
     (print-insn cps 0)
     (newline)
     (print-string "\n-- datatypes --\n")
     (alist/iterate
      (lambda (name dt)
	(print-datatype dt))
      the-context.datatypes)
     (print-string "\n-- typealiases --\n")
     (alist/iterate
      (lambda (name alias)
	(print-string (format "  " (sym name) " : " (scheme-repr alias) "\n")))
      the-context.aliases)
     (print-string "\n-- variables --\n")
     (print-vars)
     (print-string "\n-- labels --\n")
     (printn the-context.labels)
     (print-string "\n-- records --\n")
     (printn the-context.records)
     (print-string "\n-- symbols --\n")
     (alist/iterate
      (lambda (sym index)
	(print-string (format "  " (int index) " : " (sym sym) "\n")))
      the-context.symbols)
     (print-string "\n-- variant labels --\n")
     (alist/iterate
      (lambda (sym index)
	(print-string (format "  " (int index) " : " (sym sym) "\n")))
      the-context.variant-labels)
     (print-string "\n-- exceptions --\n")
     (alist/iterate
      (lambda (name type)
	(print-string (format "  " (sym name) " : " (type-repr (apply-subst type)) "\n")))
	the-context.exceptions)
     (printf "\n-- FFI --\n")
     (dump-ffi-info)
     )

    (notquiet (printf "backend...\n"))

    (compile-with-backend base cps)
    )
  )

(main)
