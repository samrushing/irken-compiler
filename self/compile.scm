;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/format.scm")
(include "lib/symbol.scm")
(include "lib/sexp.scm")
(include "lib/io.scm")
(include "lib/os.scm")
(include "lib/stdio.scm")
(include "lib/alist.scm")
(include "lib/frb.scm")
(include "lib/lisp_reader.scm")
(include "lib/cmap.scm")
(include "lib/counter.scm")
(include "lib/stack.scm")
(include "lib/set.scm")
(include "lib/enum.scm")

(include "self/backend.scm")
(include "self/autoffi.scm")

;(include "lib/metadata.scm")
;(include "lib/reflection.scm")

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

(define (parse-dump-spec spec)
  (map string->symbol (string-split spec #\,)))

(defmacro if-dump
  (if-dump phase body ...)
  -> (when (member-eq? phase the-context.options.dump)
       (printf "dump phase: " (sym phase) " {\n")
       body ...
       (newline)
       (printf "}\n")
       #u
       ))

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
	  "-c"    -> (set! options.nocompile #t)
	  "-v"    -> (set! options.verbose #t)
	  "-t"    -> (set! options.trace #t)
	  "-f"    -> (begin (set! i (+ i 1)) (set! options.extra-cflags argv[i]))
	  "-I"    -> (begin (set! i (+ i 1)) (PUSH options.include-dirs argv[i]))
          "-l"    -> (begin (set! i (+ i 1)) (PUSH options.libraries argv[i]))
          "-O"    -> (begin (set! i (+ i 1)) (set! options.opt-rounds (string->int argv[i])))
          "-i"    -> (begin (set! i (+ i 1)) (set! options.inline-threshold (string->int argv[i])))
	  "-m"    -> (set! options.debugmacroexpansion #t)
	  "-dt"   -> (set! options.debugtyping #t)
	  "-ni"   -> (set! options.noinline #t)
	  "-p"    -> (set! options.profile #t)
	  "-n"    -> (set! options.noletreg #t)
	  "-q"    -> (set! options.quiet #t)
	  "-nr"   -> (set! options.no-range-check #t)
          ;; XXX make these mutually exclusive?
	  "-llvm" -> (set! options.backend (backend:llvm))
          "-b"    -> (set! options.backend (backend:bytecode))
          "-h"    -> (usage)
          "-help" -> (usage)
          "-types" -> (set! options.dumptypes #t)
          "-dump" -> (begin (set! i (+ i 1)) (set! options.dump (parse-dump-spec argv[i])))
	  x       -> (if (char=? #\- (string-ref x 0) )
                         (raise (:UnknownOption "Unknown option" x))
                         (set! filename-index i))
	  ))
    (set-verbose-gc (not options.quiet))
    (when options.dumptypes
      ;; disable inlining so every function has a type.
      (set! options.noinline #t))
    filename-index))

(define (usage)
  (printf "
Usage: compile <irken-src-file> [options]
 -c     : don't compile .c file
 -v     : verbose (very!) output
 -t     : generate trace-printing code (C backend only)
 -f     : set CFLAGS for C compiler
 -I     : add include search directory
 -l     : add a link library
 -m     : debug macro expansion
 -dt    : debug typing
 -types : dump all type signatures (do not compile)
 -dump  : comma-separated list from (sexp,expand,ast,typed,cps)
 -ni    : no inlining
 -p     : generate profile-printing code (C backend only)
 -n     : disable letreg optimization
 -O     : rounds of optimization (default: 3)
 -i <n> : set inline threshold (10-20)
 -q     : quiet the compiler
 -nr    : no range check (e.g. vector access)
 -h     : display this usage
 -llvm  : compile using the LLVM backend.
 -b     : compile using the bytecode backend.

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
        (_ (if-dump 'sexp (pp exp0 80)))
        (_ (notquiet (printf "transform...\n")))
	(exp1 (transform exp0))
        (_ (if-dump 'expand (pp exp1 80)))
	(node0 (sexp->node exp1))
	(node1 (apply-substs node0))
        (_ (if-dump 'ast (pp-node node1)))
        (_ (find-unused-fatbar node1))
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
        (_ (if-dump 'opt (pp-node noden)))
	;; build the graph again, so strongly will work.
        (_ (notquiet (printf "depgraph...\n")))
        (_ (build-dependency-graph noden))
        ;;(_ (print-graph the-context.dep-graph))
        ;; strongly-connected components is needed by the typing phase
        ;;(_ (print-string "strongly-connected components:\n"))
        (strong (strongly the-context.dep-graph))
        (_ (verbose (printn strong)))
        (_ (set! the-context.scc-graph strong))
        (_ (notquiet (printf "typing...\n")))
        (_ (type-program noden))
        ;;(type-map (collect-all-types noden))
        ;;(_ (print-type-tree noden))
        (_ (if-dump 'typed (pp-node noden)))
        (_ (remove-onearmed-nvcase noden)) ;; safe after typing
        (_ (notquiet (printf "cps...\n")))
        )
    (if the-context.options.dumptypes
        (dump-types noden)
        (main-compile base noden))
    ))

(define (main-compile base node)
  (let ((cps (compile node)))
    (when (not (eq? (backend:bytecode) the-context.options.backend))
      ;; note: the VM's save/restore mechanism assumes regs
      ;;   are saved in contiguous order, so we cannot trim.
      (trim-free-regs cps))
    (if-dump 'cps (print-cps cps 0) (newline))
    (verbose
     (printf "\n-- datatypes --\n")
     (alist/iterate
      (lambda (name dt)
        (print-datatype dt))
      the-context.datatypes)
     (printf "\n-- typealiases --\n")
     (alist/iterate
      (lambda (name alias)
	(printf (format "  " (sym name) " : " (scheme-repr alias) "\n")))
      the-context.aliases)
     (printf "\n-- variables --\n")
     (print-vars)
     (printf "\n-- labels --\n")
     (tree/inorder
      (lambda (index label)
        (printf "  " (int index) " : " (sym label) "\n"))
      the-context.labels.rev)
     (printf "\n-- records --\n")
     (for-map index sig the-context.records.rev
       (printf "  " (int index) " : (" (join symbol->string " " sig) ")\n"))
     (printf "\n-- symbols --\n")
     (tree/inorder
      (lambda (sym index)
	(printf "  " (int index) " : " (sym sym) "\n"))
      the-context.symbols)
     (printf "\n-- variant labels --\n")
     (alist/iterate
      (lambda (sym index)
	(printf "  " (int index) " : " (sym sym) "\n"))
      the-context.variant-labels)
     (printf "\n-- exceptions --\n")
     (alist/iterate
      (lambda (name type)
	(printf "  " (sym name) " : " (type-repr (apply-subst type)) "\n"))
	the-context.exceptions)
     (printf "\n-- FFI --\n")
     (dump-ffi-info)
     )
    (notquiet (printf "backend...\n"))
    (generate-metadata)
    (compile-with-backend base cps)
    ))

(include "self/c.scm")
(include "self/llvm.scm")
(include "self/bytecode.scm")

(define (compile-with-backend base cps)
  (let ((sources
         (match the-context.options.backend with
           (backend:c)
           -> (compile-to-c base cps)
           (backend:llvm)
           -> (compile-to-llvm base cps)
           (backend:bytecode)
           -> (compile-to-bytecode base cps)
           )))
    (when (and (not (null? sources))
               (not the-context.options.nocompile))
      (notquiet (print-string "compiling...\n"))
      (invoke-cc base sources the-context.options "")
      #u)
    ))

(main)
