;; -*- Mode: Irken -*-

;; go through all the signatures and constants from all the interfaces,
;;  and emit code for them.  hopefully we can use macros to let the compiler
;;  turn these into The Right Thing for each backend.

;; sig -> c-backend:
;; (sig srand (int -> void))
;; => (define (srand arg0) (%ffi2 srand arg0))
;;
;; would be nice to have names for the args.  consider
;;   changing the spec to permit this.
;;
;; con -> c-backend:
;; (con S2N_BLOCKED_ON_READ 1)
;; => (define S2N_BLOCKED_ON_READ 1)
;;

(define ctype->sexp
  (ctype:name name)    -> (sexp:symbol name)
  (ctype:int size s?)  -> (sexp:symbol 'int)
  (ctype:array size t) -> (sexp (sexp:symbol 'cref) (ctype->sexp t))
  (ctype:pointer t)    -> (sexp (sexp:symbol 'cref) (ctype->sexp t))
  (ctype:struct name)  -> (sexp (sexp:symbol 'struct) (sexp:symbol name))
  (ctype:union name)   -> (sexp (sexp:symbol 'union) (sexp:symbol name))
  )

(define (argnames n)
  (map (lambda (x)
         (sexp:symbol
          (string->symbol
           (format "arg" (int x)))))
       (range n)))

;; because of the subtleties of transform.scm, the macros cannot expand
;; to the usual 'define' form, we need to expand the macro to be the
;; body of the define, like this:
;;
;; (define getpid (macro-magic ...))
;;

(define ctype->rtype-code
  (ctype:int _ _)    -> #\i
  (ctype:name 'void) -> #\u
  (ctype:name 'char) -> #\c
  (ctype:pointer _)  -> #\p
  x -> (error1 "ctype->rtype-code: bad rtype" (ctype-repr x))
  )

(define (autoffi interface)
  (let ((info (require-ffi* interface))
        (forms '()))

    (define (iface-prefix name)
      (string->symbol (format (sym interface) "/" (sym name))))

    (define (iface-includes)
      (map (lambda (path) (sexp (sexp:symbol 'cinclude) (sexp:string path)))
           info.includes))

    (set! the-context.ffi-count (+ 1 the-context.ffi-count))

    (match the-context.options.backend with
      (backend:bytecode)
      -> (PUSH forms (sexp (sexp:symbol 'require-ffi) (sexp (sexp:symbol 'quote) (sexp:symbol interface))))
      (backend:c)
      -> (set! forms (append (iface-includes) forms))
      (backend:llvm)
      -> (set! forms (append (iface-includes) forms))
      )

    ;; constant definitions
    (info.cons::iterate
     (lambda (k v)
       (PUSH forms (sexp (sexp:symbol 'define) (sexp:symbol k) (sexp:int v)))))

    ;; bytecode needs a runtime call to ``require-ffi``

    ;; function/variable definitions
    (info.sigs::iterate
     (lambda (k v)
       (match v with
         (csig:fun name rtype argtypes)
         -> (let ((ztname (zero-terminate (symbol->string name))))
              (PUSH forms
                  (sexp (sexp:symbol 'define)
                        (sexp:symbol (iface-prefix name))
                        (sexp (sexp:symbol 'build-ffi-fun)
                              (sexp:symbol name)
                              (sexp:string ztname)
                              (ctype->sexp rtype)
                              (sexp:char (ctype->rtype-code rtype))
                              (sexp:int (length argtypes))
                              (sexp:list (map ctype->sexp argtypes))
                              (sexp:list (argnames (length argtypes))))
                        )))
         (csig:obj name obtype)
         -> (let ((ztname (zero-terminate (symbol->string name))))
              (PUSH forms
                    (sexp (sexp:symbol 'define)
                          (sexp:symbol (iface-prefix name))
                          (sexp (sexp:symbol 'build-ffi-ob)
                                (sexp:symbol name)
                                (sexp:string ztname)
                                (ctype->sexp obtype)
                                (sexp:char (ctype->code obtype))
                                )))
              )
         )))
    (notquiet
     (printf "autoffi forms = \n")
     (pp (sexp:list forms) 100))
    (reverse forms)
    ))
