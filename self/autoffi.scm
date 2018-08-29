;; -*- Mode: Irken -*-

(require "lib/cmap.scm")

;; AutoFFI - when the compiler comes across a 'require-ffi' form, it needs
;;   to emit code that will Do the Right Thing depending on which backend
;;   is being used.  We do this by inserting macro calls with enough information
;;   to create the needed code for all backends.  The macros (and their translations)
;;   are defined in lib/ctype.scm.
;;
;; currently, the following macros are used:
;;
;; fetch-ffi-constant: FFI constants like O_TRUNC
;; build-ffi-fun: FFI functions like open()
;; build-ffi-ob: FFI objects like `errno`

;; aka 'unparse-ctype'
(define (cint->sexp cint signed?)
  (sexp:symbol
   (match cint signed? with
     (cint:int)      #t -> 'int
     (cint:int)      #f -> 'uint
     (cint:short)    #t -> 'short
     (cint:short)    #f -> 'ushort
     (cint:long)     #t -> 'long
     (cint:long)     #f -> 'ulong
     (cint:longlong) #t -> 'longlong
     (cint:longlong) #f -> 'ulonglong
     (cint:width 1)  #t -> 'i8
     (cint:width 1)  #f -> 'u8
     (cint:width 2)  #t -> 'i16
     (cint:width 2)  #f -> 'u16
     (cint:width 4)  #t -> 'i32
     (cint:width 4)  #f -> 'u32
     (cint:width 8)  #t -> 'i64
     (cint:width 8)  #f -> 'u64
     (cint:width 16) #t -> 'i128
     (cint:width 16) #f -> 'u128
     (cint:width 32) #t -> 'i256
     (cint:width 32) #f -> 'u256
     _ _ -> (raise (:AutoFFI/UnsupportedCint cint))
     )))

(define ctype->sexp
  (ctype:name name)    -> (sexp:symbol name)
  (ctype:int x s?)     -> (cint->sexp x s?)
  (ctype:array size t) -> (sexp (sexp:symbol 'cref) (ctype->sexp t))
  (ctype:pointer t)    -> (sexp (sexp:symbol 'cref) (ctype->sexp t))
  (ctype:struct name)  -> (sexp (sexp:symbol 'struct) (sexp:symbol name))
  (ctype:union name)   -> (sexp (sexp:symbol 'union) (sexp:symbol name))
  )

;; special-case: all integer arguments/return-values become 'int
;; (define argtype->sexp
;;   (ctype:int _ s?) -> (sexp:symbol 'int)
;;   arg              -> (ctype->sexp arg)
;;   )

(define (argtype->sexp x)
  (ctype->sexp x))

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

;; There is no need to distinguish between an ffi interface that has been loaded
;;   at runtime vs compile-time: they are considered platform-agnostic.  But there *is*
;;   a need to distinguish between those loaded by the compiler and those loaded by the
;;   program being compiled.  Hence, the `ffi-map` slot in the global context object.

(define (autoffi interface)
  (let ((info (require-ffi* interface))
        (first-time? (not (cmap/present? the-context.ffi-map interface)))
        (forms '()))

    (define (iface-prefix name)
      (string->symbol (format (sym interface) "/" (sym name))))

    (define (iface-includes)
      (map (lambda (path) (sexp (sexp:symbol 'cinclude) (sexp:string path)))
           info.includes))

    (when first-time?
      ;; only emit forms the first time this interface is loaded
      (cmap/add the-context.ffi-map interface)

      (match the-context.options.backend with
        ;; bytecode needs a runtime call to ``require-ffi``
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
         (PUSH forms
               ;; XXX consider using a zero placeholder for all constants when emitting
               ;;  bytecode, to avoid needless differences in bytecode compiled on different
               ;;  platforms.
               (sexp (sexp:symbol 'define) (sexp:symbol k) (sexp:int v)))))

      (match the-context.options.backend with
        ;; with the bytecode backend, we need to set! the values after loading the ffi info,
        ;;  these expressions will hopefully appear after that happens.
        (backend:bytecode)
        -> (info.cons::iterate
            (lambda (k v)
              (PUSH forms
                    (sexp (sexp:symbol 'set!)
                          (sexp:symbol k)
                          (sexp (sexp:symbol 'fetch-ffi-constant) (sexp:symbol k) (sexp:int v))))))
        _ -> #u
        )

      ;; function/variable definitions
      (info.sigs::iterate
       (lambda (visible-name v)
         (match v with
           (csig:fun name rtype argtypes)
           -> (let ((ztname (zero-terminate (symbol->string name))))
                (PUSH forms
                      (sexp (sexp:symbol 'define)
                            (sexp:symbol (iface-prefix visible-name))
                            (sexp (sexp:symbol 'build-ffi-fun)
                                  (sexp:symbol interface)
                                  (sexp:symbol name)
                                  (sexp:string ztname)
                                  (argtype->sexp rtype)
                                  (sexp:char (ctype->rtype-code rtype))
                                  (sexp:int (length argtypes))
                                  (sexp:list (map argtype->sexp argtypes))
                                  (sexp:list (argnames (length argtypes))))
                            )))
           (csig:obj name obtype)
           -> (let ((ztname (zero-terminate (symbol->string name))))
                (PUSH forms
                      (sexp (sexp:symbol 'define)
                            (sexp:symbol (iface-prefix visible-name))
                            (sexp (sexp:symbol 'build-ffi-ob)
                                  (sexp:symbol interface)
                                  (sexp:symbol name)
                                  (sexp:string ztname)
                                  (ctype->sexp obtype)
                                  (sexp:char (ctype->code obtype))
                                  )))
                )
           )))

      ;; (notquiet
      ;;  (printf "autoffi forms = \n")
      ;;  (pp (sexp:list forms) 100))
      )
    (reverse forms)
    ))
