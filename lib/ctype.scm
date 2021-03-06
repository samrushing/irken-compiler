;; -*- Mode: Irken -*-

(require-ffi 'libc)

(datatype cint
  (:char)
  (:short)
  (:int)
  (:long)
  (:longlong)
  (:width int) ;; in bytes, not bits
  )

(define (cint-repr t signed?)
  (let ((suffix
         (match t with
           (cint:char)      -> "char"
           (cint:short)     -> "short"
           (cint:int)       -> "int"
           (cint:long)      -> "long"
           (cint:longlong)  -> "longlong"
           (cint:width w)   -> (format (int (* 8 w))))))
    (match t with
      (cint:width _) -> (format (if signed? "i" "u") suffix)
      _              -> (format (if signed? "" "u") suffix)
      )))

;; lp64
(define cint-size
  (cint:char)     -> 1
  (cint:short)    -> 2
  (cint:int)      -> 4
  (cint:long)     -> 8
  (cint:longlong) -> 8
  (cint:width w)  -> w
  )

(datatype ctype
  (:name symbol)   ;; void, thing_t, etc...
  (:int cint bool) ;; size, signed?
  (:array int ctype)
  (:pointer ctype)
  (:struct symbol)
  (:union symbol)
  )

(define ctype-repr
  (ctype:name name)    -> (format "(type " (symbol->string name) ")")
  (ctype:int cint s?)  -> (cint-repr cint s?)
  (ctype:array size t) -> (format "(array " (int size) " " (ctype-repr t) ")")
  (ctype:pointer t)    -> (format "(* " (ctype-repr t) ")")
  (ctype:struct name)  -> (format "(struct " (sym name) ")")
  (ctype:union name)   -> (format "(union " (sym name) ")")
  )

(datatype cfield
  (:t int symbol ctype) ;; offset name type
  )

(define cfield-print
  (cfield:t offset name t)
  -> (printf "  [" (lpad 3 (int offset)) "] " (sym name) " : " (ctype-repr t) "\n"))

(datatype cdef
  ;; size name fields
  (:struct int symbol (list cfield))
  (:union  int symbol (list cfield))
  )

(define cdef-print
  (cdef:struct size name fields)
  -> (begin
       (printf "struct " (sym name) " {\n")
       (for-list field fields
         (cfield-print field))
       (printf "} [" (lpad 3 (int size)) "]\n"))
  (cdef:union size name fields)
  -> (begin
       (printf "union " (sym name) " {\n")
       (for-list field fields
         (cfield-print field))
       (printf "} [" (lpad 3 (int size)) "]\n"))
  )

;; c function/object signature
(datatype csig
  (:fun symbol ctype (list ctype)) ;; name return-type arg-types
  (:obj symbol ctype)              ;; name type
  )

(define csig-print
  (csig:fun name rtype argtypes)
  -> (printf "(" (sym name) " " (join ctype-repr " " argtypes) " -> " (ctype-repr rtype) ")\n")
  (csig:obj name otype)
  -> (printf "(" (sym name) " " (ctype-repr otype) ")\n")
  )

(define parse-ctype
  ;; many integer types
  (sexp:symbol 'int)       -> (ctype:int (cint:int) #t)
  (sexp:symbol 'uint)      -> (ctype:int (cint:int) #f)
  (sexp:symbol 'short)     -> (ctype:int (cint:short) #t)
  (sexp:symbol 'ushort)    -> (ctype:int (cint:short) #f)
  (sexp:symbol 'long)      -> (ctype:int (cint:long) #t)
  (sexp:symbol 'ulong)     -> (ctype:int (cint:long) #f)
  (sexp:symbol 'longlong)  -> (ctype:int (cint:longlong) #t)
  (sexp:symbol 'ulonglong) -> (ctype:int (cint:longlong) #f)
  (sexp:symbol 'i8)        -> (ctype:int (cint:width 1) #t)
  (sexp:symbol 'u8)        -> (ctype:int (cint:width 1) #f)
  (sexp:symbol 'i16)       -> (ctype:int (cint:width 2) #t)
  (sexp:symbol 'u16)       -> (ctype:int (cint:width 2) #f)
  (sexp:symbol 'i32)       -> (ctype:int (cint:width 4) #t)
  (sexp:symbol 'u32)       -> (ctype:int (cint:width 4) #f)
  (sexp:symbol 'i64)       -> (ctype:int (cint:width 8) #t)
  (sexp:symbol 'u64)       -> (ctype:int (cint:width 8) #f)
  (sexp:symbol 'i256)      -> (ctype:int (cint:width 32) #t)
  (sexp:symbol 'u256)      -> (ctype:int (cint:width 32) #f)
  (sexp:list ((sexp:symbol 'int) (sexp:list ((sexp:int size) (sexp:int signed?)))))
  -> (ctype:int (cint:width size) (if (= signed? 1) #t #f))
  (sexp:list ((sexp:symbol '*) sub))
  -> (ctype:pointer (parse-ctype sub))
  (sexp:list ((sexp:symbol 'struct) (sexp:symbol name)))
  -> (ctype:struct name)
  (sexp:list ((sexp:symbol 'union) (sexp:symbol name)))
  -> (ctype:union name)
  (sexp:list ((sexp:symbol 'array) sub (sexp:list ((sexp:int size)))))
  -> (ctype:array size (parse-ctype sub))
  ;; new array form (i.e. ctype2-repr)
  (sexp:list ((sexp:symbol 'array) (sexp:int size) sub))
  -> (ctype:array size (parse-ctype sub))
  ;; temporary, all function types are void
  (sexp:list ((sexp:symbol 'fun) . _))
  -> (ctype:name 'void)
  (sexp:symbol name)
  -> (ctype:name name)
  x -> (error1 "malformed type" (repr x))
  )

(define (parse-spec info forms)

  (define parse-field
    (sexp:list ((sexp:int size) (sexp:symbol name) ftype))
    -> (cfield:t size name (parse-ctype ftype))
    x -> (error1 "malformed field" (repr x))
    )

  (define parse-struct
    'struct ((sexp:symbol name) (sexp:int size) . fields)
    -> (info.structs::add name (cdef:struct size name (map parse-field fields)))
    'union ((sexp:symbol name) (sexp:int size) . fields)
    -> (info.unions::add name (cdef:union size name (map parse-field fields)))
    kind x -> (error1 "malformed struct" (:pair kind x))
    )

  (define arrow-type?
    ()                    -> #f
    ((sexp:symbol '->) _) -> #t
    (hd . tl)             -> (arrow-type? tl)
    )

  ;; convert all integer arguments to 'int' type.
  ;; [this will probably need revisiting]
  (define frob-int-arg
    (ctype:int _ _) -> (ctype:int (cint:int) #t)
    t -> t
    )

  (define parse-sig*
    name (sexp:list sig)
    -> (if (arrow-type? sig)
           (let ((siglen (length sig))
                 (rtype (nth sig (- siglen 1)))
                 (args (slice sig 0 (- siglen 2))))
             (csig:fun name
                       (parse-ctype rtype)
                       ;;(map frob-int-arg (map parse-ctype args))
                       (map parse-ctype args)
                       ))
           (csig:obj name (parse-ctype (sexp:list sig))))
    name sig
    -> (csig:obj name (parse-ctype sig))
    name _ -> (error1 "malformed sig" name)
    )

  (define parse-sig
    ((sexp:symbol name) sig)
    -> (info.sigs::add name (parse-sig* name sig))
    ((sexp:list ((sexp:symbol name0) (sexp:symbol name1))) sig)
    -> (let ((sig0 (parse-sig* name1 sig)))
         (info.sigs::add name0 sig0))
    x -> (error1 "malformed sig" (repr (sexp:list x)))
    )

  (define parse-con
    ((sexp:symbol name) (sexp:int val))
    -> (info.cons::add name val)
    x -> (error1 "malformed constant" x)
    )

  (define parse-tdef
    ((sexp:symbol name) t)
    -> (info.tdefs::add name (parse-ctype t))
    x -> (error1 "malformed tdef" x)
    )

  (define parse-string-list
    acc ()
    -> (reverse acc)
    acc ((sexp:string item) . rest)
    -> (parse-string-list (list:cons item acc) rest)
    acc x
    -> (raise (:FFISpec/malformed x))
    )

  (define parse-form
    (sexp:list ((sexp:symbol 'struct) . rest))
    -> (parse-struct 'struct rest)
    (sexp:list ((sexp:symbol 'union) . rest))
    -> (parse-struct 'union rest)
    (sexp:list ((sexp:symbol 'sig) . rest))
    -> (parse-sig rest)
    (sexp:list ((sexp:symbol 'con) . rest))
    -> (parse-con rest)
    (sexp:list ((sexp:symbol 'tdef) . rest))
    -> (parse-tdef rest)
    (sexp:list ((sexp:symbol 'includes) . rest))
    -> (set! info.includes (parse-string-list '() rest))
    (sexp:list ((sexp:symbol 'cflags) . rest))
    -> (set! info.cflags (parse-string-list '() rest))
    (sexp:list ((sexp:symbol 'lflags) . rest))
    -> (set! info.lflags (parse-string-list '() rest))
    (sexp:list ((sexp:symbol 'dll) (sexp:string dll)))
    -> (set! info.dll dll)
    x -> (error1 "malformed spec file" (repr x))
    )

  (for-list form forms
    (parse-form form))
  )

;; XXX do we need to distinguish between compile-time and run-time use
;;     of this registry?
(define (make-ffi-info)
  ;; we have to use two maps for struct/union because
  ;; they sometimes have the same name (e.g. in6_addr)
  ;; [alternatively we could use a pair as key?]
  {structs  = (map-maker symbol-index-cmp)
   unions   = (map-maker symbol-index-cmp)
   cons     = (map-maker symbol-index-cmp)
   sigs     = (map-maker symbol-index-cmp)
   tdefs    = (map-maker symbol-index-cmp)
   includes = '()
   cflags   = '()
   lflags   = '()
   dll      = ""
   })

(define ffi-info (make-ffi-info))

(define (merge-ffi-info info)
  (ffi-info.structs::union info.structs)
  (ffi-info.unions::union info.unions)
  (ffi-info.cons::union info.cons)
  (ffi-info.sigs::union info.sigs)
  (ffi-info.tdefs::union info.tdefs)
  ;; XXX this should probably be a set rather than a list
  (set! ffi-info.includes (append ffi-info.includes info.includes))
  (append! ffi-info.cflags info.cflags)
  (append! ffi-info.lflags info.lflags)
  )

(define (dump-ffi-info)
  (printf "includes: " (join " " ffi-info.includes) "\n")
  (printf "cflags: " (join " " ffi-info.cflags) "\n")
  (printf "lflags: " (join " " ffi-info.lflags) "\n")
  (printf "dll: " (string ffi-info.dll) "\n")
  (printf "sigs:\n")
  (map csig-print (ffi-info.sigs::values))
  (printf "defs:\n")
  (map cdef-print (ffi-info.structs::values))
  (map cdef-print (ffi-info.unions::values))
  (printf "constants:\n")
  (ffi-info.cons::iterate
   (lambda (k v)
     (printf (lpad 5 (int v)) " " (sym k) "\n")))
  (printf "typedefs:\n")
  (ffi-info.tdefs::iterate
   (lambda (k v)
     (printf "  " (sym k) " " (ctype-repr v) "\n")))
  )

(define FFI-PATH (getenv-or "IRKENLIB" "/usr/local/lib/irken/ffi/"))

(define (read-spec name)
  (let ((path0 (format FFI-PATH (sym name) "_ffi.scm")))
    ;;(printf "loading ffi spec for " (sym name) "\n")
    ;;(printf "  from " path0 "\n")
    (%backend bytecode
      (let ((fparts0 (%%cexp (string -> (list string)) "readf" path0)))
        (if (null? fparts0)
            ;; VM failed to read the file.  Try local copy (bootstrapping).
            (let ((path1 (format "ffi/" (sym name) "_ffi.scm"))
                  (fparts1 (%%cexp (string -> (list string)) "readf" path1)))
              (if (null? fparts1)
                  ;; give up
                  (raise (:NoFFI "unable to read FFI" name))
                  (set! fparts0 fparts1))))
        (read-string (string-concat fparts0))))
    (%backend (c llvm)
      ;; using stdio avoids a circular dependency on posix.ffi.
      (try
       (let ((file0 (stdio/open-read path0)))
         (reader path0 (lambda () (stdio/read-char file0))))
       except (:StdioOpenRead _ _)
       -> (let ((path1 (format "ffi/" (sym name) "_ffi.scm"))
                (file1 (stdio/open-read path1)))
            (reader path1 (lambda () (stdio/read-char file1))))
       ))
    ))

;; how many interfaces are needed in this compilation unit?
(define (interface-count)
  (match (%%compile-time-meta 'ffi-interfaces) with
    (sexp:list ifaces)
    -> (length ifaces)
    x -> (error1 "bad result from %%compile-time-meta" (repr x))
    ))

;; meant for *compile-time* use.
;; XXX this distinction not needed.
(define require-ffi*
  (let ((loaded (map-maker magic-cmp)))
    (lambda (name)
      (match (loaded::get name) with
        (maybe:yes info)
        -> info
        (maybe:no)
        -> (try
            (let ((forms (read-spec name))
                  (info (make-ffi-info)))
              (parse-spec info forms)
              (loaded::add name info)
              (merge-ffi-info info)
              (%backend bytecode
                (if (= (interface-count) (length (loaded::keys)))
                    ;; we have all the interfaces, so...
                    (update-sizeoff-table)))
              info
              )
            except
            (:OSError x)
            -> (begin
                 (printf "unable to load spec for interface " (sym name) "\n")
                 (raise (:OSError x)))
            )
        ))))

;; this is meant for *runtime* use.
;; XXX this distinction not needed.
(define (require-ffi name)
  (require-ffi* name))

(define *word-size* (get-word-size))
(define *int-size* (get-int-size))

(define base-type-size
  'void -> (error "void has no size")
  'char -> 1
  _ -> *word-size*
  )

(define (lookup-struct-size name)
  (match (ffi-info.structs::get name) with
    (maybe:yes (cdef:struct size _ _)) -> size
    _ -> (error1 "lookup-struct-size: unknown struct" name)
    ))

(define (lookup-union-size name)
  (match (ffi-info.unions::get name) with
    (maybe:yes (cdef:union size _ _)) -> size
    _ -> (error1 "lookup-union-size: unknown union" name)
    ))

(define (lookup-tdef-size name)
  (match (ffi-info.tdefs::get name) with
    (maybe:yes (ctype:int ci _)) -> (cint-size ci)
    _ -> (base-type-size name)
    ))

(define (lookup-struct-fields name)
  (match (ffi-info.structs::get name) with
    (maybe:yes (cdef:struct _ _ fields)) -> fields
    _ -> (error1 "lookup-struct-fields: unknown struct" name)
    ))

(define (lookup-union-fields name)
  (match (ffi-info.unions::get name) with
    (maybe:yes (cdef:union _ _ fields)) -> fields
    _ -> (error1 "lookup-union-fields: unknown union" name)
    ))

(define lookup-field
  name ()
  -> (error1 "lookup-field failed" name)
  name ((cfield:t offset name0 ctype) . tl)
  -> (if (eq? name name0)
         {off=offset ctype=ctype}
         (lookup-field name tl))
  )

(define (lookup-tdef name)
  (ffi-info.tdefs::get name))

(define (lookup-constant name) : (symbol -> int)
  (match (ffi-info.cons::get name) with
    (maybe:yes val) -> val
    (maybe:no) -> (error1 "lookup-constant: unknown name" name)))

(define ctype->size
  (ctype:pointer _)    -> *word-size*
  (ctype:int ci _)     -> (cint-size ci)
  (ctype:array size t) -> (* size (ctype->size t))
  (ctype:name name)    -> (lookup-tdef-size name)
  (ctype:struct name)  -> (lookup-struct-size name)
  (ctype:union name)   -> (lookup-union-size name)
  )

;; --- runtime offset calculation ---

(define cref-field
  name {off=off0 ctype=(ctype:struct sname)}
  -> (let ((ref (lookup-field name (lookup-struct-fields sname))))
       {off=(+ off0 ref.off) ctype=ref.ctype})
  name {off=off0 ctype=(ctype:union sname)}
  -> (let ((ref (lookup-field name (lookup-union-fields sname))))
       {off=(+ off0 ref.off) ctype=ref.ctype})
  _ ref -> (error1 "cref-field: type is not struct/union" (ctype-repr ref.ctype))
  )

(define cref-aref
  index {off=off0 ctype=(ctype:array size ctype0)}
  -> {off=(+ off0 (* (ctype->size ctype0) index)) ctype=ctype0}
  _ ref -> (error1 "cref-aref: type is not an array" (ctype-repr ref.ctype))
  )

(define expand-cref
  ref0 ()
  -> ref0
  ref0 ((sexp:symbol fname) . tl)
  -> (expand-cref (cref-field fname ref0) tl)
  ref0 ((sexp:int index) . tl)
  -> (expand-cref (cref-aref index ref0) tl)
  ref0 (sexp . _)
  -> (error1 "cref: elems must be symbol or integer" (repr sexp))
  )

(defmacro cref
  (cref ctype a ...)
  -> (expand-cref {off=0 ctype=ctype} (%%sexp a ...)))

;; convert a ctype to an `op_cget` code (used by the VM).
(define ctype->code
  (ctype:int (cint:char) #f)        -> #\B
  (ctype:int (cint:char) #t)        -> #\b
  (ctype:int (cint:int) #f)         -> #\i
  (ctype:int (cint:int) #t)         -> #\I
  (ctype:int (cint:short) #f)       -> #\H
  (ctype:int (cint:short) #t)       -> #\h
  (ctype:int (cint:long) #f)        -> #\l
  (ctype:int (cint:long) #t)        -> #\L
  (ctype:int (cint:longlong) #f)    -> #\n
  (ctype:int (cint:longlong) #f)    -> #\N
  (ctype:int (cint:width 1) #f)     -> #\B
  (ctype:int (cint:width 1) #t)     -> #\b
  (ctype:int (cint:width 2) #f)     -> #\H
  (ctype:int (cint:width 2) #t)     -> #\h
  (ctype:int (cint:width 4) #f)     -> #\M
  (ctype:int (cint:width 4) #t)     -> #\m
  (ctype:int (cint:width 8) #f)     -> #\Q
  (ctype:int (cint:width 8) #t)     -> #\q
  (ctype:array _ (ctype:name char)) -> #\s
  (ctype:array _ _)                 -> #\p
  (ctype:pointer _)                 -> #\p
  (ctype:struct _)                  -> #\p
  (ctype:union _)                   -> #\p
  (ctype:name 'char)                -> #\c
  (ctype:name 'void)                -> (raise (:VoidDereference))
  x                                 -> (raise (:StrangeCtype (ctype-repr x)))
  )

;; convert an irken 'sexp type' to an `op_cget` code (used by the VM).
(define (irken-type->code type-sexp)
  (char->ascii
   (match type-sexp with
     'int       -> #\i
     'uint      -> #\I
     'short     -> #\h
     'ushort    -> #\H
     'long      -> #\l
     'ulong     -> #\L
     'longlong  -> #\n
     'ulonglong -> #\N
     'u8        -> #\B
     'i8        -> #\b
     'u16       -> #\H
     'i16       -> #\h
     'u32       -> #\M
     'i32       -> #\m
     'u64       -> #\Q
     'i64       -> #\q
     'string    -> #\s
     'char      -> #\c
     x          -> (raise (:StrangeType x))
     )))

;; macros that expand into implementations of the foreign functions
;;   declared in the spec file.  the macro 'calls' are built by
;;   self/autoffi.scm.
;;
;; the c/llvm expansions are much simpler because most of the work
;;   is done at compile-time.  The bytecode is more complex because
;;   information about sizes/structs/etc is only available at runtime.

(%backend (c llvm)

  (defmacro build-ffi-fun
    (build-ffi-fun iface name ztname rtype rcode nargs argtypes (formal0 ...))
    -> (lambda (formal0 ...) (%ffi2 name formal0 ...))
    )

  (defmacro build-ffi-ob
    (build-ffi-ob iface name ztname obtype obcode)
    -> (%ffi2 name)
    )

  (defmacro fetch-ffi-constant
    ;; we have the correct value at compile time... but in order
    ;;  for the generated C to be portable, we need to emit a %%cexp here.
    (fetch-ffi-constant name val)
    -> (%%cexp int (%%stringify name))
    )

  )

(%backend bytecode

  (defmacro build-ffi-fun
    (build-ffi-fun iface name ztname rtype rcode nargs (argtype0 ...) (formal0 ...))
    -> (let (($pfun (vm-get-sym-handle (quote iface) ztname)))
         (lambda (formal0 ...)
           (%%cexp ((cref void) char int argtype0 ... -> rtype)
                   "ffi"
                   $pfun rcode nargs
                   formal0 ...))))


  (defmacro build-ffi-ob
    (build-ffi-ob iface name ztname obtype obcode)
    -> (%%cexp (string -> (cref obtype)) "dlsym0" ztname))

  (defmacro fetch-ffi-constant
    ;; we need to fetch the correct value at runtime.
    (fetch-ffi-constant name val)
    -> (lookup-constant (quote name)))

  )

(define sexp->ref
  (sexp:attr (sexp:symbol sname) fname)
  -> (let ((ref0 {off=0 ctype=(ctype:struct sname)}))
       (cref-field fname ref0))
  (sexp:attr sub fname)
  -> (let ((ref0 (sexp->ref sub)))
       (cref-field fname ref0))
  x -> (raise (:SexpRefError x))
  )

;; compute the size/offset of a type at runtime (VM).

(define sexp->sizeoff
  (sexp:list ((sexp:symbol 'struct) (sexp:symbol name)))
  -> (ctype->size (ctype:struct name))
  (sexp:list ((sexp:symbol 'union) (sexp:symbol name)))
  -> (ctype->size (ctype:union name))
  (sexp:symbol tdef)
  -> (ctype->size (ctype:name tdef))
  exp
  -> (let ((ref (sexp->ref exp)))
       ;;(printf "sexp->sizeoff sref " (repr exp) " " (int ref.off) "\n")
       ref.off)
  )

(%backend bytecode

  ;; ----------------- sizeoff table -----------------


  ;; XXX it would be really nice if we just had some generic mechanism for
  ;;   sharing metadata with the runtime.  This hack is ok, ONCE.  But not
  ;;   seven times.
  ;; this will be replaced by the VM with the contents of the sizeoff table.

  (define sizeoff-table (literal #((sexp:symbol &&sizeoff-sentinel&&))))

  (define (update-sizeoff-table)

    ;; (printf "sizeoff s-expressions: {\n")
    ;; (for-vector item sizeoff-table
    ;;   (printf "  " (repr item) "\n"))
    ;; (printf "}\n")

    (for-range i (vector-length sizeoff-table)
      (let ((val (sexp->sizeoff sizeoff-table[i])))
        (%%cexp (int int -> undefined) "sizeoff" (+ 5 i) val)))
    )

  )

;; --------------------------------------------------------------------------------
;;                                malloc
;; --------------------------------------------------------------------------------
;;
;; XXX it would be nice to have a managed malloc storage facility.
;;   I think this can be done with assistance from the GC, where we
;;     associate some kind of counter/generation-number with each 'foreign'
;;     pointer.  Then periodically we scan a map of all malloc memory to see
;;     if it has been copied, and if not, then automatically call free().
;;   This needs to handle the case where foreign pointers are truly foreign,
;;     i.e., do not try to free the pointer to external objects like `errno`.

;; this could also just use %c-cast.
(defmacro malloc
  (malloc type)       -> (%c-aref type (%malloc type 1) 0)
  (malloc type nelem) -> (%malloc type nelem)
  )

(defmacro halloc
  (halloc type)       -> (%c-aref type (%halloc type 1) 0)
  (halloc type nelem) -> (%halloc type nelem)
  )

(defmacro free
  (free ob) -> (%free #f ob))

(define (make-char-buffer size)
  (%c-aref char (malloc char size) 0))

;; ---------- utility -----------

(define (get-cstring s*)
  (let ((s0* (%c-aref char s* 0))
        (slen (libc/strlen s0*)))
    (%cref->string #f s0* slen)))

(define (cref->string s* slen)
  (%cref->string #f s* slen))

(define (c-get-int i*)
  (%c-get-int #f i*))

(define (c-set-int i* i)
  (%c-set-int #f i* i))

(define (c-get-ptr v*)
  (%c-get-ptr #f v*))

(define (c-set-ptr v** v*)
  (%c-set-ptr #f v** v*))

(define (c-aref a* idx)
  (%c-aref #f a* idx))

(define NULL (%int->cref #f 0))

(define (cref-null? x)
  (= 0 (%cref->int #f x)))

(define (cstring s)
  (%string->cref #f (zero-terminate s)))

;; errno is often a macro, and thus unreachable via FFI.
(define (get-errno)
  (%backend c (%%cexp (-> int) "errno"))
  (%backend llvm (%llvm-call ("@irk_get_errno" (-> int))))
  (%backend bytecode (%%cexp (-> int) "errno"))
  )

(define (raise-system-error)
  (let ((errno (get-errno))
        (msg (libc/strerror errno)))
    ;; (printf "system error: " (int errno)
    ;;         " " (string (%cref->string #f msg (libc/strlen msg)))
    ;;         "\n")
    (raise (:OSError errno))))

(define (syscall val)
  (if (< val 0)
      (raise-system-error)
      val))

(define (syscall-null val)
  (if (cref-null? val)
      (raise-system-error)
      val))
