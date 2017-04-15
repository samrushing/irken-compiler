;; -*- Mode: Irken -*-

(datatype ctype
  (:name symbol)  ;; void, char, thing_t, etc...
  (:int int bool) ;; size (in bytes), signed?
  (:array int ctype)
  (:pointer ctype)
  (:struct symbol)
  (:union symbol)
  )

(define ctype-repr
  (ctype:name name)    -> (symbol->string name)
  (ctype:int size s?)  -> (format (if s? "i" "u") (int (* size 8)))
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

;; c function signature
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
  (sexp:symbol name)
  -> (ctype:name name)
  (sexp:list ((sexp:symbol 'int) (sexp:list ((sexp:int size) (sexp:int signed?)))))
  -> (ctype:int size (if (= signed? 1) #t #f))
  (sexp:list ((sexp:symbol '*) sub))
  -> (ctype:pointer (parse-ctype sub))
  (sexp:list ((sexp:symbol 'struct) (sexp:symbol name)))
  -> (ctype:struct name)
  (sexp:list ((sexp:symbol 'union) (sexp:symbol name)))
  -> (ctype:union name)
  (sexp:list ((sexp:symbol 'array) sub (sexp:list ((sexp:int size)))))
  -> (ctype:array size (parse-ctype sub))
  x -> (error1 "malformed type" (repr x))
  )

(define (parse-spec forms)

  (define parse-field
    (sexp:list ((sexp:int size) (sexp:symbol name) ftype))
    -> (cfield:t size name (parse-ctype ftype))
    x -> (error1 "malformed field" (repr x))
    )

  (define parse-struct
    'struct ((sexp:symbol name) (sexp:int size) . fields)
    -> (ffi-info.structs::add name (cdef:struct size name (map parse-field fields)))
    'union ((sexp:symbol name) (sexp:int size) . fields)
    -> (ffi-info.unions::add name (cdef:union size name (map parse-field fields)))
    kind x -> (error1 "malformed struct" (:pair kind x))
    )

  (define arrow-type?
    ()                    -> #f
    ((sexp:symbol '->) _) -> #t
    (hd . tl)             -> (arrow-type? tl)
    )

  (define parse-sig*
    name (sexp:list sig)
    -> (if (arrow-type? sig)
           (let ((siglen (length sig))
                 (rtype (nth sig (- siglen 1)))
                 (args (slice sig 0 (- siglen 2))))
             (csig:fun name (parse-ctype rtype) (map parse-ctype args)))
           (csig:obj name (parse-ctype (sexp:list sig))))
    name sig
    -> (csig:obj name (parse-ctype sig))
    name _ -> (error1 "malformed sig" name)
    )

  (define parse-sig
    ((sexp:symbol name) sig)
    -> (ffi-info.sigs::add name (parse-sig* name sig))
    x -> (error1 "malformed sig" (repr (sexp:list x)))
    )

  (define parse-con
    ((sexp:symbol name) (sexp:int val))
    -> (ffi-info.cons::add name val)
    x -> (error1 "malformed constant" x)
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
    x -> (error1 "malformed spec file" x)
    )

  (for-list form forms
    (parse-form form))

  )

;; XXX do we need to distinguish between compile-time and run-time use
;;     of this registry?
(define ffi-info
  ;; we have to use two maps for struct/union because
  ;; they sometimes have the same name (e.g. in6_addr)
  ;; [alternatively we could use a pair as key?]
  {structs = (map-maker symbol-index-cmp)
   unions  = (map-maker symbol-index-cmp)
   cons    = (map-maker symbol-index-cmp)
   sigs    = (map-maker symbol-index-cmp)
   })

(define (dump-ffi-info)
  (printf "sigs:\n")
  (for-map k v ffi-info.sigs.self.t
    (csig-print v))
  (printf "defs:\n")
  (for-map k v ffi-info.structs.self.t
    (cdef-print v))
  (for-map k v ffi-info.unions.self.t
    (cdef-print v))
  (printf "constants:\n")
  (for-map k v ffi-info.cons.self.t
    (printf (lpad 5 (int v)) " " (sym k) "\n"))
  )

(define (read-spec name)
  (let ((path0 (format "ffi/" (sym name) "_ffi.scm")))
    (%backend bytecode
      (read-string
       (string-concat
        (%%cexp (string -> (list string)) "readf" path0))))
    (%backend (c llvm)
      (let ((file (file/open-read path0)))
        (reader path0 (lambda () (file/read-char file)))))
    ))

;; this is meant for *runtime* use.
(define require-ffi
  (let ((loaded '()))
    (lambda (name)
      (when (not (member-eq? name loaded))
        (let ((forms (read-spec name)))
          (parse-spec forms)
          (PUSH loaded name))))
    ))

(define *word-size* (get-word-size))

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
    _ -> (error1 "lookup-struct-size: unknown struct" name)
    ))

(define (lookup-struct-fields name)
  (match (ffi-info.structs::get name) with
    (maybe:yes (cdef:struct _ _ fields)) -> fields
    _ -> (error1 "lookup-struct-size: unknown struct" name)
    ))

(define (lookup-union-fields name)
  (match (ffi-info.unions::get name) with
    (maybe:yes (cdef:union _ _ fields)) -> fields
    _ -> (error1 "lookup-struct-size: unknown struct" name)
    ))

(define lookup-field
  name ()
  -> (error1 "lookup-field failed" name)
  name ((cfield:t offset name0 ctype) . tl)
  -> (if (eq? name name0)
         {off=offset ctype=ctype}
         (lookup-field name tl))
  )

(define ctype->size
  (ctype:pointer _)    -> *word-size*
  (ctype:int size _)   -> size
  (ctype:array size t) -> (* size (ctype->size t))
  (ctype:name name)    -> (base-type-size name)
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

;; ughhh.. really need to teach macros to emit record literals.
(define (makeref off ctype) {off=off ctype=ctype})

(defmacro cref
  (cref ctype a ...)
  -> (expand-cref (makeref 0 ctype) (%%sexp a ...)))

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

;; convert a ctype to an `op_cget` code (used by the VM).
(define ctype->code
  (ctype:int 1 #f)                  -> #\1
  (ctype:int 2 #f)                  -> #\2
  (ctype:int 4 #f)                  -> #\4
  (ctype:int 8 #f)                  -> #\8
  (ctype:int 1 #t)                  -> #\i
  (ctype:int 2 #t)                  -> #\j
  (ctype:int 4 #t)                  -> #\k
  (ctype:int 8 #t)                  -> #\l
  (ctype:array _ (ctype:name char)) -> #\s
  (ctype:array _ _)                 -> #\p
  (ctype:pointer _)                 -> #\p
  (ctype:struct _)                  -> #\p
  (ctype:union _)                   -> #\p
  (ctype:name 'char)                -> #\c
  (ctype:name 'void)                -> (raise (:VoidDereference))
  x                                 -> (raise (:StrangeCtype x))
  )
