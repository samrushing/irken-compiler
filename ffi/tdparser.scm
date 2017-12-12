;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/cmap.scm")
(include "lib/counter.scm")
(include "lib/parse/lexer.scm")
(include "lib/parse/earley.scm")
(include "ffi/clextab.scm")
(include "ffi/cgram.scm")

;; scan for typedefs, structs, and function declarations in
;;   preprocessed C output.

;; partition a file into a series of top-level objects.  this is done
;;   by reading declarations that are terminated by semicolons.  In
;;   order to do this 'safely', we must count the bracket/paren indent
;;   level and only consider a semicolon a delimiter when it occurs at
;;   the outermost level.

(define (partition-stream gen) : ((-> (maybe token)) -> (-> (maybe (list token))))
  (make-generator
   (lambda (consumer)
     (define (emit ob)
       (consumer (maybe:yes ob)))
     (let ((plevel 0)
           (blevel 0)
           (acc '()))
       (define (add tok)
         (PUSH acc tok))
       (for tok gen
         (match blevel plevel tok.kind with
           ;; these are various tokens we ignore
           _ _ 'WHITESPACE -> #u
           _ _ 'COMMENT    -> #u
           _ _ 'CPPLINE    -> #u
           _ _ 'SKIP       -> #u
           ;; a ';' at the outermost level means we need
           ;; to flush the current declaration.
           0 0 'SEMICOLON
           -> (begin
                (add tok)
                (add {kind='eof val="" range=tok.range})
                (emit (reverse acc))
                (set! acc '()))
           ;; maintain paren and brace level
           n _ 'LBRACE -> (begin (add tok) (set! blevel (+ blevel 1)))
           n _ 'RBRACE -> (begin (add tok) (set! blevel (- blevel 1)))
           _ n 'LPAREN -> (begin (add tok) (set! plevel (+ plevel 1)))
           _ n 'RPAREN -> (begin (add tok) (set! plevel (- plevel 1)))
           _ _ _ -> (add tok)
           ))
       (emit (reverse acc))
       (forever (consumer (maybe:no)))
       ))))

(define (toks->string toks)
  (format ";; " (join (lambda (x) x.val) " " toks) "\n"))

(define (is-tok? tok kind)
  (eq? tok.kind kind))

(define (impossiblex x)
  (printf "impossible:\n")
  (printf (parse-repr x) "\n")
  (raise (:Impossible)))

;; we can't use the one from lib/ctype.scm, it isn't
;;  expressive enough (yet).
(datatype ctype2
  (:name symbol)   ;; void, thing_t, etc...
  (:int cint bool) ;; kind/size, signed?
  (:float bool bool) ;; long? double?
  (:array int ctype2)
  (:pointer ctype2)
  (:struct symbol (maybe (list declarator)))
  (:union  symbol (maybe (list declarator)))
  (:function ctype2 (list declarator))
  )

(define ctype2-repr
  (ctype2:name name)    -> (format "(type " (symbol->string name) ")")
  (ctype2:int cint s?)  -> (cint-repr cint s?)
  (ctype2:float l? d?)  -> (format (if l? "long" "") (if d? "double" "float"))
  (ctype2:array size t) -> (format "(array " (int size) " " (ctype2-repr t) ")")
  (ctype2:pointer t)    -> (format "(* " (ctype2-repr t) ")")
  (ctype2:struct name (maybe:no))        -> (format "(struct " (sym name) ")")
  (ctype2:struct name (maybe:yes slots)) -> (format "(struct " (sym name) " (" (join declarator-repr " " slots) "))")
  (ctype2:union name (maybe:no))         -> (format "(union " (sym name) ")")
  (ctype2:union name (maybe:yes slots))  -> (format "(union " (sym name) " (" (join declarator-repr " " slots) "))")
  (ctype2:function type args)            -> (format "(fun " (join declarator-repr " " args) " -> " (ctype2-repr type) ")")
  )

;; a 'declarator' is a combination of name and type.  sometimes the
;; name is not present (e.g. parameters in function declarations)
(datatype declarator
  (:t ctype2 (maybe symbol))
  )

(define declarator-repr
  (declarator:t type (maybe:no))
  -> (format (ctype2-repr type))
  (declarator:t type (maybe:yes name))
  -> (format "(named " (sym name) " " (ctype2-repr type) ")")
  )

;; integer keywords can come in any order there are also nonsense
;; combinations like 'long long char', 'unsigned signed int', 'long
;; short int'.
;; (UNSIGNED LONG LONG INT) => (ctype:int (cint:longlong) #f)
;;                   (CHAR) => (ctype:int (cint:char) #t)
(define (grok-int syms)
  (define (num sym)
    (count (lambda (x) (eq? x sym)) 0 syms))
  (ctype2:int
   (match (num 'CHAR) (num 'SHORT) (num 'LONG) with
     0 0 0 -> (cint:int)
     0 0 1 -> (cint:long)
     0 0 2 -> (cint:longlong)
     0 1 0 -> (cint:short)
     1 0 0 -> (cint:char)
     _ _ _ -> (raise (:TDParser/BadInteger syms)))
   (match (num 'UNSIGNED) (num 'SIGNED) with
     0 0 -> #t
     0 1 -> #t
     1 0 -> #f
     _ _ -> (raise (:TDParser/BadInteger syms))
     )))

;; float_type: FLOAT | DOUBLE | LONG FLOAT | LONG DOUBLE;
(define parse-float-type
  (parse:nt 'float_type ((parse:t a)))
  -> (if (eq? a.kind 'DOUBLE)
         (ctype2:float #f #t)
         (ctype2:float #f #f))
  (parse:nt 'float_type (long (parse:t a)))
  -> (if (eq? a.kind 'DOUBLE)
         (ctype2:float #t #t)
         (ctype2:float #t #f))
  x -> (impossiblex x)
  )

(define parse-int-type-base
  (parse:nt 'int_type_base ((parse:t tok)))
  -> tok.kind
  x -> (impossiblex x)
  )

(define parse-int-type
  (parse:nt 'int_type (int_type int_type_base))
  -> (list:cons (parse-int-type-base int_type_base) (parse-int-type int_type))
  (parse:nt 'int_type (int_type_base))
  -> (list:cons (parse-int-type-base int_type_base) (list:nil))
  x -> (impossiblex x)
  )

(define is-struct?
  (parse:nt 'struct_or_union ((parse:t tok)))
  -> (eq? tok.kind 'STRUCT)
  x -> (impossiblex x)
  )

;; struct_elem: type_declarator SEMICOLON;
(define parse-struct-elem
  (parse:nt 'struct_elem (type-declarator SEMICOLON))
  -> (parse-type-declarator type-declarator)
  x -> (impossiblex x)
  )

;; struct_elems: struct_elems struct_elem | struct_elem;
(define parse-struct-elems
  (parse:nt 'struct_elems (elems elem))
  -> (list:cons (parse-struct-elem elem) (parse-struct-elems elems))
  (parse:nt 'struct_elems (elem))
  -> (list:cons (parse-struct-elem elem) (list:nil))
  x -> (impossiblex x)
  )

(define parse-struct-or-union-decl*
  ((parse:t IDENT))
  -> (:tuple (string->symbol IDENT.val) (maybe:no))
  (LB elems RB)
  -> (:tuple '%unnamed
             (maybe:yes (parse-struct-elems elems)))
  ((parse:t IDENT) LB elems RB)
  -> (:tuple (string->symbol IDENT.val)
             (maybe:yes (parse-struct-elems elems)))
  x -> (impossiblex (parse:nt 'struct_or_union_decl x))
  )

(define parse-struct-or-union-decl
  (parse:nt 'struct_or_union_decl (su . rest))
  -> (let (((name slots) (parse-struct-or-union-decl* rest)))
       (if (is-struct? su)
           (ctype2:struct name slots)
           (ctype2:union name slots)))
  x -> (impossiblex x)
  )

(define parse-type
  (parse:nt 'type ((parse:t token)))
  -> (ctype2:name (string->symbol token.val))
  (parse:nt 'type (type SPLAT))
  -> (ctype2:pointer (parse-type type))
  (parse:nt 'type (other))
  -> (match other with
       (parse:nt 'int_type _)
       -> (grok-int (parse-int-type other))
       (parse:nt 'float_type _)
       -> (parse-float-type other)
       (parse:nt 'struct_or_union_decl _)
       -> (parse-struct-or-union-decl other)
       x -> (impossiblex x)
       )
  (parse:nt 'type (rtype LP SPLAT RP sig))
  -> (ctype2:pointer
      (ctype2:function
       (parse-type rtype)
       (parse-signature sig)))
  x -> (impossiblex x)
  )

(define parse-arg
  (parse:nt 'arg ((parse:t dotdotdot)))
  -> (declarator:t (ctype2:name '...) (maybe:no))
  (parse:nt 'arg (sub))
  -> (match sub with
       (parse:nt 'type _)
       -> (declarator:t (parse-type sub) (maybe:no))
       (parse:nt 'type_declarator _)
       -> (parse-type-declarator sub)
       x -> (impossiblex x)
       )
  x -> (impossiblex x)
  )

(define parse-arglist
  (parse:nt 'arglist (arglist COMMA arg))
  -> (list:cons (parse-arg arg) (parse-arglist arglist))
  (parse:nt 'arglist (arg))
  -> (list:cons (parse-arg arg) (list:nil))
  x -> (impossiblex x)
  )

(define parse-signature
  (parse:nt 'signature (LP RP))
  -> (list:nil)
  (parse:nt 'signature (LP arglist RP))
  -> (reverse (parse-arglist arglist))
  x -> (impossiblex x)
  )

(define parse-fundecl
  (parse:nt 'fun_declaration (rtype (parse:t ident) signature SEMICOLON))
  -> (declarator:t
      (ctype2:function
       (parse-type rtype)
       (parse-signature signature))
      (maybe:yes (string->symbol ident.val)))
  (parse:nt 'fun_declaration
            (type IDENT signature (parse:nt 'attribute_specifiers _) SEMICOLON)) ;; ignore attributes
  -> (parse-fundecl (parse:nt 'fun_declaration (LIST type IDENT signature SEMICOLON)))
  (parse:nt 'fun_declaration
            ((parse:nt 'attribute_specifiers _) type IDENT signature SEMICOLON)) ;; ignore attributes
  -> (parse-fundecl (parse:nt 'fun_declaration (LIST type IDENT signature SEMICOLON)))
  x -> (impossiblex x)
  )

;; we don't need the exact size of any array declaration (yet), so for
;; now we'll just ignore it.

(define parse-declarator
  type (parse:nt 'declarator ((parse:t IDENT))) ;; IDENT
  -> (:tuple type (string->symbol IDENT.val))
  type (parse:nt 'declarator (sub LB RB)) ;; declarator [ ]
  -> (let (((type ident) (parse-declarator type sub)))
       (:tuple (ctype2:array 0 type) ident))
  type (parse:nt 'declarator (sub LB exp RB)) ;; declarator [ expression ]
  -> (let (((type ident) (parse-declarator type sub)))
       (:tuple (ctype2:array 0 type) ident))
  _ x -> (impossiblex x)
  )

;; ignore attributes - except for this bizarro linux case:
;; typedef int int8_t __attribute__ (( __mode__ ( __QI__ )));
;; two choices:
;; 1) parse attributes & special-case "__mode__ (__QI__)".
;; 2) ignore any attempt to redefine stdint types.
;; I like #2, but what about cases where attribute-mode is
;;   is used on other typedefs?

(define parse-declarator-specifier
  type (parse:nt 'declaratorSpecifier (decl))
  -> (parse-declarator type decl)
  type (parse:nt 'declaratorSpecifier (decl attributes))
  -> (parse-declarator type decl)
  _ x -> (impossiblex x)
  )

(define parse-type-declarator
  ;; normal type declarator
  (parse:nt 'type_declarator (type declarator-specifier))
  -> (let ((type0 (parse-type type))
           ((type1 name) (parse-declarator-specifier type0 declarator-specifier)))
       (declarator:t type1 (maybe:yes name)))
  ;; function pointer
  (parse:nt 'type_declarator (type LP SPLAT (parse:t IDENT) RP signature))
  -> (let ((args (parse-signature signature))
           (rtype (parse-type type))
           (name (string->symbol IDENT.val))
           (ftype (ctype2:function rtype args)))
       (declarator:t (ctype2:pointer ftype) (maybe:yes name)))
  x -> (impossiblex x)
  )

(define parse-typedef
  (parse:nt 'typedef (TYPEDEF type-declarator SEMICOLON))
  -> (parse-type-declarator type-declarator)
  x -> (impossiblex x)
  )

;; things at the top level that start with 'struct':
;; struct thing1;
;; struct thing1 { int x; int y; };
;; struct thing2 { int x; int y; } my_thing;
;; struct thing2 my_other_thing2;
;; struct thing3 somefun (int, ...);
;; struct thing3 somefun (int, ...) { ... }
;; we only care about #2 - the definition of a struct.
;; we will assume (for now) that functions returning structs
;;   are not an important part of APIs that will be accessed by Irken.
(define parse-struct-definition
  (parse:nt 'struct_definition (sub SEMICOLON))
  -> (match sub with
       (parse:nt 'struct_or_union_decl _)
       -> (parse-struct-or-union-decl sub)
       x -> (impossiblex x)
       )
  x -> (impossiblex x)
  )

;; given a name -> type map for all typedefs,
;;  walk a ctype replacing all typedefs.
(define (substitute-typedefs type map)

  ;; expand-structs? is a hack to stop typedef structs from expanding
  ;; in-place when they are arguments to functions. [maybe this is an argument
  ;; for keeping structure definitions separate from types]

  (define (walk-type t expand-structs?)
    (match expand-structs? t with
      es? (ctype2:name name)
      -> (match (tree/member map symbol-index-cmp name) with
           (maybe:yes next) -> (walk-type next es?)
           (maybe:no) -> t)
      es? (ctype2:array n next)                 -> (ctype2:array n (walk-type next es?))
      es? (ctype2:pointer next)                 -> (ctype2:pointer (walk-type next es?))
      #f (ctype2:struct name _)                 -> (ctype2:struct name (maybe:no))
      #t (ctype2:struct name (maybe:no))        -> (ctype2:struct name (maybe:no))
      #t (ctype2:struct name (maybe:yes slots)) -> (ctype2:struct name (maybe:yes (walk-decls #t slots)))
      #f (ctype2:union name _)                  -> (ctype2:union name (maybe:no))
      #t (ctype2:union name (maybe:no))         -> (ctype2:union name (maybe:no))
      #t (ctype2:union name (maybe:yes slots))  -> (ctype2:union name (maybe:yes (walk-decls #t slots)))
      #t (ctype2:function rtype args)           -> (ctype2:function (walk-type rtype #f) (walk-decls #f args))
      _ _ -> t
      ))

  (define walk-decls
    es? ((declarator:t type name) . rest)
    -> (list:cons (declarator:t (walk-type type es?) name) (walk-decls es? rest))
    es? ()
    -> (list:nil)
    )

  (walk-type type #t)
  )

(define (process-cpp-file tdgram gen)

  (let ((typedefs (tree/empty))
        (structs (tree/empty))
        (functions (tree/empty)))

    (define (parse-toks toks root)
      (earley tdgram (prod:nt root) (list-generator toks)))

    (define (try-parse toks root parser)
      (try
       (maybe:yes (parser (parse-toks toks root)))
       except (:NoParse tok)
       -> (begin
            (printf (bold "unable to parse " (sym root) " expression:\n"))
            (printf " toks = " (toks->string toks) "\n")
            (printf " at token = " (token-repr tok) "\n")
            (maybe:no))))

    (define add-typedef!
      (declarator:t type (maybe:yes name))
      -> (begin
           ;; if it's a struct, add it to that table as well.
           (match type with
             (ctype2:struct name slots)
             -> (tree/insert! structs symbol-index-cmp name slots)
             _ -> #u)
           (tree/insert! typedefs symbol-index-cmp name type))
      _ -> (impossible)
      )

    (define add-struct!
      (ctype2:struct name slots)
      -> (tree/insert! structs symbol-index-cmp name slots)
      _ -> (impossible)
      )

    (define add-function!
      (declarator:t type (maybe:yes name))
      -> (tree/insert! functions symbol-index-cmp name type)
      _ -> (impossible)
      )

    (for toks gen
      (printf (toks->string toks))
      (match (%%attr (car toks) kind) with
        'eof
        -> #u
        'TYPEDEF
        -> (match (try-parse toks 'typedef parse-typedef) with
             (maybe:no) -> #u
             (maybe:yes ob)
             -> (begin
                  (add-typedef! ob)
                  (printf "typedef: " (declarator-repr ob) "\n")))
        'STRUCT
        -> (match (try-parse toks 'struct_definition parse-struct-definition) with
             (maybe:no) -> #u
             (maybe:yes ob)
             -> (begin
                  (add-struct! ob)
                  (printf "struct: " (ctype2-repr ob) "\n")))
        _
        -> (match (try-parse toks 'fun_declaration parse-fundecl) with
             (maybe:no) -> #u
             (maybe:yes ob)
             -> (begin
                  (add-function! ob)
                  (printf "decl: " (declarator-repr ob) "\n"))
             )
        ))
    (printf "\n--------- typedefs -----------\n")
    (for-map k v typedefs
      ;;(printf (lpad 30 (sym k)) " " (ctype2-repr v) "\n")
      (printf (lpad 30 (sym k)) " "
              (ctype2-repr (substitute-typedefs v typedefs))
              "\n")
      )
    (printf "\n--------- structs -----------\n")
    (for-map k v structs
      (printf "struct " (bold (sym k)) " {\n")
      (match v with
        (maybe:yes slots)
        -> (for-list slot slots
             (match slot with
               (declarator:t type (maybe:yes name))
               ;;-> (printf (lpad 12 (sym name)) " : " (ctype2-repr type) "\n")
               -> (printf (lpad 12 (sym name))
                          " : "
                          (ctype2-repr
                           (substitute-typedefs type typedefs))
                          "\n")
               _ -> (impossible)
               ))
        _ -> #u
        )
      (printf "}\n")
      )
    (printf "\n--------- functions -----------\n")
    (for-map k v functions
      ;;(printf (lpad 30 (sym k)) " " (ctype2-repr v) "\n")
      (printf (lpad 30 (sym k)) " " (ctype2-repr (substitute-typedefs v typedefs)) "\n")
      )
    ))

(if (< sys.argc 2)
    (begin
      (printf "\nScan a .cpp (pre-processed C) file for typedefs.\n\n")
      (printf "Usage: " sys.argv[0] " file.c\n")
      (printf "example: $ clang -E /usr/include/zlib.h > /tmp/zlib.cpp\n")
      (printf "         $ " sys.argv[0] " /tmp/zlib.cpp\n"))
    (let ((tdgram (sexp->grammar c-grammar))
          (file (file/open-read sys.argv[1]))
          (gen0 (make-lex-generator dfa-c file))
          (gen1 (partition-stream gen0)))
      (process-cpp-file tdgram gen1)
      ))
