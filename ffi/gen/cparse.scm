;; -*- Mode: Irken -*-

;; This file parses a limited subset of C.  It expects as input
;; pre-processed C (i.e. "cc -E file.c") and scans for typedefs,
;; structs, unions, and function declarations.

(include "lib/parse/lexer.scm")
(include "lib/parse/earley.scm")
(include "ffi/gen/clextab.scm")
(include "ffi/gen/cgram.scm")

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

;; expression parser.
;; the only place in our grammar where we need to parse expressions
;; is in the size of arrays.  Most arrays have simple integer sizes,
;; but some are complex expressions involving arithmetic and 'sizeof'.
;; for now, we will only support simple integer sizes.  later, we may
;; try to implement sizeof and 'do the math'.
(define walk-unary-chain
  (parse:t {kind='NUMBER val=val ...}) -> (string->int val)
  (parse:nt someExpression (sub))      -> (walk-unary-chain sub)
  x -> (raise (:TDParser/Complex x))
  )

(define (get-simple-expression-value exp)
  (try
   (walk-unary-chain exp)
   except (:TDParser/Complex _)
   -> 0
   ))

;; integer keywords can come in any order - there are also nonsense
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
  -> (:tuple '%anonymous (maybe:yes (reverse (parse-struct-elems elems))))
  ((parse:t IDENT) LB elems RB)
  -> (:tuple (string->symbol IDENT.val) (maybe:yes (reverse (parse-struct-elems elems))))
  x -> (impossiblex (parse:nt 'struct_or_union_decl x))
  )

(define parse-struct-or-union-decl
  (parse:nt 'struct_or_union_decl (su . rest))
  -> (let (((name slots) (parse-struct-or-union-decl* rest)))
       (declarator:t
        (if (is-struct? su)
            (ctype2:struct name slots)
            (ctype2:union name slots))
        (maybe:yes name)))
  x -> (impossiblex x)
  )

;; not needed [yet]
;; (define parse-enum-specifier
;;   (parse:nt 'enum_specifier (ENUM LB enum-list RB))
;;   -> (ctype2:int (cint:int))
;;   (parse:nt 'enum_specifier (ENUM IDENT LB enum-list RB))
;;   -> (ctype2:int (cint:int))
;;   (parse:nt 'enum_specifier (ENUM IDENT))
;;   -> (ctype2:int (cint:int))
;;   x -> (impossiblex x)
;;   )

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
       -> (match (parse-struct-or-union-decl other) with
            ;; assume the name is '%anonymous?
            (declarator:t type name) -> type)
       ;; we treat all enum as int.
       (parse:nt 'enum_specifier _)
       -> (ctype2:int (cint:int) #t)
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

(define parse-funident
  (parse:nt 'fun_ident ((parse:t ident)))
  -> (string->symbol ident.val)
  (parse:nt 'fun_ident (LP (parse:t ident) RP))
  -> (string->symbol ident.val)
  x -> (impossiblex x)
  )

(define parse-fundecl
  (parse:nt 'fun_declaration (rtype ident signature SEMICOLON))
  -> (declarator:t
      (ctype2:function
       (parse-type rtype)
       (parse-signature signature))
      (maybe:yes (parse-funident ident)))
  ;; the following rules just strip any attribute specifiers off.
  (parse:nt 'fun_declaration
            (type ident signature
                  (parse:nt 'attribute_specifiers _)
                  SEMICOLON))
  -> (parse-fundecl (parse:nt 'fun_declaration (LIST type ident signature SEMICOLON)))
  (parse:nt 'fun_declaration
            ((parse:nt 'attribute_specifiers _)
             type ident signature
             SEMICOLON))
  -> (parse-fundecl (parse:nt 'fun_declaration (LIST type ident signature SEMICOLON)))
  (parse:nt 'fun_declaration
            ((parse:nt 'attribute_specifiers _)
             type ident signature
             (parse:nt 'attribute_specifiers _)
             SEMICOLON))
  -> (parse-fundecl (parse:nt 'fun_declaration (LIST type ident signature SEMICOLON)))
  x -> (impossiblex x)
  )

(define parse-declaration
  (parse:nt 'declaration (fun))
  -> (parse-fundecl fun)
  (parse:nt 'declaration (struct/union SEMICOLON))
  -> (parse-struct-or-union-decl struct/union)
  (parse:nt 'declaration (type ident SEMICOLON))
  -> (declarator:t (parse-type type) (maybe:yes (parse-funident ident)))
  x -> (impossiblex x)
  )

(define parse-declarator
  type (parse:nt 'declarator ((parse:t IDENT))) ;; IDENT
  -> (:tuple type (string->symbol IDENT.val))
  type (parse:nt 'declarator (sub LB RB))       ;; declarator [ ]
  -> (let (((type ident) (parse-declarator type sub)))
       (:tuple (ctype2:array 0 type) ident))
  type (parse:nt 'declarator (sub LB exp RB))   ;; declarator [ expression ]
  -> (let (((type ident) (parse-declarator type sub)))
       (:tuple (ctype2:array (get-simple-expression-value exp) type) ident))
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
  ;; attribute_specifier type declarator
  (parse:nt 'type_declarator (attribute_specifier type declarator-specifier))
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
;; struct thing1 x;
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

