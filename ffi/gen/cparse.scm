;; -*- Mode: Irken -*-

;; This file parses a limited subset of C.  It expects as input
;; pre-processed C (i.e. "cc -E file.c") and scans for typedef,
;; struct, union, enum, and function declarations.

(require "lib/parse/lexer.scm")
(require "lib/parse/earley.scm")
(require "ffi/gen/clextab.scm")
(require "ffi/gen/cgram.scm")

;; partition a file into a series of top-level objects.  this is done
;;   by reading declarations that are terminated by semicolons.  In
;;   order to do this 'safely', we must count the bracket/paren indent
;;   level and only consider a semicolon a delimiter when it occurs at
;;   the outermost level.
;;
;; Obscure Note: the Ancient C tongue supported declaring function params
;;   like this:
;; int
;; thing (x, y)
;;   int x;
;;   int y;
;; {
;;   return x + y;
;; }
;;
;; and any such code will break partition-stream.  But it has been decades
;;   since I've seen this.
;;
;; Note: this procedure fails whenever a function definition is present.  Because of
;;   this (very difficult) problem, the C parser now has to parse the entire C grammar
;;   in order to 'find' the declaration after the function.
;;  However, partitioning the stream definitely makes the parser work more efficiently,
;;   so it's still worth doing.

(define (partition-stream gen) : ((-> (maybe token)) -> (-> (maybe (list token))))
  (makegen emit
    (let ((plevel 0)
          (blevel 0)
          (last 'bogus)
          (acc '()))
      (define (add tok)
        (push! acc tok))
      (for tok gen
        (match blevel plevel tok.kind with
          ;; a ';' at the outermost level means we need
          ;; to flush the current declaration.
          0 0 'Semi
          -> (begin
               (add tok)
               (add {kind='EOF val="" range=tok.range})
               (emit (reverse acc))
               (set! acc '()))
          ;; maintain paren and brace level
          n _ 'LeftBrace  -> (begin (add tok) (set! blevel (+ blevel 1)))
          n _ 'RightBrace -> (begin (add tok) (set! blevel (- blevel 1)))
          _ n 'LeftParen  -> (begin (add tok) (set! plevel (+ plevel 1)))
          _ n 'RightParen -> (begin (add tok) (set! plevel (- plevel 1)))
          _ _ _ -> (add tok)
          )
        (set! last tok.kind)
        )
      (match acc with
        ({kind='EOF ...}) -> #u ;; dont' emit (EOF)
        _ -> (emit (reverse acc))
        ))))

(define (strip-whitespace gen)
  (makegen emit
    (for tok gen
      (match tok.kind with
        'Newline         -> #u
        'Whitespace      -> #u
        'LineComment     -> #u
        'BlockComment    -> #u
        'LineDirective   -> #u
        'Nullability     -> #u
        'PragmaDirective -> #u
        'Extension       -> #u
        _                -> (emit tok)
        ))))

(define (strip-attributes gen)
  (makegen emit
    (let ((in-attr? #f)
          (plevel 0))
      (for tok gen
        (match in-attr? plevel tok.kind with
          #f 0 'Attribute  -> (set! in-attr? #t)
          #f _ _           -> (emit tok)
          #t _ 'LeftParen  -> (inc! plevel)
          #t 1 'RightParen -> (begin (dec! plevel) (set! in-attr? #f))
          #t _ 'RightParen -> (dec! plevel)
          #t _ _           -> #u
          )))))

(define (strip-asm gen)
  (makegen emit
    (let ((in-asm? #f)
          (plevel 0))
      (for tok gen
        (match in-asm? plevel tok.kind with
          #f 0 'Asm        -> (set! in-asm? #t)
          #f _ _           -> (emit tok)
          #t _ 'LeftParen  -> (inc! plevel)
          #t 1 'RightParen -> (begin
                                (dec! plevel)
                                (set! in-asm? #f)
                                (emit {kind='AsmStatement val="__asm__()" range=tok.range})
                                )
          #t _ 'RightParen -> (dec! plevel)
          #t _ _           -> #u
          )))))

(define (frob-typedef-names gen name-map)
  (makegen emit
    (for tok gen
      (match tok.kind with
        'Identifier
        -> (emit
            (if (cmap/present? name-map tok.val)
                {kind='TypedefName val=tok.val range=tok.range}
                tok))
        _
        -> (emit tok)
        ))))

(define (toks->string toks)
  (format ";; " (join (lambda (x) x.val) " " toks) "\n"))

(define (is-tok? tok kind)
  (eq? tok.kind kind))

(define (impossiblex x)
  (printf "impossible:\n")
  (printf (parse-repr x) "\n")
  (raise (:Impossible)))

;; integer keywords can come in any order - there are also nonsense
;; combinations like 'long long char', 'unsigned signed int', 'long
;; short int'.
;; (UNSIGNED LONG LONG INT) => (ctype:int (cint:longlong) #f)
;;                   (CHAR) => (ctype:int (cint:char) #t)
(define (grok-int-type syms)
  (define (num sym)
    (count (lambda (x) (eq? x sym)) 0 syms))
  (ctype2:int
   (match (num 'char) (num 'short) (num 'long) with
     0 0 0 -> (cint:int)
     0 0 1 -> (cint:long)
     0 0 2 -> (cint:longlong)
     0 1 0 -> (cint:short)
     1 0 0 -> (cint:char)
     _ _ _ -> (raise (:TDParser/BadInteger syms)))
   (match (num 'unsigned) (num 'signed) with
     0 0 -> #t
     0 1 -> #t
     1 0 -> #f
     _ _ -> (raise (:TDParser/BadInteger syms))
     )))

(define (grok-num-type syms)
  (match syms with
    ('float)        -> (ctype2:float #f #f)
    ('double)       -> (ctype2:float #f #t)
    ('long 'double) -> (ctype2:float #t #t)
    ;; note: 'long float' is not legal.
    otherwise       -> (grok-int-type syms)
    ))

;; --- new (complete) parser ---

(define (parse-translation-unit exp typedef-name-map)
  ;; assumes list is of this shape:
  ;; (list
  ;;   item
  ;;   (list item))
  ;; note: does not check the nt name.
  (define (collect-unseparated-list exp)
    (define loop
      acc (parse:nt _ (sub))
      -> (list:cons sub acc)
      acc (parse:nt _ (rest sub))
      -> (loop (list:cons sub acc) rest)
      acc x
      -> (impossiblex x)
      )
    (loop '() exp))

  (define strip-unary
    (parse:nt outer (inner))
    -> inner
    x -> (impossiblex x)
    )

  (define (is-nt? nt exp)
    (match exp with
      (parse:nt nt0 _) -> (eq? nt0 nt)
      _                -> #f
      ))

  (define p-translationUnit
    acc (parse:nt 'translationUnit (eD))
    -> (p-externalDeclaration acc eD)
    acc (parse:nt 'translationUnit (tU eD))
    -> (p-translationUnit (p-externalDeclaration acc eD) tU)
    acc x -> (impossiblex x)
    )

  (define p-externalDeclaration
    acc (parse:nt 'externalDeclaration ((parse:nt 'declaration subs)))
    -> (list:cons (p-declaration (parse:nt 'declaration subs)) acc)
    acc (parse:nt 'externalDeclaration ((parse:nt 'typeRedefinition subs)))
    -> acc ;; ignore type redefinition
    acc _
    -> acc ;; we ignore function definitions and bare semicolons.
    )

  (define (p-declaration exp)
    ;; parse a single declaration.  the grammar accumulates various pieces that it
    ;;   expects you to put together.  we'll do that here in this closure.
    ;;
    ;; one 'hack' in the grammar: a typedef looks like the declaration of a value;
    ;;   i.e.:
    ;;
    ;; typedef int thing_t;
    ;;
    ;; is seen as a value named 'thing_t' of type int, and only by noticing the word
    ;;   'typedef' in front do you know that it's a completely different beast. [and
    ;;   notice that 'typedef' is marked as a #@^*ing 'storage class']

    ;; declarationSpecifiers is a completely broken rule.  I think the motivation here
    ;;  is to allow modifiers to be sprinkled literally anywhere inside the declaration
    ;;  [like __attribute__], and that made the grammar difficult to write.  The upshot
    ;;  is that you actually need a separate 'parser' just to make sense of the resulting
    ;;  mess.

    ;; parse -> (:tuple bool (list ctype2))
    (define p-declarationSpecifiers
      ;; note: we wildcard 'declarationSpecifiers[2]
      (parse:nt _ (dS-0))
      -> (let ((flat (map strip-unary (collect-unseparated-list dS-0)))
               (typedef? #f))
           ;; of these, the only ones we care about are typeSpecifier and storageClassSpecifier
           ;; (and then only to catch 'typedef').
           ;; (declarationSpecifier
           ;;  storageClassSpecifier         typedef/extern/static/threadlocal/auto/register
           ;;  typeSpecifier                 ...
           ;;  typeQualifier                 const/restrict/volatile/atomic
           ;;  functionSpecifier             stdcall/inline/etc
           ;;  alignmentSpecifier)           alignas...
           ;;
           ;; strip off typedef, which must be in front.
           (match flat with
             ((parse:nt 'storageClassSpecifier ((parse:t {kind='Typedef val=_}))) . rest)
             -> (begin (set! flat rest) (set! typedef? #t))
             _ -> #u)
           ;; filter out anything else that's not a typeSpecifier, and strip them.
           (set! flat (filter (lambda (x) (is-nt? 'typeSpecifier x)) flat))
           (:tuple typedef? (collapse-type-list (map p-typeSpecifier flat)))
           )
      x -> (impossiblex x)
      )

    ;; collapse a list of types (parsed from typeSpecifier+)
    (define (collapse-type-list types)
      (define (collapse types)
        (if (> (length types) 1)
            (grok-num-type (map type->name types))
            (first types)))
      ;; (printf "collapse-type-list " (join ctype2-repr ", " types) "\n")
      (match (length types) (last types) with
        1 type -> type
        n (ctype2:pointer sub)
        -> (ctype2:pointer (collapse (append (butlast types) (list sub))))
        n type -> (collapse types)
        ))

    ;; we need to turn these types *back* into symbols so grok-int can
    ;;   deal with them.
    (define type->name
      (ctype2:int (cint:int) #f)   -> 'unsigned
      (ctype2:int (cint:long) #t)  -> 'long
      (ctype2:int (cint:int) #t)   -> 'int
      (ctype2:int (cint:short) #t) -> 'short
      (ctype2:int (cint:char) #t)  -> 'char
      (ctype2:float _ #f)          -> 'float
      (ctype2:float _ #t)          -> 'double
      x -> (impossible)
      )
    ;; a declaration is essentially:
    ;; type name0=val0, name1=val1, ...;
    ;; where <type> is declarationSpecifiers.
    ;; note: each type can be different because of postfix modifiers:
    ;; int x[10], y;

    (define p-decl
      (parse:nt 'declaration (declSpecs Semi))
      -> (let (((_ base-type) (p-declarationSpecifiers declSpecs)))
           (:tuple #f (list (:tuple base-type '%anonymous))))
      (parse:nt 'declaration (declSpecs initDeclaratorList Semi))
      -> (let (((typedef? base-type) (p-declarationSpecifiers declSpecs)))
           (:tuple typedef? (p-initDeclaratorList base-type '() initDeclaratorList)))
      x -> (begin
             (printf "unhandled declaration " (repr (parse->sexp x)) "\n")
             (raise (:Hell)))
      )

    ;; ---- types ----

    (define name->type
      'unsigned -> (ctype2:int (cint:int) #f)
      'signed   -> (ctype2:int (cint:int) #t)
      'long     -> (ctype2:int (cint:long) #t)
      'int      -> (ctype2:int (cint:int) #t)
      'short    -> (ctype2:int (cint:short) #t)
      'char     -> (ctype2:int (cint:char) #t)
      'float    -> (ctype2:float #f #f)
      'double   -> (ctype2:float #f #t)
      x         -> (ctype2:name x)
      )

    (define p-typeSpecifier
      (parse:nt 'typeSpecifier ((parse:t {kind='BaseTypes val="void"})))
      -> (ctype2:name 'void)
      (parse:nt 'typeSpecifier ((parse:t {kind='BaseTypes val=name})))
      -> (name->type (string->symbol name))
      ;; (parse:nt 'typeSpecifier ((parse:nt 'typedefName ((parse:t {kind='Identifier val=name})))))
      ;; -> (ctype2:name (string->symbol name))
      ;; (parse:nt 'typeSpecifier ((parse:t {kind='Identifier val=name})))
      ;; -> (ctype2:name (string->symbol name))
      (parse:nt 'typeSpecifier ((parse:t {kind='TypedefName val=name})))
      -> (ctype2:name (string->symbol name))
      (parse:nt 'typeSpecifier ((parse:nt 'structOrUnionSpecifier subs)))
      -> (p-structOrUnionSpecifier subs)
      (parse:nt 'typeSpecifier ((parse:nt 'enumSpecifier sub)))
      -> (p-enumSpecifier (parse:nt 'enumSpecifier sub))
      (parse:nt 'typeSpecifier (tS pointer))
      -> (p-pointer (p-typeSpecifier tS) pointer)
      ;;(parse:nt 'typeSpecifier ((parse:nt 'atomicTypeSpecifier (Atomic LeftParen typeName RightParen))))
      ;; later, needs typeName.
      x -> (impossiblex x)
      )

    ;; ---- declarators ----

    (define p-initDeclaratorList
      type acc (parse:nt 'initDeclaratorList (iD))
      -> (reverse (list:cons (p-initDeclarator type iD) acc))
      type acc (parse:nt 'initDeclaratorList (iDL Comma iD))
      -> (p-initDeclaratorList type (list:cons (p-initDeclarator type iD) acc) iDL)
      type acc x -> (impossiblex x)
      )

    (define p-initDeclarator
      type (parse:nt 'initDeclarator (d))
      -> (p-declarator type d)
      type (parse:nt 'initDeclarator (d Assign init))
      -> (p-declarator type d) ;; ignore assignment.
      type x -> (impossiblex x)
      )

    (define p-declarator
      type (parse:nt 'declarator ((parse:nt 'directDeclarator dD) . ignore-decl-exts))
      -> (p-directDeclarator type dD)
      type (parse:nt 'declarator ((parse:nt 'pointer p) (parse:nt 'directDeclarator dD) . ignore-decl-exts))
      -> (let ((type0 (p-pointer type (parse:nt 'pointer p)))
               ((type1 name0) (p-directDeclarator type0 dD)))
           (:tuple type1 name0))
      type x -> (impossiblex x)
      )

    ;; the general idea here is to take the base type (stuff to the left of the name)
    ;; and apply a bunch of postfix modifiers to that type, then finally attach all this
    ;; to the name. e.g.:
    ;; int thing[3][4];
    ;; => {type=(array 4 (array 3 int)) name='thing}

    (define p-directDeclarator
      ;; x
      type ((parse:t {kind='Identifier val=ident}))
      -> (:tuple type (string->symbol ident))
      ;; (x)
      type ((parse:t {kind='LeftParen val=_}) decl (parse:t {kind='RightParen val=_}))
      -> (p-declarator type decl)
      ;; x:1
      type ((parse:t {kind='Identifier val=ident}) Colon DigitSequence)
      -> (:tuple type (string->symbol ident)) ;; ignore bitfield length
      ;; (*x)
      type ((parse:t {kind='LeftParen val=_}) p (parse:nt 'directDeclarator dD) (parse:t {kind='RightParen val=_}))
      -> (let (((type0 name0) (p-directDeclarator type dD)))
           (:tuple (p-pointer type0 p) name0))
      ;; (__stdcall *f)
      type ((parse:t {kind='LeftParen val=_}) tS p (parse:nt 'directDeclarator dD) (parse:t {kind='RightParen val=_}))
      -> (let (((type0 name0) (p-directDeclarator type dD)))
           (:tuple (p-pointer type0 p) name0))
      ;; x[]
      type ((parse:nt 'directDeclarator dD) (parse:t {kind='LeftBracket val=_}) (parse:t {kind='RightBracket val=_}))
      -> (let (((type0 name0) (p-directDeclarator type dD)))
           (:tuple (ctype2:array 0 type0) name0))
      ;; x[exp]
      type ((parse:nt 'directDeclarator dD) (parse:t {kind='LeftBracket val=_}) (parse:nt 'assignmentExpression aE) (parse:t {kind='RightBracket val=_}))
      -> (let (((type0 name0) (p-directDeclarator type dD)))
           (:tuple (ctype2:array (get-exp-value (parse:nt 'assignmentExpression aE)) type0) name0))
      ;; x()
      type ((parse:nt 'directDeclarator dD) (parse:t {kind='LeftParen val=_}) (parse:t {kind='RightParen val=_}))
      -> (let (((type0 name0) (p-directDeclarator type dD)))
           (:tuple (ctype2:function type0 (list:nil)) name0))
      ;; x(int,int)
      type ((parse:nt 'directDeclarator dD) (parse:t {kind='LeftParen val=_}) pTL (parse:t {kind='RightParen val=_}))
      -> (let (((type0 name0) (p-directDeclarator type dD))
               (params (p-parameterTypeList pTL)))
           (match type0 with
             (ctype2:pointer sub)
             -> (:tuple (ctype2:pointer (ctype2:function sub params)) name0)
             _
             -> (:tuple (ctype2:function type0 params) name0)
             ))
      type x -> (begin
                  (printf "unhandled directDeclarator " (repr (parse->sexp (parse:nt 'directDeclarator x))) "\n")
                  (raise (:Hell)))
      )

    (define p-abstractDeclarator
      type (parse:nt 'abstractDeclarator ((parse:nt 'pointer p)))
      -> (p-pointer type (parse:nt 'pointer p))
      type (parse:nt 'abstractDeclarator ((parse:nt 'directAbstractDeclarator dAD) . ignore-decl-exts))
      -> (p-directAbstractDeclarator type dAD)
      type (parse:nt 'abstractDeclarator ((parse:nt 'pointer p) (parse:nt 'directAbstractDeclarator dAD) . ignore-decl-exts))
      -> (p-pointer (p-directAbstractDeclarator type dAD) (parse:nt 'pointer p))
      type x -> (impossiblex x)
      )

    ;; an 'abstract' declarator is one without a name. [in other words, it's just a type]
    (define p-directAbstractDeclarator
      ;; (LeftParen abstractDeclarator RightParen)
      type ((parse:t {kind='LeftParen val=_}) (parse:nt 'abstractDeclarator decl) (parse:t {kind='RightParen val=_}))
      -> (p-abstractDeclarator type (parse:nt 'abstractDeclarator decl))
      ;; (LeftParen RightParen) [illegal]
      ;; (LeftParen RightParen directAbstractDeclarator-0) [illegal]
      ;; (LeftParen parameterTypeList RightParen)
      ;; (LeftParen parameterTypeList RightParen directAbstractDeclarator-0)
      type ((parse:t {kind='LeftParen val=_}) (parse:nt 'parameterTypeList pTL) (parse:t {kind='RightParen val=_}) . ignore)
      -> (ctype2:function type (p-parameterTypeList (parse:nt 'parameterTypeList pTL)))
      ;; (LeftBracket RightBracket)
      type ((parse:t {kind='LeftBracket val=_}) (parse:t {kind='RightBracket val=_}) . ignore)
      -> (ctype2:array 0 type)
      ;; (LeftBracket assignmentExpression RightBracket)
      type ((parse:t {kind='LeftBracket val=_}) aE (parse:t {kind='RightBracket val=_}) . ignore)
      -> (ctype2:array (get-exp-value aE) type)
      ;; (LeftBracket typeQualifierList RightBracket)
      type ((parse:t {kind='LeftBracket val=_}) (parse:nt 'typeQualifierList _) (parse:t {kind='RightBracket val=_}))
      -> (ctype2:array 0 type)
      ;; (LeftBracket typeQualifierList assignmentExpression RightBracket)
      type ((parse:t {kind='LeftBracket val=_}) (parse:nt 'typeQualifierList _) aE (parse:t {kind='RightBracket val=_}))
      -> (ctype2:array (get-exp-value aE) type)
      ;; (LeftBracket Static assignmentExpression RightBracket)
      type ((parse:t {kind='LeftBracket val=_}) (parse:t {kind='Static val=_}) aE (parse:t {kind='RightBracket val=_}))
      -> (ctype2:array (get-exp-value aE) type)
      ;; (LeftBracket Static typeQualifierList assignmentExpression RightBracket)
      type ((parse:t {kind='LeftBracket val=_}) (parse:t {kind='Static val=_}) tQL aE (parse:t {kind='RightBracket val=_}))
      -> (ctype2:array (get-exp-value aE) type)
      ;; (LeftBracket typeQualifierList Static assignmentExpression RightBracket)
      type ((parse:t {kind='LeftBracket val=_}) tQL (parse:t {kind='Static val=_}) aE (parse:t {kind='RightBracket val=_}))
      -> (ctype2:array (get-exp-value aE) type)
      ;; (LeftBracket Star RightBracket)
      type ((parse:t {kind='LeftBracket val=_}) (parse:t {kind='Star val=_}) (parse:t {kind='RightBracket val=_}))
      -> (ctype2:array 0 type)
      ;; the other half of the productions are (pretty much) identical, but with a recursive prefix.
      ;; (directAbstractDeclarator ...)
      type ((parse:nt 'directAbstractDeclarator dAD) . rest)
      -> (p-directAbstractDeclarator (p-directAbstractDeclarator type dAD) rest)
      type x -> (begin
                  (printf "unhandled directAbstractDeclarator "
                          (repr (parse->sexp (parse:nt 'directAbstractDeclarator x))) "\n")
                  (raise (:Hell)))
      )

    (define p-pointer
      type (parse:nt 'pointer ((parse:t {kind='Star ...})))
      -> (ctype2:pointer type)
      type (parse:nt 'pointer ((parse:t {kind='Star ...}) (parse:nt 'typeQualifierList _)))
      -> (ctype2:pointer type)
      type (parse:nt 'pointer ((parse:t {kind='Star ...}) pointer))
      -> (p-pointer (ctype2:pointer type) pointer)
      type (parse:nt 'pointer ((parse:t {kind='Star ...}) tQL pointer))
      -> (p-pointer (ctype2:pointer type) pointer)
      ;; '^' is part of the 'blocks' extension.  Treat it like '*'.
      type (parse:nt 'pointer ((parse:t {kind='Caret ...})))
      -> (ctype2:pointer type)
      type (parse:nt 'pointer ((parse:t {kind='Caret ...}) (parse:nt 'typeQualifierList _)))
      -> (ctype2:pointer type)
      type (parse:nt 'pointer ((parse:t {kind='Caret ...}) pointer))
      -> (p-pointer (ctype2:pointer type) pointer)
      type (parse:nt 'pointer ((parse:t {kind='Caret ...}) tQL pointer))
      -> (p-pointer (ctype2:pointer type) pointer)
      type x -> (begin
                  (printf "unhandled pointer " (repr (parse->sexp x)) "\n")
                  (raise (:Hell)))
      )

    ;; --- parameter lists ---
    (define p-parameterTypeList
      (parse:nt 'parameterTypeList (pL))
      -> (p-parameterList '() pL)
      (parse:nt 'parameterTypeList (pL Comma Ellipsis))
      -> (p-parameterList '() pL)
      x
      -> (impossiblex x)
      )

    (define p-parameterList
      acc (parse:nt 'parameterList (pD))
      -> (list:cons (p-parameterDeclaration pD) acc)
      acc (parse:nt 'parameterList (pL Comma pD))
      -> (p-parameterList (list:cons (p-parameterDeclaration pD) acc) pL)
      acc x
      -> (impossiblex x)
      )

    ;; not sure what's going on with dSs2, it's identical to dSs.
    (define p-parameterDeclaration
      (parse:nt 'parameterDeclaration (dSs (parse:nt 'declarator decl)))
      -> (let (((_ base-type) (p-declarationSpecifiers dSs))
               ((type name) (p-declarator base-type (parse:nt 'declarator decl))))
           (declarator:t type (maybe:yes name)))
      (parse:nt 'parameterDeclaration (dSs))
      -> (let (((_ base-type) (p-declarationSpecifiers dSs)))
           (declarator:t base-type (maybe:no)))
      (parse:nt 'parameterDeclaration (dSs (parse:nt 'abstractDeclarator aD)))
      -> (let (((_ base-type) (p-declarationSpecifiers dSs))
               (type (p-abstractDeclarator base-type (parse:nt 'abstractDeclarator aD))))
           (declarator:t type (maybe:no)))
      x -> (impossiblex x)
      )

    ;; --- struct/union ---

    ;; note: returns the constructor.
    (define p-structOrUnion
      (parse:t {kind='Struct val=_})
      -> ctype2:struct
      (parse:t {kind='Union val=_})
      -> ctype2:union
      x -> (impossiblex x)
      )


    (define p-structOrUnionSpecifier
      ((parse:nt 'structOrUnion (sOU)) ident)
      -> ((p-structOrUnion sOU) (p-identifier ident) (maybe:no))
      ((parse:nt 'structOrUnion (sOU)) ident (parse:t {kind='LeftBrace val=_}) sDL (parse:t {kind='RightBrace val=_}))
      -> ((p-structOrUnion sOU) (p-identifier ident) (maybe:yes (p-structDeclarationList '() sDL)))
      ((parse:nt 'structOrUnion (sOU)) (parse:t {kind='LeftBrace val=_}) sDL (parse:t {kind='RightBrace val=_}))
      -> ((p-structOrUnion sOU) '%anonymous (maybe:yes (p-structDeclarationList '() sDL)))
      x -> (impossiblex (parse:nt 'structOrUnionSpecifier x))
      )

    ;; this is part of the 'lex hack'.
    (define p-identifier
      (parse:nt 'identifier ((parse:t {kind='Identifier val=name})))
      -> (string->symbol name)
      (parse:nt 'identifier ((parse:t {kind='TypedefName val=name})))
      -> (string->symbol name)
      x -> (impossiblex x)
      )

    ;; (list declarator) parse -> (list declarator)
    (define p-structDeclarationList
      acc (parse:nt 'structDeclarationList (one))
      -> (append (p-structDeclaration one) acc)
      acc (parse:nt 'structDeclarationList (list one))
      -> (p-structDeclarationList (append (p-structDeclaration one) acc) list)
      acc x
      -> (impossiblex x)
      )

    ;; parse -> (list declarator)
    (define p-structDeclaration
      (parse:nt 'structDeclaration (sQL Semi))
      -> (list (declarator:t (p-specifierQualifierList sQL) (maybe:no)))
      (parse:nt 'structDeclaration (sQL sDL Semi))
      -> (let ((base-type (p-specifierQualifierList sQL)))
           (p-structDeclaratorList base-type '() sDL))
      (parse:nt 'staticAssertDeclaration _) ;; ignore
      -> (list:nil)
      x
      -> (impossiblex x)
      )

    ;; parse -> ctype2
    (define (p-specifierQualifierList ast)
      (define loop
        acc (parse:nt 'specifierQualifierList ((parse:nt 'typeSpecifier tS)))
        -> (collapse-type-list (list:cons (p-typeSpecifier (parse:nt 'typeSpecifier tS)) acc))
        acc (parse:nt 'specifierQualifierList ((parse:nt 'typeSpecifier tS) sQL))
        -> (loop (list:cons (p-typeSpecifier (parse:nt 'typeSpecifier tS)) acc) sQL)
        ;; we ignore qualifiers
        acc (parse:nt 'specifierQualifierList ((parse:nt 'typeQualifier _)))
        -> (collapse-type-list acc)
        acc (parse:nt 'specifierQualifierList ((parse:nt 'typeQualifier _) sQL))
        -> (loop acc sQL)
        acc x
        -> (impossiblex x)
        )
      (loop '() ast)
      )

    ;; type (list declarator) parse -> (list declarator)
    (define p-structDeclaratorList
      type acc (parse:nt 'structDeclaratorList (sD))
      -> (list:cons (p-structDeclarator type sD) acc)
      type acc (parse:nt 'structDeclaratorList (sDL Comma sD))
      -> (p-structDeclaratorList type (list:cons (p-structDeclarator type sD) acc) sDL)
      type acc x
      -> (impossiblex x)
      )

    ;; type parse -> declarator
    (define p-structDeclarator
      type (parse:nt 'structDeclarator ((parse:t {kind='Colon val=_}) exp))
      -> (declarator:t type (maybe:no))
      type (parse:nt 'structDeclarator (decl . ignore-bitfield))
      -> (let (((type0 name) (p-declarator type decl)))
           (declarator:t type0 (maybe:yes name)))
      type x
      -> (impossiblex x)
      )

    ;; --- enum ---

    (define p-enumSpecifier
      (parse:nt 'enumSpecifier (Enum (parse:t {kind='LeftBrace val=_}) eL RightBrace))
      -> (ctype2:enum '%anonymous (maybe:yes (p-enumeratorList '() eL)))
      (parse:nt 'enumSpecifier (Enum (parse:t {kind='LeftBrace val=_}) eL (parse:t {kind='Comma val=_}) RightBrace))
      -> (ctype2:enum '%anonymous (maybe:yes (p-enumeratorList '() eL)))
      (parse:nt 'enumSpecifier (Enum (parse:t {kind='Identifier val=name}) (parse:t {kind='LeftBrace val=_}) eL RightBrace))
      -> (ctype2:enum (string->symbol name) (maybe:yes (p-enumeratorList '() eL)))
      (parse:nt 'enumSpecifier (Enum (parse:t {kind='Identifier val=name}) (parse:t {kind='LeftBrace val=_}) eL (parse:t {kind=Comma val=_}) RightBrace))
      -> (ctype2:enum (string->symbol name) (maybe:yes (p-enumeratorList '() eL)))
      (parse:nt 'enumSpecifier (Enum (parse:t {kind='Identifier val=name})))
      -> (ctype2:enum (string->symbol name) (maybe:no))
      x
      -> (impossiblex x)
      )

    (define p-enumeratorList
      acc (parse:nt 'enumeratorList (e))
      -> (list:cons (p-enumerator e) acc)
      acc (parse:nt 'enumeratorList (eL Comma e))
      -> (p-enumeratorList (list:cons (p-enumerator e) acc) eL)
      acc x
      -> (impossiblex x)
      )

    (define p-enumerator
      (parse:nt 'enumerator (eC))
      -> (enum-pair:f (p-enumerationConstant eC))
      (parse:nt 'enumerator (eC Assign cE))
      -> (enum-pair:t (p-enumerationConstant eC) (get-exp-value cE))
      x
      -> (impossiblex x)
      )

    (define p-enumerationConstant
      (parse:nt 'enumerationConstant ((parse:t {kind='Identifier val=name})))
      -> (string->symbol name)
      x
      -> (impossiblex x)
      )

    ;; --- expressions ---
    ;; this is needed usually for array declarations, where the size is usually
    ;;  an integer constant, but sometimes a complex expression.
    (define (get-exp-value exp)
      (try
       (eval-exp exp)
       except
       (:ExpTooComplex exp0)
       -> (begin
            ;; (printf "expression too complex to evaluate: " (repr (parse->sexp exp0)) "\n")
            0)))

    ;; we evaluate simple arithmetic expressions involving *,/,%,+,-,<<, and >>.
    (define eval-exp
      (parse:t {kind='Constant val=num})
      -> (string->int num)
      (parse:nt rule (one))
      -> (eval-exp one)
      (parse:nt 'primaryExpression ((parse:t {kind='LeftParen val=_}) one (parse:t {kind='RightParen val=_})))
      -> (eval-exp one)
      (parse:nt 'multiplicativeExpression (a (parse:t {kind='Star val=_}) b))
      -> (* (eval-exp a) (eval-exp b))
      (parse:nt 'multiplicativeExpression (a (parse:t {kind='Div val=_}) b))
      -> (/ (eval-exp a) (eval-exp b))
      (parse:nt 'multiplicativeExpression (a (parse:t {kind='Mod val=_}) b))
      -> (mod (eval-exp a) (eval-exp b))
      (parse:nt 'additiveExpression (a (parse:t {kind='Plus val=_}) b))
      -> (+ (eval-exp a) (eval-exp b))
      (parse:nt 'additiveExpression (a (parse:t {kind='Minus val=_}) b))
      -> (- (eval-exp a) (eval-exp b))
      (parse:nt 'shiftExpression (a (parse:t {kind='LeftShift val=_}) b))
      -> (<< (eval-exp a) (eval-exp b))
      (parse:nt 'shiftExpression (a (parse:t {kind='RightShift val=_}) b))
      -> (>> (eval-exp a) (eval-exp b))
      otherwise
      -> (raise (:ExpTooComplex otherwise))
      )

    ;; --- body of p-declaration ---

    (let (((typedef? decls) (p-decl exp))
          (result '()))
      ;;(printf "decls: " (bold (if typedef? "typedef" "")) "\n")
      (for-list decl decls
        (match decl with
          (:tuple type name)
          -> (begin
               (when typedef?
                 (cmap/add typedef-name-map (symbol->string name))
                 #u)
               ;;(printf " name: " (bold (sym name)) " type: " (ctype2-repr type) "\n")
               (push! result (declarator:t type (maybe:yes name)))
               #u
               )
          ))
      (:tuple typedef? (reverse result))))
  ;; body of parse-translation-unit
  ;; (printf "trying:\n")
  ;; (pp (parse->sexp exp) 60)
  (p-translationUnit '() exp)
  )
