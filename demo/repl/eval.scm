;; -*- Mode: Irken -*-

;; --- universal datatype ---
;;
;; this datatype covers all the types known by the interpreter.
;;

(datatype univ
  (:int int)
  (:char char)
  (:string string)
  (:bool bool)
  (:symbol symbol)
  (:undef)
  (:list (list univ))
  (:function (list symbol) sexp) ;; formals body
  )

;; how to print out a universal value

(define univ-repr
  (univ:int n)    -> (format (int n))
  (univ:char n)   -> (format (char n))
  (univ:string s) -> (format (string s))
  (univ:bool b)   -> (format (bool b))
  (univ:symbol s) -> (format (sym s))
  (univ:undef)    -> "#u"
  (univ:list subs)
  -> (format "(" (join univ-repr " " subs) ")")
  (univ:function rands body)
  -> (format "<function (" (join symbol->string " " rands) ") " (repr body) ">")
  )

;; lexical environment.
;;
;; a 'rib' consists of a set of variable bindings.
;; when a function is called, the arguments are evaluated
;;  and bound to their formal variable names in a new rib,
;;  which is pushed onto the environment for that call.

;; the keys are symbols, the values are a record type containing a single
;;  field named 'val', of type 'univ'.
;;  the values must be placed in some kind of mutable storage, for set! to work.

(datatype env
  (:empty)
  (:rib (alist symbol {val=univ}) env)
  )


(define namespace (env:empty))

(define (get-cell name env)
  (match env with
    (env:empty)
    -> (maybe:no)
    (env:rib rib next)
    -> (match (alist/lookup rib name) with
         (maybe:no) -> (get-cell name next)
         (maybe:yes cell) -> (maybe:yes cell)
         )
    ))

(define (varref name env)
  (match (get-cell name env) with
    (maybe:no) -> (repl-error (format "undefined variable: " (sym name)))
    (maybe:yes {val=val}) -> val
    ))

(define (varset name val env)
  (match (get-cell name env) with
    (maybe:no) ;; not defined, create a top-level entry
    -> (let ((cell {val=val}))
         (match namespace with
           (env:empty)
           -> (set! namespace (env:rib (alist:entry name cell (alist:nil)) (env:empty)))
           (env:rib rib next)
           -> (set! namespace (env:rib (alist:entry name cell rib) next))
           ))
    (maybe:yes cell)
    -> (set! cell.val val)
    )
  (univ:undef)
  )

(define (int-bin-op env op arg0 arg1)
  (let ((a (eval arg0 env))
        (b (eval arg1 env)))
    (match a b with
      (univ:int a) (univ:int b) -> (univ:int (op a b))
      _ _ -> (repl-error (format "bad args: " (univ-repr a) " " (univ-repr b)))
      )))

;; evaluate a primitive operator (one starting with '%')

(define eval-prim
  '%+ (arg0 arg1) env -> (int-bin-op env binary+ arg0 arg1)
  '%- (arg0 arg1) env -> (int-bin-op env binary- arg0 arg1)
  '%* (arg0 arg1) env -> (int-bin-op env binary* arg0 arg1)
  '%/ (arg0 arg1) env -> (int-bin-op env / arg0 arg1)
  '%cons (arg0 arg1) env
  -> (let ((a (eval arg0 env))
           (b (eval arg1 env)))
       (match b with
         (univ:list lx)
         -> (univ:list (cons a lx))
         _ -> (repl-error (format "bad args: " (univ-repr b)))))
  '%nil () env
  -> (univ:list '())
  '%car (arg) env
  -> (match (eval arg env) with
       (univ:list ())
       -> (repl-error (format "car of empty list" (repr arg)))
       (univ:list (hd . tl))
       -> hd
       x -> (repl-error (format "car of non-list" (univ-repr x))))
  prim _ _
  -> (repl-error (format "unknown prim: " (sym prim)))
  )

(define (make-function rands body)
  (let ((formals '()))
    (for-list rand rands
      (match rand with
        (sexp:symbol name _)
        -> (begin (push! formals name) (univ:undef))
        _ -> (repl-error (format "bad formals: " (repr (sexp:list rands))))
        ))
    (univ:function (reverse formals) body)
    ))

;; build an environment rib for these formals
;;  and operands, ready to be pushed onto the lexical
;;  environment.
(define (eval-args fun formals rands env)
  (let loop ((formals formals)
             (rands rands)
             (rib (alist:nil)))
    (match formals rands with
      (name . formals) (arg . rands)
      -> (loop formals rands (alist:entry name {val=(eval arg env)} rib))
      () ()
      -> (maybe:yes rib)
      _ _
      -> (maybe:no)
      )))

(define (eval-apply rator rands env)
  (match (eval rator env) with
    (univ:function formals body)
    -> (match (eval-args rator formals rands env) with
         (maybe:yes new-rib) -> (eval body (env:rib new-rib env))
         (maybe:no)          -> (repl-error (format "wrong number of arguments to function " (repr rator)))
         )
    op -> (repl-error (format "operator is not a function: " (univ-repr op)))
    ))

;; top-level eval function

(define (eval exp env)
  (match exp with
    ;; self-evaluating expressions
    (sexp:int n)    -> (univ:int n)
    (sexp:char ch)  -> (univ:char ch)
    (sexp:string s) -> (univ:string s)
    (sexp:bool b)   -> (univ:bool b)
    (sexp:undef)    -> (univ:undef)
    ;; variable lookup
    (sexp:symbol s _) -> (varref s env)
    ;; variable assignment
    (sexp:list ((sexp:symbol 'set! _) (sexp:symbol name _) val))
    -> (varset name (eval val env) env)
    ;; lambda
    (sexp:list ((sexp:symbol 'lambda _) (sexp:list formals) body))
    -> (make-function formals body)
    ;; application
    (sexp:list (rator . rands))
    -> (match rator with
         (sexp:symbol name _)
         -> (if (starts-with (symbol->string name) "%")
                (eval-prim name rands env)
                (eval-apply rator rands env))
         _ -> (eval-apply rator rands env))
    ;; anything else...
    exp -> (repl-error (format "bad/unknown expression: " (repr exp)))
    ))

