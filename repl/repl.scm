;; -*- Mode: Irken -*-

(include "self/lisp_reader.scm")

;; --- s-expression input ---

(define (find-and-read-file path)
  (raise (:NoIncludeFiles))
  )

(define (file/read-line file)
  (let loop ((ch (file/read-char file))
         (r '()))
    (if (eq? ch #\newline)
    (list->string (reverse r))
    (loop (file/read-char file) (list:cons ch r)))))

(define (ask prompt file)
  (printf prompt) (flush)
  (file/read-line file))

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
  (:function (list symbol) sexp)
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

(define (repl-error what)
  (printf "error: " what "\n")
  (univ:undef)
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

;; evaluate a primitive operator (one starting with '%')

(define eval-prim
  '%+ (arg0 arg1) env
  -> (let ((a (eval arg0 env))
           (b (eval arg1 env)))
       (match a b with
         (univ:int a) (univ:int b) -> (univ:int (+ a b))
         _ _ -> (repl-error (format "bad args: " (univ-repr a) " " (univ-repr b)))
         ))
  '%- (arg0 arg1) env
  -> (let ((a (eval arg0 env))
           (b (eval arg1 env)))
       (match a b with
         (univ:int a) (univ:int b) -> (univ:int (- a b))
         _ _ -> (repl-error (format "bad args: " (univ-repr a) " " (univ-repr b)))
         ))
  prim _ _
  -> (repl-error (format "unknown prim: " (sym prim)))
  )

(define (make-function rands body)
  (let ((formals '()))
    (for-list rand rands
      (match rand with
        (sexp:symbol name)
        -> (begin (PUSH formals name) (univ:undef))
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
         (maybe:no) -> (repl-error (format "wrong number of arguments to function " (repr rator)))
         (maybe:yes new-rib)
         -> (eval body (env:rib new-rib env))
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
    (sexp:symbol s) -> (varref s env)
    ;; variable assignment
    (sexp:list ((sexp:symbol 'set!) (sexp:symbol name) val))
    -> (varset name (eval val env) env)
    ;; lambda
    (sexp:list ((sexp:symbol 'lambda) (sexp:list formals) body))
    -> (make-function formals body)
    ;; application
    (sexp:list (rator . rands))
    -> (match rator with
         (sexp:symbol name)
         -> (if (starts-with (symbol->string name) "%")
                (eval-prim name rands env)
                (eval-apply rator rands env))
         _ -> (eval-apply rator rands env))
    ;; anything else...  
    exp -> (repl-error (format "bad/unknown expression: " (repr exp)))
    ))

(define (setup-initial-environment)
  (varset 'x (univ:int 34) namespace)
  )

(define (read-eval-print-loop stdin)
  (setup-initial-environment)
  (let loop ((line (ask "> " stdin)))
    (match (string-length line) with
      0 -> #u
      _ -> (begin
         ;;(printf "line = '" line "'\n")
         (for-list exp (read-string line)
           (printf (univ-repr (eval exp namespace)) "\n"))
         (loop (ask "> " stdin)))
      )))

(read-eval-print-loop (file/open-stdin))
