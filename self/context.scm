;; -*- Mode: Irken -*-

(define (make-context)
  {datatypes = (alist/make)
    macros = (alist/make)
    dep-graph = (alist-maker)
    scc-graph = '()
    vars = (tree:empty)
    regalloc = (make-register-allocator)
    standard-macros = "self/selfmac.scm"
    records = '()
    labels = '()
    }
  )

;; XXX a builtin flags object would be nice...

(define (vars-get-var context name)
  (match (tree/member context.vars symbol<? name) with
    (maybe:no) -> (error1 "vars-get-var: no such var" name)
    (maybe:yes v) -> v))

(define (vars-get-flag context name flag)
  (let ((var (vars-get-var context name)))
    (bit-get var.flags flag)))

(define (vars-set-flag! context name flag)
  (let ((var (vars-get-var context name)))
    (set! var.flags (bit-set var.flags flag))))

(define (vars-inc-calls! context name flag)
  (let ((var (vars-get-var context name)))
    (set! var.calls (+ 1 var.calls))))

(define VFLAG-RECURSIVE 0) ;; function that is recursive
(define VFLAG-ESCAPES   1) ;; function/variable that escapes
(define VFLAG-FUNCTION  2) ;; variable is a function
(define VFLAG-ALLOCATES 3) ;; function that allocates
(define VFLAG-FREE      4) ;; function that accesses free variables
(define VFLAG-NFLAGS    5)

(define (build-vars root context)
  (let ((vars context.vars))

    (define (add-var name)
      (match (tree/member vars symbol<? name) with
	(maybe:no) -> (set! vars (tree/insert vars symbol<? name {flags=0 calls=0 refs=0 sets=0 mult=0}))
	(maybe:yes _) -> #u)) ;; fix then function, shows up twice, ignore.

    (define (search exp)
      (match exp.t with
	;; only these three bind names.
	(node:fix names)	     -> (for-each add-var names)
	(node:let names)	     -> (for-each add-var names)
	(node:function name formals) -> (begin (for-each add-var formals)
					       (add-var name))
	_ -> #u)
      (for-each search exp.subs))

    (search root)
    (add-var 'top)
    (set! context.vars vars)))

(define (lookup-label-code label context)
  (let loop ((pairs context.labels))
    (match pairs with
      () -> (error1 "lookup-label-code" label)
      ((:pair key val) . rest)
      -> (if (eq? key label)
	     val
	     (loop rest)))))

(define (print-vars context)
  (let ((flagpad (+ 2 VFLAG-NFLAGS)))
    (print-string "vars = {\n")
    (print-string
     (format "  " (cpad  6 "refs") (cpad  6 "sets") (cpad 6 "calls") (cpad 6 "mult") (lpad flagpad "flags") "  " (rpad 30 "name") "\n"))
    (tree/inorder
     (lambda (k v)
       (print-string
	(format "  "
		(lpad 6 (int v.refs))
		(lpad 6 (int v.sets))
		(lpad 6 (int v.calls))
		(lpad 6 (int v.mult))
		(lpad flagpad (flags-repr v.flags))
		"  "
		(rpad 30 (sym k))
		"\n")))
     context.vars)
    (print-string "}\n")))
