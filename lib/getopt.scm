;; -*- Mode: Irken -*-

;; simple getopt facility

;; option value - this represents both the result
;;  and default values.
;; XXX maybe add comma-separated symbol list?

(datatype optval
  (:bool bool)
  (:string string)
  (:int int)
  (:null)
  (:list (list optval))
  )

;; XXX add help string...

(datatype option
 (:flag symbol optval)          ;; name value
 (:arg symbol optval)           ;; name default
 (:pos symbol bool bool optval) ;; name &rest? required? default
 )

(define option->name
  (option:flag name _)    -> name
  (option:arg name _)     -> name
  (option:pos name _ _ _) -> name
  )

(define option->default
  (option:flag _ d)    -> d
  (option:arg _ d)     -> d
  (option:pos _ _ _ d) -> d
  )

(define (process-options options-spec arglist)

  (define (as-flag s)
    (format "-" (sym s)))

  (define arg-matches?
    (option:flag name _) (arg . rest)
    -> (string=? (as-flag name) arg)
    (option:arg name default) (arg val . rest)
    -> (string=? (as-flag name) arg)
    (option:pos name _ _ _) (arg . rest)
    -> (not (eq? (string-ref arg 0) #\-))
    _ _ -> #f
    )

  (define parse-optval
    (optval:string _) val -> (optval:string val)
    (optval:int _)    val -> (optval:int (string->int val))
    _ _                   -> (optval:null)
    )

  (define (get-value opt args)
    (match opt args with
      (option:flag name _) (flag . rest)
      -> (:tuple name (optval:bool #t) rest)
      (option:arg name ov)  (arg value . rest)
      -> (:tuple name (parse-optval ov value) rest)
      (option:pos name #f _ ov) (value . rest)
      -> (:tuple name (parse-optval ov value) rest)
      (option:pos name #t _ ov ) args
      -> (:tuple name
                 (optval:list
                  (map
                   (lambda (arg)
                     (parse-optval ov arg))
                   args))
                 '())
      _ _
      -> (begin
           (printf "unable to process option:\n")
           (printn opt)
           (printn args)
           (raise (:Getopt/Error "get-value" opt args)))
      ))

  (define (scan)
    (let ((args arglist)
          (result (list:nil))
          (found #f))
      (while (not (null? args))
        (for-list opt options-spec
          (if (arg-matches? opt args)
              (let (((name value rest) (get-value opt args)))
                (set! found #t)
                (push! result (:tuple name value))
                (set! args rest))))
        (if (not found)
            (begin
              (printf "Unwanted argument: " (first args) "\n")
              (raise (:Getopt/UnknownArg "unwanted argument" args)))
            (set! found #f)))
      ;; add in defaults when necessary
      (let ((found (map (lambda (:tuple name _) name) result)))
        (for-list opt options-spec
          (if (not (member-eq? (option->name opt) found))
              (match opt with
                (option:pos name _ #t ov)
                -> (begin
                     (printf "Missing argument: '" (sym name) "'\n")
                     (raise (:Getopt/MissingArg "missing argument" name)))
                _ -> #u ;; (push! result (:tuple (option->name opt) (option->default opt)))
                ))))
      (reverse result)))

  (scan)
  )

;; DSL for building option lists. (see tests/t_getopt.scm for samples)

(defmacro optval
  (optval (<int>))            -> (optval:int 0)
  (optval (<int> default))    -> (optval:int default)
  (optval (<string>))         -> (optval:string "")
  (optval (<string> default)) -> (optval:string default)
  (optval (<list> default))   -> (optval:list (list (optval default)))
  (optval (<bool>)        )   -> (optval:bool #f)
  (optval (<bool> default))   -> (optval:bool default)
  )

(defmacro optitem
  (optitem (<flag> name))      -> (option:flag name (optval:bool #f))
  (optitem (<arg> name oval))  -> (option:arg name (optval oval))
  (optitem (<pos> name oval))  -> (option:pos name #f #t (optval oval))
  (optitem (<pos+> name oval)) -> (option:pos name #t #t (optval oval))
  (optitem (<pos*> name oval)) -> (option:pos name #t #f (optval oval))
  )

(defmacro makeopt*
  (makeopt* acc ()) -> acc
  (makeopt* acc (item0 items ...))
  -> (makeopt* (list:cons (optitem item0) acc) (items ...))
  )

(defmacro makeopt
  (makeopt opt ...)
  -> (reverse (makeopt* () (opt ...)))
  )

(define (getopt given name)
  (let loop ((options given))
    (match options with
      ()
      -> (maybe:no)
      ((:tuple name0 value) . rest)
      -> (if (eq? name0 name)
             (maybe:yes value)
             (loop rest))
      )))

(define (get-bool-opt given name)
  (match (getopt given name) with
    (maybe:yes (optval:bool v)) -> (maybe:yes v)
    _ -> (maybe:no)
    ))
(define (get-string-opt given name)
  (match (getopt given name) with
    (maybe:yes (optval:string s)) -> (maybe:yes s)
    _ -> (maybe:no)
    ))
(define (get-int-opt given name)
  (match (getopt given name) with
    (maybe:yes (optval:int n)) -> (maybe:yes n)
    _ -> (maybe:no)
    ))
