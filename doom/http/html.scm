;; -*- Mode: Irken -*-

(require "lib/sexp.scm")
(require "lib/rope.scm")

;; DSL for generating HTML

(define (rope-bracket left middle right)
  (rope-make left (rope-make middle right)))

(define (open-tag name params)
  (rope-bracket
   (rope:leaf "<")
   (rope/build
    (rope:leaf (symbol->string name))
    (if (not (null? params))
        (rope:leaf (format " " (join " " (reverse params))))
        (rope:leaf "")))
   (rope:leaf ">")
   ))

(define (close-tag name)
  (rope-bracket
   (rope:leaf "</")
   (rope:leaf (symbol->string name))
   (rope:leaf ">")))

(define (empty-tag name)
  (rope-bracket
   (rope:leaf "<")
   (rope:leaf (symbol->string name))
   (rope:leaf ">")))

(define (empty-params name params)
  (rope-bracket
   (rope:leaf "<")
   (rope/build
    (rope:leaf (symbol->string name))
    (if (not (null? params))
        (rope:leaf (format " " (join " " (reverse params))))
        (rope:leaf "")))
   (rope:leaf "/>")))

(define (html-wrap tag params contents)
  (rope-bracket (open-tag tag params) contents (close-tag tag)))

;; text for HTML: anything that isn't ours (a request path, a header,
;;  a DNS name...) must go through this, or it can inject markup.
(define (html-escape s)
  (let ((r (list:nil)))
    (define (out str) (for-string ch str (push! r ch)))
    (for-string ch s
      (match ch with
        #\& -> (out "&amp;")
        #\< -> (out "&lt;")
        #\> -> (out "&gt;")
        #\" -> (out "&quot;")
        #\' -> (out "&#39;")
        _   -> (push! r ch)))
    (list->string (reverse r))))

;; attribute values are escaped (and quoted).
(defmacro hparam
  (hparam (k v)) -> (format (sym (quote (%%symbol k))) "=\"" (html-escape v) "\"")
  )

(defmacro hparams
  (hparams acc ())
  -> acc
  (hparams acc (param0 param1 ...))
  -> (hparams (list:cons (hparam param0) acc) (param1 ...))
  )

(defmacro hitem
  ;; '&=' means 'escape the html language': x is a rope, used as is.
  (hitem (<&=> x))          -> x
  ;; text: formatted, then escaped.  use this for anything untrusted.
  (hitem (<&t> x ...))      -> (rope:leaf (html-escape (format x ...)))
  ;; string (NOT escaped: see &t)
  (hitem (<&s> s))          -> (rope:leaf s)
  ;; format (NOT escaped: see &t)
  (hitem (<&f> x ...))      -> (rope:leaf (format x ...))
  ;; cat
  (hitem (<&cat> x))        -> (rope/cat x)
  ;; join
  (hitem (<&join> sep l))   -> (rope/join (rope:leaf sep) l)
  (hitem (<&join> p sep l)) -> (rope/join (rope:leaf sep) (map p l))
  ;; pretty-printed sexp
  (hitem (<&pp> exp))       -> (html (pre (&cat (generator->list (pp-html exp 80)))))
  ;; empty tag with params.
  (hitem ((tag param ...)))
  -> (empty-params (quote (%%symbol tag)) (hparams (list:nil) (param ...)))
  ;; empty tag
  (hitem (tag))
  -> (empty-tag (quote (%%symbol tag)))
  ;; tag with params.
  (hitem ((tag param ...) sub ...))
  -> (html-wrap (quote (%%symbol tag))
                (hparams (list:nil) (param ...))
                (html sub ...))
  ;; tag without params.
  (hitem (tag sub ...))
  -> (html-wrap (quote (%%symbol tag))
                (list:nil)
                (html sub ...))
  ;; lone items must be strings (NOT escaped: they're for markup and
  ;;  constant text; see &t).
  (hitem item)         -> (rope:leaf item)
  )

(defmacro html
  (html item) -> (hitem item)
  (html item0 item1 ...) -> (rope-make (hitem item0) (html item1 ...))
  )

;; XXX might be nice to have a `sexp->html` capability.
;; could use some CSS to style it up a bit.
;; we also want to pprint it. hmmm...

(define field->html
  (field:t '... _ _) -> (html "...")
  (field:t name val _) -> (html (&t (sym name) "=") (&= (sexp->html val)))
  )

;; (define sexp-css
;;   (string-join
;;    (list
;;     "pre { line-height: 125%; }"
;;     "body  { background: #f8f8f8; }"
;;     "body .c { color: #408080; font-style: italic }"
;;     "body .k { color: #008000; font-weight: bold }"
;;     "body .s { color: #BA2121; }"
;;     "body .l { color: #800000; }"
;;     "body .t { color: #800080; }"
;;     "body .f { color: #0000C0; font-weight:bold }"
;;     )
;;    "\n"))

(define sexp->html
  (sexp:undef _)     -> (html ((span (class "u")) "#u"))
  (sexp:int n _)     -> (html ((span (class "i")) (&f (int n))))
  (sexp:char ch _)   -> (html ((span (class "c")) (&t (char ch))))
  (sexp:bool b _)    -> (html ((span (class "b")) (&f (bool b))))
  (sexp:string s _)  -> (html ((span (class "s")) (&t (string s))))
  (sexp:symbol s _)  -> (html ((span (class "y")) (&t (sym s))))
  (sexp:cons 'nil c _) -> (html ((span (class "C")) (&t ":" (sym c))))
  (sexp:cons dt c _) -> (html ((span (class "C")) (&t (sym dt) ":" (sym c))))
  (sexp:list l _)    -> (html ((span (class "p")) "(") (&join sexp->html " " l) ((span (class "p")) ")"))
  (sexp:vector v _)  -> (html "#(" (&join sexp->html " " v) ")")
  (sexp:record fl _) -> (html "{" (&join field->html " " fl) "}")
  (sexp:attr lhs a _)  -> (html (&= (sexp->html lhs)) "." ((span (class "y")) (&t (sym a))))
  )

(define (pp-html exp width)

  (define (indent n)
    (rope/repeat n (rope:leaf "&nbsp;")))

  (makegen emit
    (let recur ((d 0) (exp exp))
      (let ((size (pp-size exp)))
        (if (< size width)
            (emit (sexp->html exp))
            (match exp with
              (sexp:list () _)
              -> (emit (sexp->html (sexp:list (list:nil) null-src)))
              (sexp:list (hd . tl) _)
              -> (begin (emit (html ((span (class "p")) "(")))
                        (recur d hd)
                        (for-list item tl
                          (emit (html (br)))
                          (emit (indent (+ d 1)))
                          (recur (+ d 1) item))
                        (emit (html ((span (class "p")) ")"))))
              ;; XXX complete for vector & record.
              _ -> (emit (sexp->html exp))
              ))))
    (emit (html (br)))))
