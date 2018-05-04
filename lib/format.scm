;; -*- Mode: Irken -*-

(define hex-table #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))

(define (int->hex-string n)
  (let loop ((x (abs n)) (r '()))
    (if (= 0 x)
	(list->string
	 (if (< n 0) (list:cons #\- r) r))
	(loop (>> x 4)
	      (list:cons hex-table[(logand x 15)] r))
        )))

(define (int->oct-string n)
  (let loop ((x (abs n)) (r '()))
    (if (= 0 x)
	(list->string
	 (if (< n 0) (list:cons #\- r) r))
	(loop (>> x 3)
	      (list:cons hex-table[(logand x 7)] r))
        )))

(define (int->bin-string n)
  (let loop ((x (abs n)) (r '()))
    (if (= 0 x)
        (list->string (if (< n 0) (list:cons #\- r) r))
        (loop (>> x 1)
              (list:cons (if (= 0 (logand x 1)) #\0 #\1) r))
        )))

(define (pad width s left? ch)
  (let ((n (string-length s)))
    (if (> n width)
	s ;; too wide
	(let ((np (- width n)))
	  (if left?
	      (format (list->string (n-of np ch)) s)
	      (format s (list->string (n-of np ch))))))))
(define (lpad w s ch) (pad w s #t ch))
(define (rpad w s ch) (pad w s #f ch))
(define (cpad w s ch)
  (let ((sl (string-length s))
	(lp (+ sl (/ (- w sl) 2))))
    (rpad w (lpad lp s ch) ch)))

;; ----------------------------
;; simple ansi escape sequences
;; ----------------------------

;; another good excuse for using ropes.

(defmacro ansi-color
  (ansi-color <black>)   -> "30"
  (ansi-color <red>)     -> "31"
  (ansi-color <green>)   -> "32"
  (ansi-color <yellow>)  -> "33"
  (ansi-color <blue>)    -> "34"
  (ansi-color <magenta>) -> "35"
  (ansi-color <cyan>)    -> "36"
  (ansi-color <white>)   -> "37"
  )

(defmacro ansi*
  (ansi* color bright item ...) -> (format "\x1b[" bright (ansi-color color) "m" (format item ...) "\x1b[0m")
  )

(defmacro ansi
  (ansi (<bold> color) item ...) -> (ansi* color "1;" item ...)
  (ansi color item ...)          -> (ansi* color   "" item ...)
  )

;; ---------------------

;; XXX could formatting be made more efficient using 'ropes' rather than lists?

(defmacro fitem
  (fitem (<int> n))		-> (int->string n)
  (fitem (<char> ch))		-> (char->string ch)
  (fitem (<bool> b))		-> (bool->string b)
  (fitem (<hex> n))		-> (int->hex-string n)
  (fitem (<oct> n))		-> (int->oct-string n)
  (fitem (<bin> n))             -> (int->bin-string n)
  (fitem (<sym> s))		-> (symbol->string s)
  (fitem (<join> l))		-> (string-concat l)
  (fitem (<join> sep l))	-> (string-join l sep)         ;; separate each string in <l> with <sep>
  (fitem (<join> p sep l))	-> (string-join (map p l) sep) ;; map <p> over list <l>, separate each with <sep>
  (fitem (<string> s))          -> (repr-string s)
  (fitem (<base64> s))          -> (b64-encode s)                     ;; include lib/base64.scm
  (fitem (<sexp> exp))          -> (repr exp)
  (fitem (<lpad> n item ...))	-> (lpad n (format item ...) #\space) ;; left-pad
  (fitem (<rpad> n item ...))	-> (rpad n (format item ...) #\space) ;; right-pad
  (fitem (<cpad> n item ...))	-> (cpad n (format item ...) #\space) ;; center-pad
  (fitem (<zpad> n item ...))	-> (lpad n (format item ...) #\0)     ;; zero-pad
  (fitem (<repeat> n item ...)) -> (string-concat (n-of n (format item ...)))
  ;; maybe do more ansi codes?
  (fitem (<bold> item ...))     -> (format "\x1b[1;1m" item ... "\x1b[0m")
  (fitem (<ansi> spec item ...)) -> (ansi spec item ...)
  (fitem (<maybe> mob yes no))  -> (if-maybe $x mob (yes $x) no)
  ;; XXX this might be nice?
  ;; (fitem (<tuple> x (a b c) (int a) (string b) (hex c)))
  (fitem x)			-> x	;; anything else must already be a string
  )

(defmacro formatl
  (formatl) -> (list:nil)
  (formatl item items ...) -> (list:cons (fitem item) (formatl items ...))
  )

(defmacro format
  (format item)	    -> (fitem item)
  (format item ...) -> (string-concat (formatl item ...))
  )

(defmacro printf
  (printf item ...) -> (print-string (format item ...))
  )

