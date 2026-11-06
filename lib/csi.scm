;; -*- Mode: Irken -*-

;; ANSI/ECMA-48 CSI (Control Sequence Introducer) escape sequences:
;;   ESC [ <params> <intermediates> <final>
;;
;; https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
;;
;; encode with csi->string, or in format: (format (csi (csi:el 2)) ...)
;; decode terminal input with csi-get, after reading ESC and '['.

(datatype csi
  (:cuu int)      ;; cursor up
  (:cud int)      ;; cursor down
  (:cuf int)      ;; cursor forward
  (:cub int)      ;; cursor back
  (:cnl int)      ;; cursor next line
  (:cpl int)      ;; cursor prev line
  (:cha int)      ;; cursor horizontal absolute
  (:cup int int)  ;; cursor position (row, col), 1-based
  (:ed int)       ;; erase in display 0=to end 1=to start 2=all 3=all+scrollback
  (:el int)       ;; erase in line 0=to end 1=to start 2=all
  (:ds int int)   ;; device status (row, col): the reply to DSR
  (:su int)       ;; scroll up
  (:sd int)       ;; scroll down
  (:hvp int int)  ;; horizontal vertical position
  (:sgr (list int)) ;; select graphic rendition, e.g. (1 34) = bold blue
  (:dsr)          ;; device status report (asks for :ds)
  (:other char (list int)) ;; anything else: final char and numeric params
  )

;; ------------------------------------------------------------
;; SGR codes, for (csi:sgr (list ...))
;; ------------------------------------------------------------

(define SGR-reset     0)
(define SGR-bold      1)
(define SGR-dim       2)
(define SGR-italic    3)
(define SGR-underline 4)
(define SGR-reverse   7)
;; colors: foreground is 30+n, background 40+n.
(define SGR-black   0)
(define SGR-red     1)
(define SGR-green   2)
(define SGR-yellow  3)
(define SGR-blue    4)
(define SGR-magenta 5)
(define SGR-cyan    6)
(define SGR-white   7)
(define (sgr-fg color) (+ 30 color))
(define (sgr-bg color) (+ 40 color))

;; ------------------------------------------------------------
;; encoding
;; ------------------------------------------------------------

(define CSI "\x1b[")

(define csi->string
  (csi:cuu n)   -> (format CSI (int n) "A")
  (csi:cud n)   -> (format CSI (int n) "B")
  (csi:cuf n)   -> (format CSI (int n) "C")
  (csi:cub n)   -> (format CSI (int n) "D")
  (csi:cnl n)   -> (format CSI (int n) "E")
  (csi:cpl n)   -> (format CSI (int n) "F")
  (csi:cha n)   -> (format CSI (int n) "G")
  (csi:cup n m) -> (format CSI (int n) ";" (int m) "H")
  (csi:ed n)    -> (format CSI (int n) "J")
  (csi:el n)    -> (format CSI (int n) "K")
  (csi:ds n m)  -> (format CSI (int n) ";" (int m) "R")
  (csi:su n)    -> (format CSI (int n) "S")
  (csi:sd n)    -> (format CSI (int n) "T")
  (csi:hvp n m) -> (format CSI (int n) ";" (int m) "f")
  (csi:sgr ns)  -> (format CSI (join int->string ";" ns) "m")
  (csi:dsr)     -> (format CSI "6n")
  (csi:other ch nums) -> (format CSI (join int->string ";" nums) (char ch))
  )

;; would be nice to have something that could derive this automatically
(define csi->sexp
  (csi:cuu n)   -> (sexp (sym 'cuu) (int n))
  (csi:cud n)   -> (sexp (sym 'cud) (int n))
  (csi:cuf n)   -> (sexp (sym 'cuf) (int n))
  (csi:cub n)   -> (sexp (sym 'cub) (int n))
  (csi:cnl n)   -> (sexp (sym 'cnl) (int n))
  (csi:cpl n)   -> (sexp (sym 'cpl) (int n))
  (csi:cha n)   -> (sexp (sym 'cha) (int n))
  (csi:cup n m) -> (sexp (sym 'cup) (int n) (int m))
  (csi:ed n)    -> (sexp (sym 'ed) (int n))
  (csi:el n)    -> (sexp (sym 'el) (int n))
  (csi:ds n m)  -> (sexp (sym 'ds) (int n) (int m))
  (csi:su n)    -> (sexp (sym 'su) (int n))
  (csi:sd n)    -> (sexp (sym 'sd) (int n))
  (csi:hvp n m) -> (sexp (sym 'hvp) (int n) (int m))
  (csi:sgr ns)  -> (sexp (sym 'sgr) (list (map sexp:int ns)))
  (csi:dsr)     -> (sexp (sym 'dsr))
  (csi:other ch nums) -> (sexp (sym 'other) (char ch) (list (map sexp:int nums)))
  )

;; ------------------------------------------------------------
;; decoding
;; ------------------------------------------------------------

;; read the rest of a sequence, assuming ESC [ has already been read.
;;  returns the raw (:tuple params intermediates final-char-code),
;;  as result:ok, or result:err if the final byte is out of range.

(define (csi-read getch)

  (define (par? ch) (and (>= ch #x30) (<= ch #x3f)))
  (define (int? ch) (and (>= ch #x20) (<= ch #x2f)))
  (define (fin? ch) (and (>= ch #x40) (<= ch #x7e)))

  (define (read-while pred ch)
    (let loop ((r (list:nil))
               (ch ch))
      (if (pred ch)
          (loop (list:cons ch r) (char->int (getch)))
          (:tuple (list->string (map int->char (reverse r))) ch))))

  (let ((ch (char->int (getch)))
        ((params ch) (read-while par? ch))
        ((interm ch) (read-while int? ch)))
    (if (fin? ch)
        (result:ok (:tuple params interm ch))
        (result:err (:tuple params interm ch))
        )))

;; params are decimal numbers separated by ';'.  an empty (omitted)
;;  param means "use the default", which is 1 for most sequences.
;;  each param is (maybe int): (maybe:no) when omitted or not a number.

(define (csi-split-params params)
  (define (digits? s)
    (let ((ok (> (string-length s) 0)))
      (for-string ch s
        (when (not (digit? ch))
          (set! ok #f)))
      ok))
  (if (= 0 (string-length params))
      (list:nil)
      (map (lambda (s) (if (digits? s) (maybe:yes (string->int s)) (maybe:no)))
           (string-split params #\;))))

(define (csi-param ps i default)
  (match (nth-or ps i) with
    (maybe:yes (maybe:yes n)) -> n
    _ -> default))

(define nth-or
  ()        _ -> (maybe:no)
  (hd . _)  0 -> (maybe:yes hd)
  (_ . tl)  n -> (nth-or tl (- n 1))
  )

;; private sequences (params starting with < = > ?, e.g. ESC [ ? 25 h)
;;  and anything with intermediates come back as :other.

(define (csi-parse params interm final)
  (let ((plen (string-length params))
        (private? (and (> plen 0) (>= (char->int (string-ref params 0)) #x3c)))
        ;; the numbers in a private sequence follow its marker character.
        (ps (csi-split-params (if private? (substring params 1 plen) params)))
        (ch (int->char final)))
    (define (p i) (csi-param ps i 1))
    (define (p0 i) (csi-param ps i 0))
    (define (all-params)
      (map (lambda (x) (match x with (maybe:yes n) -> n (maybe:no) -> 0)) ps))
    (if (or private? (> (string-length interm) 0))
        (csi:other ch (all-params))
        (match ch with
          #\A -> (csi:cuu (p 0))
          #\B -> (csi:cud (p 0))
          #\C -> (csi:cuf (p 0))
          #\D -> (csi:cub (p 0))
          #\E -> (csi:cnl (p 0))
          #\F -> (csi:cpl (p 0))
          #\G -> (csi:cha (p 0))
          #\H -> (csi:cup (p 0) (p 1))
          #\J -> (csi:ed (p0 0))
          #\K -> (csi:el (p0 0))
          #\R -> (csi:ds (p 0) (p 1))
          #\S -> (csi:su (p 0))
          #\T -> (csi:sd (p 0))
          #\f -> (csi:hvp (p 0) (p 1))
          #\m -> (csi:sgr (if (null? ps) (list 0) (all-params)))
          #\n -> (if (= (p0 0) 6) (csi:dsr) (csi:other ch (all-params)))
          _   -> (csi:other ch (all-params))
          ))))

(define (csi-get getch)
  (match (csi-read getch) with
    (result:ok (:tuple p i k))
    -> (result:ok (csi-parse p i k))
    (result:err (:tuple p i k))
    -> (result:err (:tuple p i k))
    ))

;; decode a whole string that starts with ESC [ (handy for testing).
(define (csi-decode s)
  (let ((pos 2))
    (define (getch)
      (if (< pos (string-length s))
          (let ((ch (string-ref s pos)))
            (inc! pos)
            ch)
          #\eof))
    (csi-get getch)))
