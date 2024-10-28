;; -*- Mode: Irken -*-

;; eventually move this into lib, and add `csi` to the format DSL.

;; https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences

;; consider just making this an enum with a list of int params.

(datatype csi
  (:cuu int)      ;; cursor up
  (:cud int)      ;; cursor down
  (:cuf int)      ;; cursor forward
  (:cub int)      ;; cursor back
  (:cnl int)      ;; cursor next line
  (:cpl int)      ;; cursor prev line
  (:cha int)      ;; cursor horizontal absolute
  (:cup int int)  ;; cursor position
  (:ed int)       ;; erase in display 0=eos 1=bos 2=all 3=extreme
  (:el int)       ;; erase in line 0=eol 1=bol 2=all
  (:ds int int)   ;; device status (response to DSR)
  (:su int)       ;; scroll up
  (:sd int)       ;; scroll down
  (:hvp int int)  ;; horizontal vertical position
  (:sgr int)      ;; select graphic rendition
  (:dsr)          ;; device status report
  (:other char (list int)) ;; support random things
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
  (csi:sgr n)   -> (sexp (sym 'sgr) (int n))
  (csi:dsr)     -> (sexp (sym 'dsr))
  (csi:other ch nums) -> (sexp (sym 'other) (char ch) (list (map sexp:int nums)))
  )

;; assuming we have read \x1b[ already

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

(define (csi-parse params interm final)
  ;; params are _usually_ decimal numbers separated by ';'
  ;; XXX handle the case with non-numeric params.
  ;; XXX this map/split won't work with cases where the semicolons are elided.
  ;;     [ probably going to need a state machine 8^) ]
  (let ((nums (map string->int (string-split params #\;))))
    (match (int->char final) nums with
      #\A (n)   -> (csi:cuu n)
      #\B (n)   -> (csi:cud n)
      #\C (n)   -> (csi:cuf n)
      #\D (n)   -> (csi:cub n)
      #\E (n)   -> (csi:cnl n)
      #\F (n)   -> (csi:cpl n)
      #\G (n)   -> (csi:cha n)
      #\H (n m) -> (csi:cup n m)
      #\J (n)   -> (csi:cha n)
      #\K (n)   -> (csi:cha n)
      #\R (n m) -> (csi:ds n m)
      #\S (n)   -> (csi:cha n)
      #\T (n)   -> (csi:cha n)
      #\f (n m) -> (csi:hvp n m)
      #\m (n)   -> (csi:sgr n)
      #\n (6)   -> (csi:dsr)
      ch nums   -> (csi:other ch nums)
      )))

(define (csi-get getch)
  (match (csi-read getch) with
    (result:ok (:tuple p i k))
    -> (result:ok (csi-parse p i k))
    (result:err (:tuple p i k))
    -> (result:err (:tuple p i k))
    ))

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
  (csi:sgr n)   -> (format CSI (int n) "m")
  (csi:dsr)     -> (format CSI "6n")
  (csi:other ch nums) -> (format CSI (join int->string ";" nums) (char ch))
  )
