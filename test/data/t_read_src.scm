;; -*- Mode: Irken -*-

;; the reader gives every datum the location (line:column, both counting
;;  from 1) of its first character: lists at the paren, `'x` at the quote,
;;  postfix forms (x[i], dt:alt, x.y) at their left operand, record fields
;;  at the field name.

(include "lib/basis.scm")

(define (loc x)
  (match (sexp/src x) with
    (src:base r) -> (format (int r.line) ":" (int r.pos))
    _            -> "?"))

(define (show x)
  (printf (loc x) " " (repr x) "\n")
  (match x with
    (sexp:list l _)    -> (for-each show l)
    (sexp:vector l _)  -> (for-each show l)
    (sexp:record fl _) -> (for-list f fl
                            (match f with
                              (field:t name val src)
                              -> (begin
                                   (printf (loc (sexp:symbol name src)) " field " (sym name) "\n")
                                   (show val))))
    (sexp:attr x _ _)  -> (show x)
    _ -> #u))

(for-each show (read-string "(f 12 \"s\" #\\a)\n  'q #t\n x[3] list:nil a.b\n{x=1 y=#(2 #u)}"))
;; => 1:1 (f 12 "s" #\a)
;; => 1:2 f
;; => 1:4 12
;; => 1:7 "s"
;; => 1:11 #\a
;; => 2:3 'q
;; => 2:3 quote
;; => 2:4 q
;; => 2:6 #t
;; => 3:2 (%array-ref #f x 3)
;; => 3:2 %array-ref
;; => 3:2 #f
;; => 3:2 x
;; => 3:4 3
;; => 3:7 list:nil
;; => 3:16 a.b
;; => 3:16 a
;; => 4:1 {x=1 y=#(2 #u)}
;; => 4:2 field x
;; => 4:4 1
;; => 4:6 field y
;; => 4:8 #(2 #u)
;; => 4:10 2
;; => 4:12 #u
