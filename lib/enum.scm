;; -*- Mode: Irken -*-

;; generate a forward-map alist
(defmacro fwdgen
  (fwdgen name (acc ...) ())
  -> (alist/make acc ...)
  (fwdgen name (acc ...) ((k0 v0) rest ...))
  -> (fwdgen name ((((%%constructor name k0)) v0) acc ...) (rest ...))
  )

;; generate a reverse-map alist
(defmacro revgen
  (revgen name (acc ...) ())
  -> (alist/make acc ...)
  (revgen name (acc ...) ((k0 v0) rest ...))
  -> (revgen name ((v0 ((%%constructor name k0))) acc ...) (rest ...))
  )

;; generate an enum->symbol alist
(defmacro symgen
  (symgen name (acc ...) ())
  -> (alist/make acc ...)
  (symgen name (acc ...) ((k0 v0) rest ...))
  -> (symgen name ((((%%constructor name k0)) (quote k0)) acc ...) (rest ...))
  )

;; generate a new datatype
(defmacro enumgen
  (enumgen name (acc ...) ())
  -> (datatype name acc ...)
  (enumgen name (acc ...) ((k0 v0) rest ...))
  -> (enumgen name (((%%constructor nil k0)) acc ...) (rest ...))
  )

;; generate an enum where each variant is associated with an integer.
;; 1) a new datatype. e.g. (datatype color ...)
;; 2) alists for mapping forward, reverse, and name. e.g. `color-fwd-alist`
;; 3) functions for those maps, e.g. color->int, int->color, color->name.
(defmacro make-enum
  (make-enum name pairs ...)
  -> (%splice
      (enumgen name () (pairs ...))
      (define (%%symbol name "-fwd-alist") (fwdgen name () (pairs ...)))
      (define (%%symbol name "-rev-alist") (revgen name () (pairs ...)))
      (define (%%symbol name "-sym-alist") (symgen name () (pairs ...)))
      (define ((%%symbol name "->int") x)  (alist/get (%%symbol name "-fwd-alist") x "err"))
      (define ((%%symbol "int->" name) x)  (alist/get (%%symbol name "-rev-alist") x "err"))
      (define ((%%symbol name "->name") x) (alist/get (%%symbol name "-sym-alist") x "err"))
      ))
