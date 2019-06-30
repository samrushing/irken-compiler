;; -*- Mode: Irken -*-

;; generate a new datatype
(defmacro enumgen
  (enumgen name (k0 v0) ...) -> (datatype name ((%%constructor nil k0)) ...)
  )
;; generate a forward-map alist
(defmacro fwdgen
  (fwdgen name (k0 v0) ...) -> (alist/make (((%%constructor name k0)) v0) ...)
  )

;; generate a reverse-map alist
(defmacro revgen
  (revgen name (k0 v0) ...) -> (alist/make (v0 ((%%constructor name k0))) ...)
  )

;; generate an enum->symbol alist
(defmacro symgen
  (symgen name (k0 v0) ...) -> (alist/make (((%%constructor name k0)) (quote k0)) ...)
  )

;; generate an enum where each variant is associated with an integer.
;; 1) a new datatype. e.g. (datatype color ...)
;; 2) alists for mapping forward, reverse, and name. e.g. `color-fwd-alist`
;; 3) functions for those maps, e.g. color->int, int->color, color->name.
(defmacro make-enum
  (make-enum name pair ...)
  -> (%%splice
      (enumgen name pair ...)
      (define (%%symbol name "-fwd-alist") (fwdgen name pair ...))
      (define (%%symbol name "-rev-alist") (revgen name pair ...))
      (define (%%symbol name "-sym-alist") (symgen name pair ...))
      (define ((%%symbol name "->int") x)  (alist/get (%%symbol name "-fwd-alist") x "err"))
      (define ((%%symbol "int->" name) x)  (alist/get (%%symbol name "-rev-alist") x "err"))
      (define ((%%symbol name "->name") x) (alist/get (%%symbol name "-sym-alist") x "err"))
      ))
