;; -*- Mode: Irken -*-

(include "self/lisp_reader.scm")

(datatype thing
  (:one)
  (:two)
  (:three)
  )

(defmacro dt-numalts
  (dt-numalts name)
  -> (let ((dtsexp (datatype->sexp name)))
       (match dtsexp with
         (sexp:list ((sexp:symbol name) (sexp:list alts)))
         -> (length alts)
         _ -> (impossible)
         ))
  )

(pp (datatype->sexp thing) 20)
(dt-numalts thing)

;; this macro generates an alist mapping the name of each alt to its index.
(defmacro pgen
  (pgen (name (alt ...)))
  -> (pgen* name () alt ...)
  )

(defmacro pgen*
  (pgen* name (acc ...))
  -> (alist/make acc ...)
  (pgen* name (acc ...) (altname index types) alts ...)
  -> (pgen* name (((quote altname) index) acc ...) alts ...)
  )

(define thing-alist (dtreflect pgen thing))
;; ultimately becomes: 
;;   (alist:entry 'three 2 (alist:entry 'two 1 (alist:entry 'one 0 (alist:nil))))

(match (alist/lookup thing-alist 'two) with
  (maybe:yes tag) -> (printf "thing:two has tag " (int tag) "\n")
  (maybe:no)      -> (impossible)
  )
