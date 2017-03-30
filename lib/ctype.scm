;; -*- Mode: Irken -*-

(datatype ctype
  (:name symbol)  ;; void, char, thing_t, etc...
  (:int int bool) ;; size (in bytes), signed?
  (:array int ctype)
  (:pointer ctype)
  (:struct symbol)
  (:union symbol)
  )

(define ctype-repr
  (ctype:name name)    -> (symbol->string name)
  (ctype:int size s?)  -> (format (if s? "u" "i") (int (* size 8)))
  (ctype:array size t) -> (format "(array " (int size) " " (ctype-repr t) ")")
  (ctype:pointer t)    -> (format "(* " (ctype-repr t) ")")
  (ctype:struct name)  -> (format "(struct " (sym name) ")")
  (ctype:union name)   -> (format "(union " (sym name) ")")
  )

(datatype cfield
  (:t int symbol ctype) ;; offset name type
  )

(define cfield-print
  (cfield:t offset name t)
  -> (printf "  [" (lpad 3 (int offset)) "] " (sym name) " : " (ctype-repr t) "\n"))

(datatype cdef
  ;; size name fields
  (:struct int symbol (list cfield)) 
  (:union  int symbol (list cfield))
  )

(define cdef-print
  (cdef:struct size name fields)
  -> (begin 
       (printf "struct " (sym name) " {\n")
       (for-list field fields
         (cfield-print field))
       (printf "} [" (lpad 3 (int size)) "]\n"))
  (cdef:union size name fields)
  -> (begin 
       (printf "union " (sym name) " {\n")
       (for-list field fields
         (cfield-print field))
       (printf "} [" (lpad 3 (int size)) "]\n"))
  )

;; c function signature
(datatype csig
  (:t symbol ctype (list ctype)) ;; name return-type arg-types
  )

(define csig-print
  (csig:t name rtype argtypes)
  -> (printf "(" (sym name) " " (join ctype-repr " " argtypes) " -> " (ctype-repr rtype) ")\n")
  )

;; XXX would we want a namespace for these?
(define cstructs (map-maker symbol-index-cmp))
(define cfuns (map-maker symbol-index-cmp))
