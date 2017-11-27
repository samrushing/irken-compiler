
(include "lib/basis.scm")

;; test attributes in macros
(defmacro access_field
  (access_field a) -> a.field)

(define a_rec {field="1"})
(printn (access_field a_rec))    ;; -> "1"


;; test record creation in macros
(defmacro create_record
  (create_record a) -> {field=a})

(define new_rec (create_record "test"))  ;; -> {field="test"}

(printn new_rec.field) ;; -> "test"

