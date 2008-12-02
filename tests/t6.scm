

(include "lib/string.scm")
(include "lib/pair.scm")

;(%printn (string->list "howdy"))
;(%printn (list->string (cons #\a '())))

(%printn (string->list "howdy"))
(%printn (reverse (string->list "howdy")))
(%printn (list->string (string->list "howdy")))
