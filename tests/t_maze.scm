;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(require-ffi 'posix)

;; https://10print.org/
;; 10 PRINT CHR$(205.5+RND(1)); : GOTO 10

(while #t (printf (if (= 0 (mod (posix/rand) 2)) "╱" "╲")))
