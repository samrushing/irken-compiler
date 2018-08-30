;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/frb.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/symbol.scm")
(include "lib/alist.scm")
(include "lib/lisp_reader.scm")

(define (vm-read-file path)
  (string-concat
   (%%cexp (string -> (list string)) "readf" path)
   ))

(for-list exp (read-string (vm-read-file "ffi/socket_ffi.scm"))
  (pp exp 0))

        



