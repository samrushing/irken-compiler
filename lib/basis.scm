;; -*- Mode: Irken -*-

;; the kitchen sink

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/format.scm")
(include "lib/symbol.scm")
(include "lib/sexp.scm")
(include "lib/queue.scm")
(include "lib/set.scm")
(include "lib/alist.scm")
(include "lib/stdio.scm")
(include "lib/ctype.scm") ;; needed by os & io.
(include "lib/lisp_reader.scm") ;; needed by ctype
(include "lib/os.scm")
(include "lib/io.scm")
(include "lib/frb.scm")
(include "lib/enum.scm")
(include "lib/metadata.scm") ;; access metadata
(include "lib/reflection.scm")
(include "lib/exception.scm") ;; upgrade the base exception handler.
