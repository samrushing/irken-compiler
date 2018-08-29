;; -*- Mode: Irken -*-

;; the kitchen sink

(require "lib/core.scm")
(require "lib/pair.scm")
(require "lib/symbol.scm")
(require "lib/string.scm")
(require "lib/format.scm")
(require "lib/sexp.scm")
(require "lib/queue.scm")
(require "lib/set.scm")
(require "lib/alist.scm")
(require "lib/stdio.scm")
(%backend bytecode (require "lib/vmffi.scm"))
(require "lib/ctype.scm") ;; needed by os & io.
(require "lib/lisp_reader.scm") ;; needed by ctype
(require "lib/os.scm")
(require "lib/io.scm")
(require "lib/frb.scm")
(require "lib/enum.scm")
(require "lib/metadata.scm") ;; access metadata
(require "lib/reflection.scm")
(require "lib/exception.scm") ;; upgrade the base exception handler.
(require "lib/map.scm")
