;; -*- Mode: Irken -*-

;; everything in lib/basis.scm except posix: these need only the libc and
;;  stdio interfaces, which are the same on every platform.  The compiler
;;  and genffi use this (with lib/file.scm for file I/O), so they can be
;;  built before the platform's posix interface has been generated.

(require "lib/core.scm")
(require "lib/pair.scm")
(require "lib/symbol.scm")
(require "lib/string.scm")
(require "lib/format.scm")
(require "lib/sexp.scm")
(require "lib/queue.scm")
(require "lib/set.scm")
(require "lib/alist.scm")
;; (order matters in the VM: an FFI function is looked up when its
;;  definition is evaluated, which needs vmffi.scm and ctype.scm.)
(%backend bytecode (require "lib/vmffi.scm"))
(require "lib/ctype.scm")
(require "lib/stdio.scm")
(require "lib/read.scm")
(require "lib/os.scm")
(require "lib/frb.scm")
(require "lib/enum.scm")
(require "lib/metadata.scm") ;; access metadata
(require "lib/reflection.scm")
(require "lib/exception.scm") ;; upgrade the base exception handler.
(require "lib/map.scm")
