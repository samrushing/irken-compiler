;; -*- Mode: Irken -*-

;; by leaving out metadata, reflection, and exception.scm, we get a much smaller output file.
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
(require "lib/ctype.scm")
(require "lib/read.scm")
(require "lib/os.scm")
(require "lib/io.scm")
(require "lib/frb.scm")
(require "lib/enum.scm")
(require "lib/map.scm")

(printn (posix/getpid))
