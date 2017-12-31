;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/symbol.scm")
(include "lib/queue.scm")
(include "lib/set.scm")
(include "lib/alist.scm")
(include "lib/ctype.scm") ;; needed by os & io.
(include "lib/lisp_reader.scm") ;; needed by ctype
(include "lib/frb.scm")

(include "lib/map.scm")
(include "lib/stdio.scm")

(let ((FILE* (stdio/open-read sys.argv[1])))
  (for ch (stdio-char-generator FILE*)
    (printf (char ch))
    )
  (printf "\n")
  (stdio/close FILE*)
  )

