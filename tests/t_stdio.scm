;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(let ((FILE* (stdio/open-read sys.argv[1])))
  (for ch (stdio-char-generator FILE*)
    (printf (char ch))
    )
  (printf "\n")
  (stdio/close FILE*)
  )

