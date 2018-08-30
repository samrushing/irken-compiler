;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(require-ffi 'posix)
(%backend bytecode (update-sizeoff-table))

(%backend bytecode (printf "lookup O_TRUNC:  " (int (lookup-constant 'O_TRUNC)) "\n"))
(printf "       O_TRUNC = " (int O_TRUNC) "\n")
