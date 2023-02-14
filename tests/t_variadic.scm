;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(require-ffi 'vari)

;;(vari/printf (%string->cref #f "thing: %d\n\x00") 3141)
(%%ffi printf (%string->cref #f "thing: %d\n\x00") 3141)

