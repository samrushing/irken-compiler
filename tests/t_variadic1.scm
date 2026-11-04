;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(require-ffi 'stdio)

(%%ffi printf (%string->cref #f "thing1: %d\n\x00") 3141)
(%%ffi printf (%string->cref #f "thing2: %d %c\n\x00") 100 65)
(%%ffi printf (%string->cref #f "thing3: %d %s\n\x00") 100 "Yowsa!\x00")

