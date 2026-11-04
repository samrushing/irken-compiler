;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/vmffi.scm")

;; bytecode-specific version

(require-ffi 'vari)

(let ((pfun (vm-get-sym-handle 'vari "printf\x00")))
  (printn pfun)
  (%%ffi printf pfun #\i 2 (%string->cref #f "thing1: %d\n\x00") 3141)
  (%%ffi printf pfun #\i 3 (%string->cref #f "thing2: %d %d\n\x00") 3141 1927)
  (%%ffi printf pfun #\i 2 (%string->cref #f "thing3: %s\n\x00") (%string->cref #f "Yowsa!\x00"))
  )

