;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/vmffi.scm")

;; bytecode-specific version

(require-ffi 'vari)

;; the interface, ffi/vari_ffi.scm, depends on this machine's headers,
;;  so it isn't in the repository: generate it (after a bootstrap has
;;  built genffi), from the top of the tree:
;;    $ IRKENLIB=. ffi/gen/genffi -gen ffi/vari.ffi
;;  (and run util/install.py again, if you use an installed compiler).

(let ((pfun (vm-get-sym-handle 'vari "printf\x00")))
  (printn pfun)
  (%%ffi printf pfun #\i 2 (%string->cref #f "thing1: %d\n\x00") 3141)
  (%%ffi printf pfun #\i 3 (%string->cref #f "thing2: %d %d\n\x00") 3141 1927)
  (%%ffi printf pfun #\i 2 (%string->cref #f "thing3: %s\n\x00") (%string->cref #f "Yowsa!\x00"))
  )

