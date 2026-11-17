;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(require-ffi 'vari)

;; the interface, ffi/vari_ffi.scm, depends on this machine's headers,
;;  so it isn't in the repository: generate it (after a bootstrap has
;;  built genffi), from the top of the tree:
;;    $ IRKENLIB=. ffi/gen/genffi -gen ffi/vari.ffi
;;  (and run util/install.py again, if you use an installed compiler).

;;(vari/printf (%string->cref #f "thing: %d\n\x00") 3141)
(%%ffi printf (%string->cref #f "thing: %d\n\x00") 3141)

