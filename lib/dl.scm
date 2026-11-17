;; -*- Mode: Irken -*-

(require-ffi 'dl)

;; the interface, ffi/dl_ffi.scm, depends on this machine's headers,
;;  so it isn't in the repository: generate it (after a bootstrap has
;;  built genffi), from the top of the tree:
;;    $ IRKENLIB=. ffi/gen/genffi -gen ffi/dl.ffi
;;  (and run util/install.py again, if you use an installed compiler).

(define (dlopen name)
  (let ((ztname (zero-terminate name))
        (handle (dl/dlopen (cstring ztname) (logior RTLD_LAZY RTLD_LOCAL))))
    (if (cref-null? handle)
        (raise (:DL/OpenFailed name))
        handle)))

(define (dlsym handle name) : ((cref void) string -> (cref void))
  (let ((ztname (zero-terminate name))
        (result (dl/dlsym handle (cstring ztname))))
    (if (cref-null? result)
        (raise (:DL/SymFailed name))
        result)))

;; note: extension is platform-specific.
;; (let ((hpng (dlopen "libpng.dylib")))
;;   (printn (dlsym hpng "png_get_copyright"))
;;   )
;; (dlsym-default "puts")
