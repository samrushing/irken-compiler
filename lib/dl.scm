;; -*- Mode: Irken -*-

(require-ffi 'dl)

;; the interface, ffi/dl_ffi.scm, isn't in the repository: the
;;  bootstrap generates it (util/gen_ffis.py).

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
