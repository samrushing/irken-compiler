;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(cinclude "dlfcn.h")

;; wrap as a handle type?
(define (dlopen name)
  (let ((ztname (zero-terminate name))
        (handle (%%cexp (string -> int) 
                        "dlopen (%0, RTLD_LAZY)"
                        ztname)))
    (if (= 0 handle )
        (raise (:DlOpenFailed name))
        handle)))

(define (dlsym-default name)
  (let ((ztname (zero-terminate name)))
    (%%cexp (string -> int)
            "dlsym (RTLD_DEFAULT, %0)"
            ztname)))

(define (dlsym handle name)
  (let ((ztname (zero-terminate name)))
    (%%cexp (int string -> int)
            "dlsym ((void*)%0, %1)"
            handle
            ztname)))

;; note: extension is platform-specific.
;; (let ((hpng (dlopen "libpng.dylib")))
;;   (printn (dlsym hpng "png_get_copyright"))
;;   )
;; (dlsym-default "puts")

