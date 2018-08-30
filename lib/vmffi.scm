;; -*- Mode: Irken -*-

;; XXX would be nice to have a generic 'memoize' feature!

(define (vm-dlopen name)
  (%%cexp (string -> (cref void)) "dlopen" name))

(define (vm-dlsym0 name)
  (%%cexp (string -> (cref void)) "dlsym0" name))

(define (vm-dlsym handle name)
  (%%cexp ((cref void) string -> (cref void)) "dlsym" handle name))

(define vm-get-lib-handle
  (let ((libs (tree:empty)))
    (lambda (lib)
      (match (tree/member libs symbol-index-cmp lib) with
        (maybe:yes handle)
        -> handle
        (maybe:no)
        -> (let ((libinfo (require-ffi* lib))
                 (handle
                  (if (not (string=? "" libinfo.dll))
                      (vm-dlopen libinfo.dll)
                      (raise (:VMFFI/NoDLL lib)))))
             (tree/insert! libs symbol-index-cmp lib handle)
             handle)))))

;; XXX first, try it without the library handle, only try to
;;     load the library if it fails.  that way libc/kqueue/etc
;;     will work.
(define vm-get-sym-handle
  (let ((syms (tree:empty)))
    (lambda (lib name)
      (let ((key (:tuple lib name)))
        (match (tree/member syms magic-cmp key) with
          (maybe:yes sym-handle)
          -> sym-handle
          (maybe:no)
          -> (let ((sym-handle0 (vm-dlsym0 name))
                   (sym-handle1
                    (if (cref-null? sym-handle0)
                        (vm-dlsym (vm-get-lib-handle lib) name)
                        sym-handle0)))
               (tree/insert! syms magic-cmp key sym-handle1)
               sym-handle1)
          )))))
