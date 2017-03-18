;; -*- Mode: Irken -*-

(include "lib/core.scm")

;; test ffi from irken-vm

(defmacro make-ffi
  (make-ffi name rtype nargs (formal0 ...) (ftype0 ...))
  -> (let (($p (%%cexp (string -> int) "dlsym" name)))
       (lambda (formal0 ...)
         (%%cexp (int char int ftype0 ...)
                 "ffi"
                 $p rtype nargs
                 formal0 ...))))

(define (tmpnam)
  ((make-ffi "tmpnam\x00" #\s 0 () (-> string))))

(define (write fd s)
  ((make-ffi "write\x00"
             #\i 3
             (fd s slen)
             (int string int -> int))
   fd s (string-length s)))

(write 1 "yabba dabba dooooooo\n")
(printn (tmpnam))
