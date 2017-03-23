;; -*- Mode: Irken -*-

(defmacro make-ffi
  (make-ffi name rtype nargs (formal0 ...) (ftype0 ...))
  -> (let (($pfun (%%cexp (string -> int) "dlsym" name)))
       (lambda (formal0 ...)
         (%%cexp (int char int ftype0 ...)
                 "ffi"
                 $pfun rtype nargs
                 formal0 ...))))


(define (system cmd)
  ((make-ffi "system\x00" #\i 1 (cmd) (string -> int)) cmd))

(define (abort)
  ((make-ffi "abort\x00" #\i 0 () (-> int))))

(define (tmpnam)
  ((make-ffi "tmpnam\x00" #\s 0 () (-> string))))

(define (getenv name)
  ((make-ffi "getenv\x00" #\s 1 (name) (string -> string)) name))

(define (unlink name)
  ((make-ffi "unlink\x00" #\i 1 (name) (string -> int)) name))

;; (define vm-get-object
;;   (let ((object-getter (%%cexp (-> (string -> 'a)) "getc")))
;;     (lambda (name)
;;       (%%cexp ((string -> 'a) int string -> 'a) "irk" object-getter 1 name))))

;; (define irken-argv : (vector string) (vm-get-object "argv"))

(define sys
  (let ((argv0 (%%cexp (-> (vector string)) "argv"))
        (argc (vector-length argv0)))
    { argc=argc argv=argv0 }
    ))

