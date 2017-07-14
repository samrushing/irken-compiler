;; -*- Mode: Irken -*-

(define (system cmd)
  ((make-ffi "system\x00" #\i 1 (cmd) (string -> int)) (zero-terminate cmd)))

(define (abort)
  ((make-ffi "abort\x00" #\i 0 () (-> int))))

(define (tmpnam)
  ((make-ffi "tmpnam\x00" #\s 0 () (-> string))))

(define (getenv name)
  ((make-ffi "getenv\x00" #\s 1 (name) (string -> string)) (zero-terminate name)))

(define (unlink name)
  ((make-ffi "unlink\x00" #\i 1 (name) (string -> int)) (zero-terminate name)))

(define sys
  (let ((argv0 (%%cexp (-> (vector string)) "argv"))
        (argc (vector-length argv0)))
    { argc=argc argv=argv0 }
    ))

