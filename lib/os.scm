;; -*- Mode: Irken -*-

(require-ffi 'posix)
(require-ffi 'libc)

(define (system cmd)
  (syscall (libc/system (%string->cref #f (zero-terminate cmd)))))

(define (abort)
  (libc/abort))

(define (tmpnam)
  (let ((buf* (make-char-buffer L_tmpnam))
        (r* (libc/tmpnam buf*)))
    (if (not (cref-null? r*))
        (let ((r (get-cstring buf*)))
          (free buf*)
          r)
        (begin
          (free buf*)
          (raise-system-error)))))

(define (getenv name)
  (let ((val* (libc/getenv (%string->cref #f (zero-terminate name)))))
    (if (cref-null? val*)
        ""
        (%cref->string #f val* (libc/strlen val*))
        )))

(define (unlink name)
  (syscall (posix/unlink (%string->cref #f (zero-terminate name)))))

