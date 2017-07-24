;; -*- Mode: Irken -*-

(require-ffi 'posix)

(define (system cmd)
  (syscall (posix/system (%string->cref #f (zero-terminate cmd)))))

(define (abort)
  (posix/abort))

(define (cref-null? x)
  (= 0 (%cref->int #f x)))

(define (tmpnam)
  (let ((buf* (make-char-buffer L_tmpnam))
        (r* (posix/tmpnam buf*)))
    (if (not (cref-null? r*))
        (let ((r (get-cstring buf*)))
          (free buf*)
          r)
        (begin
          (free buf*)
          (raise-system-error)))))

(define (getenv name)
  (let ((val* (posix/getenv (%string->cref #f (zero-terminate name)))))
    (if (cref-null? val*)
        ""
        (%c-sfromc #f val* (posix/strlen val*))
        )))

(define (unlink name)
  (syscall (posix/unlink (%string->cref #f (zero-terminate name)))))

(%backend bytecode
  (define sys
    (let ((argv0 (%%cexp (-> (vector string)) "argv"))
          (argc (vector-length argv0)))
      { argc=argc argv=argv0 }
      ))
  )

(%backend (c llvm)
  (define sys
    ;; XXX rename argc/argv to irk_argc/irk_argv
    (let ((argc (%%cexp (-> int) "argc"))
          (argv
           (let ((v (make-vector argc "")))
             (define (get-arg n)
               (copy-cstring (%%cexp (int -> cstring) "argv[%0]" n)))
             (let loop ((n argc))
               (cond ((zero? n) v)
                     (else
                      (set! v[(- n 1)] (get-arg (- n 1)))
                      (loop (- n 1))))))))
      { argc=argc argv=argv }
      ))
  )
