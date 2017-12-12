;; -*- Mode: Irken -*-

(require-ffi 'posix)
(require-ffi 'libc)

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
        (%cref->string #f val* (libc/strlen val*))
        )))

(define (unlink name)
  (syscall (posix/unlink (%string->cref #f (zero-terminate name)))))

;; VM needs to shift all argv right by one.
(define (shrink-argv v)
  (let ((n (- (vector-length v) 1))
        (r (make-vector n "")))
    (for-range i n
      (set! r[i] v[(+ i 1)]))
    r))

(define (get-argv)
  (%backend c (%%cexp (-> (vector string)) "irk_make_argv()"))
  (%backend llvm (%llvm-call ("@irk_make_argv" (-> (vector string)) ccc)))
  (%backend bytecode (shrink-argv (%%cexp (-> (vector string)) "argv")))
  )

;; note: argc is redundant, but convenient.
(define sys
  (let ((argv (get-argv))
        (argc (vector-length argv)))
    { argc=argc argv=argv }
    ))
