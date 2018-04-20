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

(make-enum dirent-file-type
  (regular      DT_REG)
  (directory    DT_DIR)
  (fifo         DT_FIFO)
  (socket       DT_SOCK)
  (char         DT_CHR)
  (block        DT_BLK)
  (symlink      DT_LNK)
  )

(define (listdir path)

  (define (get-file-type dirent*)
    (let ((val (c-get-int (%c-sref dirent.d_type dirent*))))
      (match (alist/lookup dirent-file-type-rev-alist val) with
        (maybe:yes ft) -> (dirent-file-type->name ft)
        (maybe:no) -> 'unknown
        )))

  (define (get-file-name dirent*)
    (get-cstring (%c-sref dirent.d_name dirent*)))

  (let ((DIR* (syscall-null (posix/opendir (cstring path))))
        (result '()))
    (printn DIR*)
    (let loop ((dirent* (posix/readdir DIR*)))
      (cond ((not (cref-null? dirent*))
             (PUSH result {kind = (get-file-type dirent*) name = (get-file-name dirent*)})
             (loop (posix/readdir DIR*)))
            (else #u)))
    (posix/closedir DIR*)
    (reverse result)
    ))
    
