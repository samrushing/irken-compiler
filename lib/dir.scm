;; -*- Mode: Irken -*-

;; reading directories (needs posix: struct dirent and DT_* vary by platform).

(require-ffi 'posix)

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
    (let loop ((dirent* (posix/readdir DIR*)))
      (cond ((not (cref-null? dirent*))
             (push! result {kind = (get-file-type dirent*) name = (get-file-name dirent*)})
             (loop (posix/readdir DIR*)))
            (else #u)))
    (posix/closedir DIR*)
    (reverse result)
    ))
