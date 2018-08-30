;; -*- Mode: Irken -*-

(require-ffi 'zlib)

(define zlib_version
  (format (int ZLIB_VER_MAJOR) "."
          (int ZLIB_VER_MINOR) "."
          (int ZLIB_VER_REVISION)))

(define (deflate gen osize level)

  (let ((Z* (malloc (struct z_stream_s)))
        (obuf* (make-char-buffer osize))
        (zlib_version* (cstring zlib_version)))

    (when (or (< level 0) (> level Z_BEST_COMPRESSION))
      (raise (:Zlib/BadLevel level)))

    (makegen emit
      (c-set-ptr (%c-sref z_stream_s.zalloc Z*) NULL)
      (c-set-ptr (%c-sref z_stream_s.zfree Z*) NULL)
      (c-set-ptr (%c-sref z_stream_s.opaque Z*) NULL)
      (when (not (= Z_OK (zlib/deflateInit_ Z*
                                            level
                                            zlib_version*
                                            (%c-sizeof (struct z_stream_s)))))
        (zlib/deflateEnd Z*)
        (free Z*)
        (free obuf*)
        (raise (:Zlib/APIFailure 'deflateInit_)))

      (define (out-loop flush)
        (let ((done? #f)
              (avail_out 0)
              (ready 0))
          (while (not done?)
            (c-set-int (%c-sref z_stream_s.avail_out Z*) osize)
            (c-set-ptr (%c-sref z_stream_s.next_out Z*) (%c-cast uchar obuf*))
            (zlib/deflate Z* flush)
            (set! avail_out (c-get-int (%c-sref z_stream_s.avail_out Z*)))
            (set! ready (- osize avail_out))
            (when (> ready 0)
              (emit (%cref->string #f obuf* ready)))
            (set! done? (not (= 0 avail_out)))
            )))

      (for ibuf gen
        (c-set-int (%c-sref z_stream_s.avail_in Z*) (string-length ibuf))
        (c-set-ptr (%c-sref z_stream_s.next_in Z*) (%c-cast uchar (%string->cref #f ibuf)))
        (out-loop Z_NO_FLUSH))
      (out-loop Z_FINISH)
      (zlib/deflateEnd Z*)
      (free Z*)
      (free obuf*)
      )))

(define (inflate gen osize)

  (let ((Z* (malloc (struct z_stream_s)))
        (obuf* (make-char-buffer osize))
        (zlib_version* (cstring zlib_version)))

    (makegen emit
      (c-set-ptr (%c-sref z_stream_s.zalloc Z*) NULL)
      (c-set-ptr (%c-sref z_stream_s.zfree Z*) NULL)
      (c-set-ptr (%c-sref z_stream_s.opaque Z*) NULL)
      (when (not (= Z_OK (zlib/inflateInit_ Z*
                                            zlib_version*
                                            (%c-sizeof (struct z_stream_s)))))
        (c-set-int (%c-sref z_stream_s.avail_in Z*) 0)
        (c-set-ptr (%c-sref z_stream_s.next_in Z*) NULL)
        (zlib/inflateEnd Z*)
        (free Z*)
        (free obuf*)
        (raise (:Zlib/APIFailure 'inflateInit_)))

      (define (out-loop flush)
        (let ((done? #f)
              (avail_out 0)
              (ready 0))
          (while (not done?)
            (c-set-int (%c-sref z_stream_s.avail_out Z*) osize)
            (c-set-ptr (%c-sref z_stream_s.next_out Z*) (%c-cast uchar obuf*))
            (match (zlib/inflate Z* flush) with
              0 -> #u ;; Z_OK
              1 -> #u ;; Z_STREAM_END
              n -> (raise (:Zlib/APIFailure 'inflate)))
            (set! avail_out (c-get-int (%c-sref z_stream_s.avail_out Z*)))
            (set! ready (- osize avail_out))
            (when (> ready 0)
              (emit (%cref->string #f obuf* ready)))
            (set! done? (not (= 0 avail_out)))
            )))

      (for ibuf gen
        (c-set-int (%c-sref z_stream_s.avail_in Z*) (string-length ibuf))
        (c-set-ptr (%c-sref z_stream_s.next_in Z*) (%c-cast uchar (%string->cref #f ibuf)))
        (out-loop Z_NO_FLUSH))
      (out-loop Z_FINISH)
      (zlib/inflateEnd Z*)
      (free Z*)
      (free obuf*)
      )))

