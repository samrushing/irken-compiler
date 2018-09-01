;; -*- Mode: Irken -*-

(require-ffi 'brotli)

(define (brotli/encode gen osize level)
  (let ((BES* (brotli/BrotliEncoderCreateInstance NULL NULL NULL))
        (avail_in (halloc ulong))
        (next_in (halloc (* uchar)))
        (avail_out (halloc ulong))
        (next_out (halloc (* uchar)))
        (obuf* (%c-cast uchar (make-char-buffer osize))))

    (define (cleanup)
      (when (not (cref-null? BES*))
        (brotli/BrotliEncoderDestroyInstance BES*)
        #u)
      (when (not (cref-null? obuf*))
        (free obuf*)
        #u))

    (when (cref-null? BES*)
      (cleanup)
      (raise (:Brotli/APIFail)))

    (c-set-int avail_out osize)
    (c-set-ptr next_out obuf*)
    (brotli/BrotliEncoderSetParameter BES* BROTLI_PARAM_QUALITY level)
    (makegen emit
      (for (last? ibuf) (notify-last-gen gen)
        (c-set-int avail_in (string-length ibuf))
        (c-set-ptr next_in (%c-cast uchar (%string->cref #f ibuf)))
        (while (> (c-get-int avail_in) 0)
          (let ((r (brotli/BrotliEncoderCompressStream
                    BES*
                    (if last? BROTLI_OPERATION_FINISH BROTLI_OPERATION_PROCESS)
                    avail_in
                    next_in
                    avail_out
                    next_out
                    NULL)))
            (when (not (= 1 r))
              (cleanup)
              (raise (:Brotli/APIFail)))
            (let ((ready (- osize (c-get-int avail_out))))
              (when (not (= 0 ready))
                (emit (cref->string (%c-cast char obuf*) ready))
                (c-set-int avail_out osize)
                (c-set-ptr next_out obuf*)))
            ))
        )
      (brotli/BrotliEncoderDestroyInstance BES*)
      (free obuf*)
      )))

