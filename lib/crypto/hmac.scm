;; -*- Mode: Irken -*-

;; RFC 2104

(define (hmac/make hash)

  ;; (int string -> string)
  (define (xor-string s val)
    (let ((len (string-length s))
          (r (make-string len)))
      (for-range i len
        (string-set! r i (int->char (logxor val (char->int (string-ref s i))))))
      r))

  (define (hash-string s)
    (let ((h (hash.make)))
      (h.update s)
      (h.final)))

  (define (zero-pad s w)
    (let ((r (make-string w))
          (slen (string-length s))
          (zero (int->char 0)))
      (for-range i w
        (string-set! r i (if (< i slen) (string-ref s i) zero)))
      r))

  ;; --- body of hmac/make ---
  (define (make key)
    (let ((inner (hash.make)))

      (define (update m)
        (inner.update m))

      (define (final)
        (let ((outer (hash.make)))
          (outer.update (xor-string key #x5c))
          (outer.update (inner.final))
          (outer.final)))

      ;; force the key size to match the hash's block size.
      ;; NOTE: these cannot be combined! (hash size != block size)
      (if (> (string-length key) hash.block-size)
          (set! key (hash-string key)))
      (if (< (string-length key) hash.block-size)
          (set! key (zero-pad key hash.block-size)))
      (inner.update (xor-string key #x36))
      {update=update final=final}
      ))

  {size=hash.size block-size=hash.block-size make=make}
  )

(define hmac/sha256 (hmac/make sha256-hash))
(define hmac/sha512 (hmac/make sha512-hash))
