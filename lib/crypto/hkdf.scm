;; -*- Mode: Irken -*-

(require "lib/crypto/hmac.scm")

;; RFC 5869

(define (hkdf/make hash)

  (define (extract salt IKM)
    (let ((hmac0 (hmac/make hash))
          (hmac1 (hmac0.make salt)))
      (hmac1.update IKM)
      (hmac1.final)))

  (define (expand PRK info L)
    (let ((N (how-many L hash.size))
          (T '())
          (Tn ""))
      (when (> N 255)
        (raise (:HKDF/Length-Too-Large)))
      (for-range i N
        (let ((hmac0 (hmac/make hash))
              (hmac1 (hmac0.make PRK)))
          (hmac1.update Tn)
          (hmac1.update info)
          (hmac1.update (list->string (LIST (int->char (+ 1 i)))))
          (set! Tn (hmac1.final))
          (PUSH T Tn)))
      ;; OKM
      (substring (string-concat (reverse T)) 0 L)
      ))

  (define (oneshot salt IKM info L)
    (expand (extract salt IKM) info L))

  {extract=extract expand=expand oneshot=oneshot}
  )

(define hkdf/sha256 (hkdf/make sha256-hash))
(define hkdf/sha512 (hkdf/make sha512-hash))
