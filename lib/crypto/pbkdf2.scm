;; -*- Mode: Irken -*-

;; PBKDF2 (rfc8018 5.2), with HMAC as the pseudorandom function.
;;
;; (pbkdf2/sha256 password salt iterations length) -> key

(require "lib/crypto/sodium.scm")
(require "lib/crypto/hmac.scm")

;; a xor b, both the same length.
(define (pbkdf2/xor a b)
  (let ((n (string-length a))
        (r (make-string n)))
    (for-range i n
      (string-set! r i (int->char (logxor (char->int (string-ref a i))
                                          (char->int (string-ref b i))))))
    r))

;; a 32-bit big-endian integer.
(define (pbkdf2/int32 i)
  (list->string
   (map (lambda (shift) (int->char (logand #xff (>> i shift))))
        '(24 16 8 0))))

(define (pbkdf2/make hmac)

  (define (prf key data)
    (let ((h (hmac.make key)))
      (h.update data)
      (h.final)))

  ;; the <i>th block: U_1 xor U_2 xor ... U_c
  (define (block password salt iterations i)
    (let ((u (prf password (string-append salt (pbkdf2/int32 i))))
          (t u))
      (for-range j (- iterations 1)
        (set! u (prf password u))
        (set! t (pbkdf2/xor t u)))
      t))

  (lambda (password salt iterations len)
    (when (< iterations 1)
      (raise (:PBKDF2/BadIterations iterations)))
    (let ((nblocks (how-many len hmac.size))
          (blocks (list:nil)))
      (for-range i nblocks
        (push! blocks (block password salt iterations (+ i 1))))
      (substring (string-concat (reverse blocks)) 0 len))))

(define pbkdf2/sha256 (pbkdf2/make hmac/sha256))
(define pbkdf2/sha512 (pbkdf2/make hmac/sha512))
