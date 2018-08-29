;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/crypto/sodium.scm")
(include "lib/codecs/hex.scm")
(include "lib/crypto/hmac.scm")
(include "lib/crypto/hkdf.scm")

(defmacro hexcat
  (hexcat a ...) -> (hex->string (string-concat (list a ...)))
  )

;; test vectors from rfc 5869
;; note: only those for sha256

(define (test-case test)
  (let ((hkdf0 (hkdf/sha256.oneshot test.salt test.IKM test.info test.L)))
    (assert (string=? hkdf0 test.exp256))))

(define tests
  (list
   ;; test 1
   {IKM=(hexcat "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b")
    salt=(hexcat "000102030405060708090a0b0c")
    info=(hexcat "f0f1f2f3f4f5f6f7f8f9")
    L=42
    exp256=(hexcat
            "3cb25f25faacd57a90434f64d0362f2a"
            "2d2d0a90cf1a5a4c5db02d56ecc4c5bf"
            "34007208d5b887185865")}
   ;; test 2
   {IKM=(hexcat
         "000102030405060708090a0b0c0d0e0f"
         "101112131415161718191a1b1c1d1e1f"
         "202122232425262728292a2b2c2d2e2f"
         "303132333435363738393a3b3c3d3e3f"
         "404142434445464748494a4b4c4d4e4f")
    salt=(hexcat
          "606162636465666768696a6b6c6d6e6f"
          "707172737475767778797a7b7c7d7e7f"
          "808182838485868788898a8b8c8d8e8f"
          "909192939495969798999a9b9c9d9e9f"
          "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf")
    info=(hexcat
          "b0b1b2b3b4b5b6b7b8b9babbbcbdbebf"
          "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf"
          "d0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
          "e0e1e2e3e4e5e6e7e8e9eaebecedeeef"
          "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff")
    L=82
    exp256=(hexcat
            "b11e398dc80327a1c8e7f78c596a4934"
            "4f012eda2d4efad8a050cc4c19afa97c"
            "59045a99cac7827271cb41c65e590e09"
            "da3275600c2f09b8367793a9aca3db71"
            "cc30c58179ec3e87c14c01d5c1f3434f"
            "1d87")}
   ;; test 3
   {IKM=(hexcat "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b")
    salt=""
    info=""
    L=42
    exp256=(hexcat
            "8da4e775a563c18f715f802a063c5a31"
            "b8a11f5c5ee1879ec3454e5f3c738d2d"
            "9d201395faa4b61a96c8")}
   ))

(for-list test tests
  (test-case test))



