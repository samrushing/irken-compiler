;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/crypto/sodium.scm")
(include "lib/codecs/hex.scm")

(defmacro hexcat
  (hexcat a ...) -> (hex->string (string-concat (LIST a ...)))
  )

(define (t0)
  (let ((h (sha256/make))
        (h0 (begin (h.update "testing") (h.final)))
        (h1 (begin (h.update "fnord") (h.final))))
    (assert
     (string=? h0 (hexcat
                   "cf80cd8aed482d5d1527d7dc72fceff8"
                   "4e6326592848447d2dc0b0e87dfc9a90")))
    (assert
     (string=? h1 (hexcat
                   "98a278170435ed06dbcea8f6e53ed115"
                   "9b870c07491edf857300c0844daf815c")))
    (assert
     (string=? h1 (sha256 "testingfnord")))
    ))

(define (t1)
  (let ((h (sha384-hash.make))
        (h0 (begin (h.update "testing") (h.final)))
        (h1 (begin (h.update "fnord") (h.final))))
    (assert
     (string=? h0 (hexcat
                   "cf4811d74fd40504674fc3273f824fa42f755b9660a2e902"
                   "b57f1df74873db1a91a037bcee65f1a88ecd1ef57ff254c9")))
    (assert
     (string=? h1 (hexcat
                   "3bcff62f6b1e6cd2e070398bd3155ee127010ebb9e914da7"
                   "c119db9cd0baa59841c78d50f2824f13c926a52f68133312")))
    ))

(define (t2)
  (let ((h (sha512-hash.make))
        (h0 (begin (h.update "testing") (h.final)))
        (h1 (begin (h.update "fnord") (h.final))))
    (assert
     (string=? h0 (hexcat
                   "521b9ccefbcd14d179e7a1bb877752870a6d620938b28a66a107eac6e6805b9d"
                   "0989f45b5730508041aa5e710847d439ea74cd312c9355f1f2dae08d40e41d50")))
    (assert
     (string=? h1 (hexcat
                   "6180ed61d50f938b67b38ebe04e7b5d1cbe677d637b93fa3fd8dee80ef3d9311"
                   "17bd5d2476b3fe6f5b2f04bf6962883461ad10eb7d89523e1e9783dfc192028f")))
    ))

(t0)
(t1)
(t2)



