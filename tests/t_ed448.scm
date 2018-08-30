;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/crypto/pem.scm")
(include "lib/codecs/base64.scm")
(include "lib/codecs/hex.scm")
(include "lib/crypto/decaf.scm")

(defmacro hexcat
  (hexcat a ...) -> (hex->string (string-concat (list a ...)))
  )

(let ((sk (hexcat
           "cd23d24f714274e744343237b93290f5"
           "11f6425f98e64459ff203e8985083ffd"
           "f60500553abc0e05cd02184bdb89c4cc"
           "d67e187951267eb328"))
      (pk (ed448-derive-public-key sk))
      (m (hexcat "0c3e544074ec63b0265e0c"))
      (sig (ed448-sign m sk pk))
      (v? (ed448-verify sig m pk))
      )
  (printf "skey = " (string->hex sk) "\n")
  (printf "pkey = " (string->hex pk) "\n")
  (printf "sig  = " (string->hex sig) "\n")
  (printf "verified?  = " (bool v?) "\n")
  (assert v?)
  )

;; test vector from RFC7748
(let ((sk0 (hexcat
            "9a8f4925d1519f5775cf46b04b5800d4ee9ee8bae8bc5565d498c28d"
            "d9c9baf574a9419744897391006382a6f127ab1d9ac2d8c0a598726b"))
      (sk1 (hexcat
            "1c306a7ac2a0e2e0990b294470cba339e6453772b075811d8fad0d1d"
            "6927c120bb5ee8972b0d3e21374c9c921b09d1b0366f10b65173992d"))
      (pk0 (x448-derive-public-key sk0))
      (pk1 (x448-derive-public-key sk1))
      (ss0 (x448-gen-shared-key sk0 pk1))
      (ss1 (x448-gen-shared-key sk1 pk0))
      (exp (hexcat "07fff4181ac6cc95ec1c16a94a0f74d12da232ce40a77552281d282b"
                   "b60c0b56fd2464c335543936521c24403085d59a449a5037514a879d")))
  (printf "sk0 " (string->hex sk0) "\n")
  (printf "sk1 " (string->hex sk1) "\n")
  (printf "pk0 " (string->hex pk0) "\n")
  (printf "pk1 " (string->hex pk1) "\n")
  (printf "ss0 " (string->hex ss0) "\n")
  (printf "ss1 " (string->hex ss1) "\n")
  (assert (string=? ss0 exp))
  (assert (string=? ss1 exp))
  )

