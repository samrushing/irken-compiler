;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/codecs/hex.scm")
(include "lib/codecs/base64.scm")
(include "lib/crypto/pem.scm")
(include "lib/crypto/sodium.scm")
(include "lib/asn1/ber.scm")
(include "demo/bignum.scm")
(include "lib/crypto/sig.scm")

(defmacro hexcat
  (hexcat a ...) -> (hex->string (string-concat (LIST a ...)))
  )

;; verified with:
;; $ openssl dgst -sha256 -sigopt rsa_padding_mode:pss -sigopt rsa_pss_saltlen:digest -signature /tmp/test.sig -verify /tmp/pubkey.pem /tmp/test.txt

(define skey-pem
"-----BEGIN PRIVATE KEY-----
MIIEvwIBADANBgkqhkiG9w0BAQEFAASCBKkwggSlAgEAAoIBAQCwwj3ohpQNvcSC
qMcdLvMaXVRVCfRMTasZ9e1zXoME7PBgQLVd13JGD7au0pT9hmH9MfyPSgVc6s5X
yJoVZXWsQQfDC8dhP2a7B4o9HWiIq1/vY9NNSwja+/W4C0vbPuSol5CqBSU+koyw
LeFv/4g7GqzkL9OrB3y1sSrhIX32n63m4nGQnQKZ65BgL04wnb70QyFkBms1lWjj
8gLmDKzKtbHGHRegWNuJ1IOTkyjMMX56olChiauH4Nkd/W5Ob7oySB7xsLluJp1/
uaO4etvDf/WHQZhiUCGobMA0CRtWMjV6V45jXuNVhCKiJIrduZtzgi+xeT4uo5Go
20TdVdIhAgMBAAECggEASxj7vIMQ/BWYG/shPaDo4FauscKc7J5DB7CYxd9twvX3
2q0jYU9b2bBraTNG4b/qeex25VoJlUSdjmZIMLb+m8S1S4UBnKuXNfcqIHUSKJ7H
D6pV6AztIjzSVPNXszqxY3+hktrEzCTvaRrVIQRsGNBojDpVr+Bqd7diBGx6XAEe
mr9vjUgQ+oJe0e+Epdr0ZrVxvgPugRaT55+SRxbJbZ57hmt6Gma9FdcNpuwYEVZ0
2VMZD1SP0kSHRmfQsgYENkmUfNBs3OEOYqu5mntoiijqa5f746ks1t8agRYNkvbK
tPKSn+kxG1IQb3RjtLh8QTvj9RdXoQdLTUSyc+/DAQKBgQDgUVIQOF0ABn5bSUX8
Byr5pnb9KG53JDvL3hznUzbVjczi6yu51x6TIloSrfD7qM5N+jXWGrz00dUTtO3e
dpU5tzXHteACFb0szUh1sEqWl+iWH/nulTJ5qB0NUwSDcTZmVgVZlxuPcei26DsC
/N4rmjLe7jzTsv3UOnHiiyLMeQKBgQDJuVQels9hUyXGiXwlIpMmYMs3zYGy7XJH
+cZOX75DLDa5/qltUz3RbW9NbSzLqvVkW7ixqzF9F+eRrQgkLpV9KwembYdLlCff
8qvDEO6Yv5pFzWwmcIDWYDAO9xQgEV6JyLx+ZrmqH/b1ljHgpWzEmjyyGb2H7/sY
Q35iyxl46QKBgQDWGOfBcXpaJth9Gl+An/rFkrto/ykH7/5eo51ksIjapN/UCW+P
jaGx/wyHXZtvJvvZcyHlO1n0NT4w8/RA61+cWaAsAAxRERyg4UHyihN2zoCAoP50
V9R4RxuLdxwMtIaegCXVD6OPcsIRqW+Bd4Y5iZBaNHq798/KuwmtluzcyQKBgQCc
n/YNbiEEuy94cKVgm/DO+pau9DSA/dYexnPPxI0Q0NKJJY7dYKV9509lOaBvXoT7
8C9+W6KoMxhMxqGsnjLZpcBh22cSKq2vN1dftNTc6t/Y1L/lNLMEQFjwNa19nJ7f
smmQZyg/TGROmNiPSUIjgHIiAtRjKNS4KSeHg5R6iQKBgQC5IgyptjZFM0s8SeVI
j0W6Gu8hNqQmAAFyKoRm5HvM42o75tqRE7aXrx++OxIkPt5NYxdte4ykk1zKCcPZ
VcklpvRvC0Frj5brghEZ2KByh4RpNQminXj5vVepTpq2oCLRYgz0miLZCppDP5k8
gibm14Hw3vfegc3dB2YtcvM9xg==
-----END PRIVATE KEY-----
")

(define (get-skey)
  (match ((pem-gen (string-generator skey-pem))) with
    (maybe:yes (:tuple "PRIVATE KEY" ber))
    -> (match (decode-skey (ber->asn1 ber)) with
         (skey:rsa skey) -> skey
         _               -> (raise (:Fail1))
         )
    _ -> (raise (:Fail2))
    ))

(define (test-rsassa-pss)
  (define salt (sha256-hash.oneshot "howdy howdy"))
  (define msg "testing, testing\n")
  (define sig0
    (hexcat "54391b0fa93e89de0b56760dbd83bb9872f608c053695421458edde10a16e25a"
            "17b9b262e324538df2192884c0de9cdf9f69a30e5e6e161ea745808e057a3cee"
            "848b88bb38bc77cbbe63213530fe7f7b2d611917ca3fcaed8b3fa81a036fb10d"
            "41aeece1dc1de2addf93feb2cecc6b00b2c3940eee5357b505e75922449a77fc"
            "4c08e2f96a304ba3c36bff141c398a131d5ed859a2c1e6705514110435c9ed0f"
            "0576251458e725ddb914528caf4c50e11646fd12c4701eb008e5e9dd5c64abfc"
            "94689c6dd7d31ca05b5f202f10074a8810969948c25444ed042313b7d112cd1d"
            "bf2cccda0317a73d2a80bd33dda371ba134bf163c584a702de294091b5ba2ecd"))
  (define (RNG n)
    (printf "RNG n = " (int n) " |salt|= " (int (string-length salt)) "\n")
    (assert (= n (string-length salt)))
    (printf "salt = " (string->hex salt) "\n")
    salt)
  (let ((skey (get-skey))
        (sig1 (rsassa-pss-sign skey msg sha256-hash RNG)))
    (printf "sig0 " (string->hex sig0) "\n")
    (printf "sig1 " (string->hex sig1) "\n")
    (assert (string=? sig0 sig1))
    ))
(test-rsassa-pss)
