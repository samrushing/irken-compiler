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
(include "lib/crypto/ctbig.scm")

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

(define (rsa-crt skey m)
  (let ((m1 (big-exp-mod m skey.dP skey.p))
        (m2 (big-exp-mod m skey.dQ skey.q))
        (h (big-mod (big-mul skey.qInv (big-sub m1 m2)) skey.p)))
    (big-add m2 (big-mul skey.q h))))

(define (rsa-mod skey m)
  (big-exp-mod m skey.d skey.n))

(defmacro timeit
  (timeit body ...)
  -> (let (($t0 (read-cycle-counter))
           ($result (begin body ...)))
       (:tuple (- (read-cycle-counter) $t0) $result)))

(define (test-i31-rsa-crt)
  (let ((x (dec->big "527030406119664977613808298711602125568980210431883051838473061179926950602260028605335815825935010323384968889612430780647909862210418268917699059358641389288774804547822274634711862370376359412530737745425426320068165959427248155700546879545437499954898376547414485802711664675796219021299624572082412371146931673029280519418073580454899386040610922017837523559352232487509296954421046321466989381264953071072937212842545981352092841704767613199327740507312633368267363422371745978952726064863517500968855550159819830046481428977583474821835122957176776730208972174114124419176285930100997846108341635475432763324"))
        (sk (get-skey))
        ((t0 r0) (timeit (rsa-mod sk x)))
        ((t1 r1) (timeit (rsa-crt sk x)))
        ((t2 r2) (timeit (i31/rsa-crt x sk)))
        )
    (assert (eq? (cmp:=) (big-cmp r0 r1)))
    (assert (eq? (cmp:=) (big-cmp r0 r2)))
    (printf "ticks (mod): " (lpad 12 (int t0)) "\n")
    (printf "ticks (crt): " (lpad 12 (int t1)) "\n")
    (printf "ticks (i31): " (lpad 12 (int t2)) "\n")
    ))

(test-i31-rsa-crt)
