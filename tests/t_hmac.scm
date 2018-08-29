;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/crypto/sodium.scm")
(include "lib/codecs/hex.scm")
(include "lib/crypto/hmac.scm")

(defmacro hexcat
  (hexcat a ...) -> (hex->string (string-concat (list a ...)))
  )

(define (t0)
  (let ((hmac (hmac/sha512.make "frobnicator")))
    (hmac.update "the quick brown fox jumped over the lazy dog")
    (assert
     (string=?
      (hmac.final)
      (hexcat
       "3d0a0ba73a72829e6cf4e0f7baabb53c"
       "63a3e9055f9e67b894503ecf0a897eeb"
       "ac393347e00d1b765a155a3dcf403628"
       "2636b71a57642f3739aa403097fcd422")
      ))
    ))

;; test vectors from rfc4231
;; note: only for sha256 & sha512, tests for 224 & 384 left out.

(define (test-case test)
  (let ((hmac0 (hmac/sha256.make test.k))
        (hmac1 (hmac/sha512.make test.k)))
    (for-list part test.d
      (hmac0.update part)
      (hmac1.update part))
    (assert (string=? (hmac0.final) test.exp256))
    (assert (string=? (hmac1.final) test.exp512))
    ))

(define tests
  (list {k=(format (repeat 20 "\x0b"))
           d=(list "Hi There")
           exp256=(hexcat "b0344c61d8db38535ca8afceaf0bf12b"
                          "881dc200c9833da726e9376c2e32cff7")
           exp512=(hexcat "87aa7cdea5ef619d4ff0b4241a1d6cb0"
                          "2379f4e2ce4ec2787ad0b30545e17cde"
                          "daa833b7d6b8a702038b274eaea3f4e4"
                          "be9d914eeb61f1702e696c203a126854")}
        {k="Jefe"
           d=(list "what do ya want " "for nothing?")
           exp256=(hexcat "5bdcc146bf60754e6a042426089575c7"
                          "5a003f089d2739839dec58b964ec3843")
           exp512=(hexcat "164b7a7bfcf819e2e395fbe73b56e0a3"
                          "87bd64222e831fd610270cd7ea250554"
                          "9758bf75c05a994a6d034f65f8f0e6fd"
                          "caeab1a34d4a6b4b636e070a38bce737")}
        {k=(format (repeat 20 "\xaa"))
           d=(list (format (repeat 50 "\xdd")))
           exp256=(hexcat "773ea91e36800e46854db8ebd09181a7"
                          "2959098b3ef8c122d9635514ced565fe")
           exp512=(hexcat "fa73b0089d56a284efb0f0756c890be9"
                          "b1b5dbdd8ee81a3655f83e33b2279d39"
                          "bf3e848279a722c806b485a47e67c807"
                          "b946a337bee8942674278859e13292fb")}
        {k=(hexcat "0102030405060708090a0b0c0d0e0f10"
                   "111213141516171819")
           d=(list (format (repeat 50 "\xcd")))
           exp256=(hexcat "82558a389a443c0ea4cc819899f2083a"
                          "85f0faa3e578f8077a2e3ff46729665b")
           exp512=(hexcat "b0ba465637458c6990e5a8c5f61d4af7"
                          "e576d97ff94b872de76f8050361ee3db"
                          "a91ca5c11aa25eb4d679275cc5788063"
                          "a5f19741120c4f2de2adebeb10a298dd")}
        {k=(format (repeat 131 "\xaa"))
           d=(list "Test Using Large" "r Than Block-Siz" "e Key - Hash Key" " First")
           exp256=(hexcat "60e431591ee0b67f0d8a26aacbf5b77f"
                          "8e0bc6213728c5140546040f0ee37f54")
           exp512=(hexcat "80b24263c7c1a3ebb71493c1dd7be8b4"
                          "9b46d1f41b4aeec1121b013783f8f352"
                          "6b56d037e05f2598bd0fd2215d6a1e52"
                          "95e64f73f63f0aec8b915a985d786598")}
        {k=(format (repeat 131 "\xaa"))
           d=(list (hexcat "54686973206973206120746573742075"
                           "73696e672061206c6172676572207468"
                           "616e20626c6f636b2d73697a65206b65"
                           "7920616e642061206c61726765722074"
                           "68616e20626c6f636b2d73697a652064"
                           "6174612e20546865206b6579206e6565"
                           "647320746f2062652068617368656420"
                           "6265666f7265206265696e6720757365"
                           "642062792074686520484d414320616c"
                           "676f726974686d2e"))
           exp256=(hexcat "9b09ffa71b942fcb27635fbcd5b0e944"
                          "bfdc63644f0713938a7f51535c3a35e2")
           exp512=(hexcat "e37b6a775dc87dbaa4dfa9f96e5e3ffd"
                          "debd71f8867289865df5a32d20cdc944"
                          "b6022cac3c4982b10d5eeb55c3e4de15"
                          "134676fb6de0446065c97440fa8c6a58")}
        ))

(t0)

(for-list test tests
 (test-case test))
