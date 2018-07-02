;; -*- Mode: Irken -*-

;; some test vectors from
;; https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/mac/gcmtestvectors.zip

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/crypto/sodium.scm")
(include "lib/codecs/hex.scm")

(defmacro hexcat
  (hexcat a ...) -> (hex->string (string-concat (LIST a ...)))
  )

(define (do-encrypt-test t)
  (let ((k (hexcat t.Key))
        (m (hexcat t.PT))
        (ad (hexcat t.AAD))
        (nonce (hexcat t.IV))
        (result (aead-aes256gcm-encrypt m k nonce ad)))
    (assert (string=? result.ciphertext (hexcat t.CT)))
    (assert (string=? result.mac (hexcat t.Tag)))
    ))

(define (do-decrypt-test t)
  (let ((k (hexcat t.Key))
        (c (hexcat t.CT))
        (ad (hexcat t.AAD))
        (nonce (hexcat t.IV))
        (mac (hexcat t.Tag))
        (result (aead-aes256gcm-decrypt c k nonce ad mac)))
    (assert (string=? result (hexcat t.PT)))
    ))

(define encrypt-tests
  (LIST
   {Key = "83688deb4af8007f9b713b47cfa6c73e35ea7a3aa4ecdb414dded03bf7a0fd3a"
        IV = "0b459724904e010a46901cf3"
        PT = "33d893a2114ce06fc15d55e454cf90c3"
        AAD = "794a14ccd178c8ebfd1379dc704c5e208f9d8424"
        CT = "cc66bee423e3fcd4c0865715e9586696"
        Tag = "0fb291bd3dba94a1dfd8b286cfb97ac5"}
   {Key = "013f549af9ecc2ee0259d5fc2311059cb6f10f6cd6ced3b543babe7438a88251"
        IV = "e45e759a3bfe4b652dc66d5b"
        PT = "79490d4d233ba594ece1142e310a9857"
        AAD = "b5fe530a5bafce7ae79b3c15471fa68334ab378e"
        CT = "619443034e4437b893a45a4c89fad851"
        Tag = "6da8a991b690ff6a442087a356f8e9e3"}
   {Key = "4b2815c531d2fceab303ec8bca739a97abca9373b7d415ad9d6c6fa9782518cc"
        IV = "47d647a72b3b5fe19f5d80f7"
        PT = "d3f6a645779e07517bd0688872e0a49b"
        AAD = "20fd79bd0ee538f42b7264a5d098af9a30959bf5"
        CT = "00be3b295899c455110a0ae833140c4d"
        Tag = "d054e3997c0085e87055b79829ec3629"}
   {Key = "2503b909a569f618f7eb186e4c4b81dbfe974c553e2a16a29aea6846293e1a51"
        IV = "e4fa3dc131a910c75f61a38b"
        PT = "188d542f8a815695c48c3a882158958c"
        AAD = "f80edf9b51f8fd66f57ce9af5967ec028245eb6e"
        CT = "4d39b5494ca12b770099a8eb0c178aca"
        Tag = "adda54ad0c7f848c1c72758406b49355"}
   ))

(define decrypt-tests
  (LIST
   {Key = "aef220035cbb9e47ce605698aa28e3b0ba50b4ffcd473bb8da2017889b38055f"
        IV = "cde7af095360ea827778761d"
        CT = "bb1cdf25717445e5a77444d488387aee"
        AAD = "f269837306abbcee2da1722f28be35163e3d8567"
        Tag = "e72340deabc1589125e9e4a2755512c7"
        PT = "9775db638e5d964fc9c70b5fe456ec14"}

   {Key = "c4a274efbec1c6818e7e0ce44e4fe6ca4815cd2435995dd80ff0ac855eb612ac"
        IV = "687305b573a5f56ce9d83a0a"
        CT = "392bd3b883ac0705c5b33a43ebd911f2"
        AAD = "9459fce3860b4823a1c20b98e7f4f46fcdc0fc1d"
        Tag = "399ca7f1f6bb603c615378f9fe16e1e0"
        PT = "87fe6e3efc6314bd99f56f84b11a01aa"}

   {Key = "91cd1fb99a58c1181a1b38689ea8241e79a1dce28d6956cd4ba65eb51975b293"
        IV = "e78c0d929e83118dc1e5eee1"
        CT = "1ff5f4f876ccc54759b6cbbe39cc075b"
        AAD = "f7b00a973d54036a9a29c518664fb8fd9f71b0d3"
        Tag = "7a290632d3f89ccd7d3083333e90a004"
        PT = "9b493ec8baf529fe219ffb1b4461b397"}

   {Key = "ce521a256e1d7afdf363a03d3e99b96bed8cf039e6ee5f241a477f3a5b5f76a9"
        IV = "d676cee000335b694fb9576f"
        CT = "c3ae3e883886ed82fbc795eb3e892834"
        AAD = "2ed50e7bdfdb8932caed2d5a9498171875d4d76d"
        Tag = "a0236b9314f25fdd20a2dcce4dc14034"
        PT = "ddf434bc6c7898c1750452015908f6b8"}
   ))

(for-list test encrypt-tests
  (do-encrypt-test test))

(for-list test decrypt-tests
  (do-decrypt-test test))
