;; -*- Mode: Irken -*-

;; [two] test vectors from rfc7539.

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/crypto/sodium.scm")
(include "lib/codecs/hex.scm")

(defmacro hexcat
  (hexcat a ...) -> (hex->string (string-concat (list a ...)))
  )

(define (t0)
  (let ((k (hexcat "1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0"))
        (c (hexcat
            "64a0861575861af460f062c79be643bd"
            "5e805cfd345cf389f108670ac76c8cb2"
            "4c6cfc18755d43eea09ee94e382d26b0"
            "bdb7b73c321b0100d4f03b7f355894cf"
            "332f830e710b97ce98c8a84abd0b9481"
            "14ad176e008d33bd60f982b1ff37c855"
            "9797a06ef4f0ef61c186324e2b350638"
            "3606907b6a7c02b0f9f6157b53c867e4"
            "b9166c767b804d46a59b5216cde7a4e9"
            "9040c5a40433225ee282a1b0a06c523e"
            "af4534d7f83fa1155b0047718cbc546a"
            "0d072b04b3564eea1b422273f548271a"
            "0bb2316053fa76991955ebd63159434e"
            "cebb4e466dae5a1073a6727627097a10"
            "49e617d91d361094fa68f0ff77987130"
            "305beaba2eda04df997b714d6c6f2c29"
            "a6ad5cb4022b02709b"))
        (nonce (hexcat "000000000102030405060708"))
        (ad (hexcat "f33388860000000000004e91"))
        (mac (hexcat "eead9d67890cbb22392336fea1851f38")))
    (assert
     (string=?
      (aead-chacha20poly1305-decrypt c k nonce ad mac)
      "Internet-Drafts are draft documents valid for a maximum of six months and may be updated, replaced, or obsoleted by other documents at any time. It is inappropriate to use Internet-Drafts as reference material or to cite them other than as /“work in progress./”"
      ))
    ))

(define (t1)
  (let ((m (hexcat "4c616469657320616e642047656e746c"
                   "656d656e206f662074686520636c6173"
                   "73206f66202739393a20496620492063"
                   "6f756c64206f6666657220796f75206f"
                   "6e6c79206f6e652074697020666f7220"
                   "746865206675747572652c2073756e73"
                   "637265656e20776f756c642062652069"
                   "742e"))
        (ad (hexcat "50515253c0c1c2c3c4c5c6c7"))
        (k (hexcat "808182838485868788898a8b8c8d8e8f"
                   "909192939495969798999a9b9c9d9e9f"))
        (nonce (hexcat "070000004041424344454647"))
        (result (aead-chacha20poly1305-encrypt m k nonce ad)))
    ;; (printf "ciphertext: " (string->hex result.ciphertext) "\n"
    ;;         "      mac:  " (string->hex result.mac) "\n")
    (assert
     (string=?
      result.ciphertext
      (hexcat "d31a8d34648e60db7b86afbc53ef7ec2"
              "a4aded51296e08fea9e2b5a736ee62d6"
              "3dbea45e8ca9671282fafb69da92728b"
              "1a71de0a9e060b2905d6a5b67ecd3b36"
              "92ddbd7f2d778b8c9803aee328091b58"
              "fab324e4fad675945585808b4831d7bc"
              "3ff4def08e4b7a9de576d26586cec64b"
              "6116")))
    (assert
     (string=?
      result.mac
      (hexcat "1ae10b594f09e26a7e902ecbd0600691")))
    ))

(t0)
(t1)
