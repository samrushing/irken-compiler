;; -*- Mode: Irken -*-


(require "lib/basis.scm")
(require "lib/codecs/base64.scm")

(printn (b64-decode "QWxhZGRpbjpvcGVuIHNlc2FtZQ=="))
(printn (b64-decode "dGVzdDoxMjPCow=="))
(printn (b64-decode "YW55IGNhcm5hbCBwbGVhcw=="))
(printn (b64-encode "test:123"))
