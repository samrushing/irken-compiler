;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/codecs/base64.scm")

(define t0 "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

(define e0 "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")

(define u0 '(#x13585bb #x081a5cf #x08191a7 #x1cdd1a7 #x1b99dd7 #x1a5cda3 #x19590b3
             #x081b9bf #x1d081bf #x1b9b1e7 #x08189e7 #x081a1a7 #x1cc81cb #x19585cf
             #x1bdb8b3 #x08189d7 #x1d0818b #x1e481d3 #x1a1a5cf #x081cda7 #x1b99dd7
             #x1b185cb #x081c187 #x1cdcda7 #x1bdb883 #x199c9bf #x1b481bf #x1d1a197
             #x1c88187 #x1b9a5b7 #x185b1cf #x0b081df #x1a1a58f #x1a081a7 #x1cc8187
             #x081b1d7 #x1cdd083 #x1bd9883 #x1d1a197 #x081b5a7 #x1b990b3 #x081d1a3
             #x185d083 #x189e483 #x18481c3 #x195c9cf #x195d997 #x1c985bb #x18d9483
             #x1bd9883 #x19195b3 #x1a59da3 #x1d081a7 #x1b881d3 #x1a19483 #x18dbdbb
             #x1d1a5bb #x1d59593 #x08185bb #x19081a7 #x1b99197 #x19985d3 #x1a59d87
             #x189b197 #x0819d97 #x1b995cb #x185d1a7 #x1bdb883 #x1bd9883 #x1adb9bf
             #x1ddb197 #x1919d97 #x0b08197 #x1e18d97 #x19591cf #x081d1a3 #x19481cf
             #x1a1bdcb #x1d081db #x195a197 #x1b595bb #x18d9483 #x1bd9883 #x185b9e7
             #x0818d87 #x1c9b987 #x1b081c3 #x1b19587 #x1cdd5cb #x194b802))

;; test u26->ch
(assert (string=? t0 (list->string (generator->list (u26->ch (list-generator u0))))))
;; test ch->u26
(assert (eq? (cmp:=) (list-cmp int-cmp u0 (generator->list (ch->u26 (string-generator t0))))))
;; test u26->b64
(assert (string=? e0 (list->string (generator->list (u26->b64 (list-generator u0))))))
;; test b64->u26
(assert (eq? (cmp:=) (list-cmp int-cmp u0 (generator->list (b64->u26 (string-generator e0))))))
;; test ch->u26->b64
(assert (string=? e0 (list->string (generator->list (u26->b64 (ch->u26 (string-generator t0)))))))
;; test b64->u26->ch
(assert (string=? t0 (list->string (generator->list (u26->ch (b64->u26 (string-generator e0)))))))

(assert (string=? "Zm5vcmQwMQ==" (b64-encode "fnord01")))
(assert (string=? "fnord01" (b64-decode "Zm5vcmQwMQ==")))

;; from the wikipedia page
(assert (string=? "YW55IGNhcm5hbCBwbGVhcw==" (b64-encode "any carnal pleas")))
(assert (string=? "YW55IGNhcm5hbCBwbGVhc3U=" (b64-encode "any carnal pleasu")))
(assert (string=? "YW55IGNhcm5hbCBwbGVhc3Vy" (b64-encode "any carnal pleasur")))

(assert (string=? "any carnal pleas" (b64-decode "YW55IGNhcm5hbCBwbGVhcw==")))
(assert (string=? "any carnal pleasu" (b64-decode "YW55IGNhcm5hbCBwbGVhc3U=")))
(assert (string=? "any carnal pleasur" (b64-decode "YW55IGNhcm5hbCBwbGVhc3Vy")))
