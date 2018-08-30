;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/codecs/base85.scm")

(defmacro CAT
  (CAT a ...) -> (string-concat (list a ...))
  )

;; sample from https://en.wikipedia.org/wiki/Ascii85

(define t0
  (CAT "Man is distinguished, not only by his reason, but by this singul"
       "ar passion from other animals, which is a lust of the mind, that"
       " by a perseverance of delight in the continued and indefatigable"
       " generation of knowledge, exceeds the short vehemence of any car"
       "nal pleasure.---" ;; note padding.
       ))

(define e0
  (CAT "9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,"
       "O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKY"
       "i(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIa"
       "l(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G"
       ">uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/hJY,"
       ))

(define u0
  '(#x4d616e20 #x69732064 #x69737469 #x6e677569 #x73686564 #x2c206e6f
    #x74206f6e #x6c792062 #x79206869 #x73207265 #x61736f6e #x2c206275
    #x74206279 #x20746869 #x73207369 #x6e67756c #x61722070 #x61737369
    #x6f6e2066 #x726f6d20 #x6f746865 #x7220616e #x696d616c #x732c2077
    #x68696368 #x20697320 #x61206c75 #x7374206f #x66207468 #x65206d69
    #x6e642c20 #x74686174 #x20627920 #x61207065 #x72736576 #x6572616e
    #x6365206f #x66206465 #x6c696768 #x7420696e #x20746865 #x20636f6e
    #x74696e75 #x65642061 #x6e642069 #x6e646566 #x61746967 #x61626c65
    #x2067656e #x65726174 #x696f6e20 #x6f66206b #x6e6f776c #x65646765
    #x2c206578 #x63656564 #x73207468 #x65207368 #x6f727420 #x76656865
    #x6d656e63 #x65206f66 #x20616e79 #x20636172 #x6e616c20 #x706c6561
    #x73757265 #x2e2d2d2d
    ))

(define tests-passed 0)

(defmacro assert2
  (assert2 exp)
  -> (if (not exp)
         (begin
           (printf "assertion failed: " (repr (car (%%sexp exp))) "\n")
           (raise (:AssertionFailed)))
         (set! tests-passed (+ tests-passed 1))))

;; test u32->ch
(assert2 (string=? t0 (list->string (generator->list (u32->ch (list-generator u0))))))
;; test ch->u32
(assert2 (eq? (cmp:=) (list-cmp int-cmp u0 (generator->list (ch->u32 (string-generator t0))))))
;; test u32->b85
(assert2 (string=? e0 (list->string (generator->list (u32->b85 (list-generator u0))))))
;; test b85->u32
(assert2 (eq? (cmp:=) (list-cmp int-cmp u0 (generator->list (b85->u32 (string-generator e0))))))
;; test ch->u32->b85
(assert2 (string=? e0 (list->string (generator->list (u32->b85 (ch->u32 (string-generator t0)))))))
;; test b85->u32->ch
(assert2 (string=? t0 (list->string (generator->list (u32->ch (b85->u32 (string-generator e0)))))))

(printf "passed " (int tests-passed) " tests.\n")
