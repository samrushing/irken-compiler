;; -*- Mode: Irken -*-

;; lib/json.scm: parsing, including empty objects and arrays, and
;;  compact rendering.

(require "lib/basis.scm")
(require "lib/parse/lexer.scm")
(require "lib/dfa/charset.scm")
(require "lib/dfa/rx.scm")
(require "lib/dfa/deriv.scm")
(require "lib/dfa/dfa.scm")
(require "lib/dfa/emit.scm")
(require "lib/dfa/lexicon.scm")
(require "lib/parse/earley.scm")
(require "lib/parse/parser.scm")
(require "lib/codecs/hex.scm")
(require "lib/json.scm")

(define parse (make-json-parser))

(define (show s)
  (printf (try (json->string (parse (string-generator s)))
               except
               (:NoParse _) -> "no parse")
          "\n"))

(show "{\"a\": [1, true, false, null, -5, \"x\\\"y\"]}")
(show "{}")
(show "[]")
(show "{\"extensions\": {}, \"accepts\": [], \"extra\": {\"name\": \"USDC\"}}")
(show "[{}, {}, [{}]]")
(show "{\"a\": ")
(show "{,}")
;; => {"a":[1,true,false,null,-5,"x\"y"]}
;; => {}
;; => []
;; => {"extensions":{},"accepts":[],"extra":{"name":"USDC"}}
;; => [{},{},[{}]]
;; => no parse
;; => no parse

;; members are found by name.
(let ((j (parse (string-generator "{\"x402Version\": 2, \"error\": \"none\", \"resource\": {}}"))))
  (printf (json/get-string j "error") "\n")
  (printf (json->string (match (json/get j "resource") with (maybe:yes r) -> r (maybe:no) -> (json:null))) "\n"))
;; => none
;; => {}
