;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/cmap.scm")
(include "lib/counter.scm")
(include "lib/parse/lexer.scm")
(include "lib/dfa/charset.scm")
(include "lib/dfa/rx.scm")
(include "lib/dfa/deriv.scm")
(include "lib/dfa/dfa.scm")
(include "lib/dfa/emit.scm")
(include "lib/dfa/lexicon.scm")
(include "lib/parse/earley.scm")
(include "lib/parse/parser.scm")

(include "lib/json.scm")
(include "lib/codecs/hex.scm")

;; get tests from here:
;; https://github.com/http2jp/hpack-test-case

(include "doom/http/hpack.scm")

(define json->pairs
  (json:array headers)
  -> (map
      (lambda (x)
        (match x with
          (json:obj ((:tuple name (json:string val))))
          -> (:tuple name val)
          ob -> (error1 "unexpected json" (json-repr ob))
          ))
      headers)
  ob -> (error1 "unexpected json" (json-repr ob))
  )

(define compare-pairs
  (:tuple name0 val0) (:tuple name1 val1)
  -> (when (not (and (string=? name0 name1)
                     (string=? val0 val1)))
       (printf "mismatch :\n"
               name0 ": " val0 "\n"
               name1 ": " val1 "\n")
       (raise (:TestFailed name0 name1 val0 val1))
       ))

(define print-pair
  (:tuple name val)
  -> (printf name ": " val "\n")
  )

;; some of the test inputs are improperly ordered (rfc7540/8.1.2.1),
;;   so in order to compare against our encoder we have to sort the
;;   outputs.
(define (compare-headers h0 h1 sort?)
  (when (not (eq? (cmp:=) (magic-cmp h0 h1)))
    (if sort?
        (if (not (eq? (cmp:=) (magic-cmp (sort magic<? h0) (sort magic<? h1))))
            (raise (:TestFailed1)))
        (begin
          (printf "h0:\n")
          (for-each print-pair h0)
          (printf "h1:\n")
          (for-each print-pair h1)
          (raise (:TestFailed2))
          ))))

(define (read-story story)
  (let ((decoder (hpack/decoder))
        (encoder (hpack/encoder))
        (decoder2 (hpack/decoder)) ;; can't use the same decoder!
        (total 0))
    ;;(printf "description: " (json-repr (json/get1 story "description")) "\n")
    (match (json/get1 story "cases") with
      (json:array cases)
      -> (for-list case cases
           (let ((headers (json/get1 case "headers"))
                 (seqno (json/get1 case "seqno"))
                 (wire (json/get1 case "wire")))
             ;;(printf "seqno " (json-repr seqno) "\n")
             ;;(printf "wire " (json-repr wire) "\n")
             (match wire with
               (json:string wire0)
               -> (let ((headers0 (json->pairs headers))
                        (headers1 (decoder (hex->string wire0)))
                        (wire1 (encoder headers0))
                        (headers2 (decoder2 wire1)))
                    (compare-headers headers0 headers1 #f)
                    (compare-headers headers0 headers2 #t)
                    (inc! total)
                    )
               ob -> (error1 "unexpected json" (json-repr ob))
               )))
      _ -> (error "expected json array of cases")
      )
    (printf "passed " (int total) " tests.\n")
    ))

;; read a story file from https://github.com/http2jp/hpack-test-case
(define (main)
  (set-verbose-gc #f)
  (let ((json (read-json-file sys.argv[1])))
    (read-story json)
    ))

(main)
