;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "doom/http/hpack.scm")

(define encoded
  (list
   "\x88_\x87I|\xa5\x89\xd3M\x1fb\x8f\xe4\xc7\xf2\x17B\xf3\xe2u\xa6\xdb]\x97\x02\x1f\xe7d\x96\xdf=\xbfJ\x05\xb5!\xb6e\x04\x014\xa0\x81q\x96n4\x151h\xdf"
   "\x88_\x8b5#\x98\xac#\xbc\xdf\xef\xcd4\x7fb\x91\xe4\xc7\xf2\x16\xc4\xeb\xef4\xfbO.\xd3\xe0}\xa7\xbf\x9f\xc0"
   "\x8b\xbfb\x91\xe4\xc7\xf2\x16\xc4\xeb\xee\xb8'\x1d]\xd7\xd9m\x97\xbf\x9f\xc1"
   "\x88\xc0b\x91\xe4\xc7\xf2\x16\xc4\xeb\xee\xbe\x16\x9e]\xd7\xdfm\x97\xbf\x9f\xc2"
   ))

;; from python hpack module, same results:
;; >>> d.decode (b"\x88_\x87I|\xa5\x89\xd3M\x1fb\x8f\xe4\xc7\xf2\x17B\xf3\xe2u\xa6\xdb]\x97\x02\x1f\xe7d\x96\xdf=\xbfJ\x05\xb5!\xb6e\x04\x014\xa0\x81q\x96n4\x151h\xdf")
;; [(':status', '200'), ('content-type', 'text/html'), ('etag', 'W/"1718927455.3611"'), ('expires', 'Thu, 15 Aug 2024 20:33:41 GMT')]
;; >>> d.decode (b"\x88_\x8b5#\x98\xac#\xbc\xdf\xef\xcd4\x7fb\x91\xe4\xc7\xf2\x16\xc4\xeb\xef4\xfbO.\xd3\xe0}\xa7\xbf\x9f\xc0")
;; [(':status', '200'), ('content-type', 'image/svg+xml'), ('etag', 'W/"1527984948.490948"'), ('expires', 'Thu, 15 Aug 2024 20:33:41 GMT')]
;; >>> d.decode (b"\x8b\xbfb\x91\xe4\xc7\xf2\x16\xc4\xeb\xee\xb8'\x1d]\xd7\xd9m\x97\xbf\x9f\xc1")
;; [(':status', '304'), ('content-type', 'image/svg+xml'), ('etag', 'W/"1527976267.793538"'), ('expires', 'Thu, 15 Aug 2024 20:33:41 GMT')]
;; >>> d.decode (b"\x88\xc0b\x91\xe4\xc7\xf2\x16\xc4\xeb\xee\xbe\x16\x9e]\xd7\xdfm\x97\xbf\x9f\xc2")
;; [(':status', '200'), ('content-type', 'image/svg+xml'), ('etag', 'W/"1527979148.799538"'), ('expires', 'Thu, 15 Aug 2024 20:33:41 GMT')]
;; >>>

(define (print-headers h)
  (define print-pair
    (:tuple name val)
    -> (printf "  " name ": " val "\n")
    )
  (printf "headers {\n")
  (for-each print-pair h)
  (printf "}\n"))

(define (t0 encoded)
  (let ((decoder (hpack/decoder)))
    (for-list enc encoded
      (print-headers (decoder enc)))))

(t0 encoded)

