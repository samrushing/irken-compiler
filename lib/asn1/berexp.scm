;; -*- Mode: Irken -*-

;; aiming for the reverse of ber-repr
;;
;; expects 'unparsed' integers (see lib/read.scm:read2)
;;

(define (sexp->ber exp)

  (define parse-oid
    acc ()                  -> (ber:OID (reverse acc))
    acc ((sexp:int n _) . tl) -> (parse-oid (list:cons n acc) tl)
    acc x                   -> (raise (:MalformedOID (sexp:list x null-src)))
    )


  (define parse-class
    'univ -> (asn1class:univ)
    'appl -> (asn1class:appl)
    'ctxt -> (asn1class:ctxt)
    'priv -> (asn1class:priv)
    x -> (raise (:BadASN1Class x))
    )

  (define parse-string
    'OCTETSTRING           -> (asn1string:OCTET)
    'BITSTRING             -> (asn1string:BIT)
    'UTF8STRING            -> (asn1string:UTF8)
    'NUMERICSTRING         -> (asn1string:NUMERIC)
    'PRINTABLESTRING       -> (asn1string:PRINTABLE)
    'T61STRING             -> (asn1string:T61)
    'VIDEOTEXSTRING        -> (asn1string:VIDEOTEX)
    'IA5STRING             -> (asn1string:IA5)
    'UTCTIMESTRING         -> (asn1string:UTCTIME)
    'GENERALIZEDTIMESTRING -> (asn1string:GENERALIZEDTIME)
    'GRAPHICSTRING         -> (asn1string:GRAPHIC)
    'VISIBLESTRING         -> (asn1string:VISIBLE)
    'GENERALSTRING         -> (asn1string:GENERAL)
    'UNIVERSALSTRING       -> (asn1string:UNIVERSAL)
    'BMPSTRING             -> (asn1string:BMP)
    x -> (raise (:BadASN1StringType x))
    )

  (define parse
    (sexp:symbol 'NULL _)  -> (ber:NULL)
    (sexp:symbol 'TRUE _)  -> (ber:BOOLEAN #t)
    (sexp:symbol 'FALSE _) -> (ber:BOOLEAN #f)
    (sexp:int n _)         -> (ber:INTEGER n)

    (sexp:list ((sexp:symbol 'SEQUENCE _) . items) _)
    -> (ber:SEQUENCE (map parse items))

    (sexp:list ((sexp:symbol 'SET _) . items) _)
    -> (ber:SET (map parse items))

    (sexp:list ((sexp:symbol 'CONTEXT _) (sexp:int tag _) . items) _)
    -> (ber:CONTEXT tag (map parse items))

    (sexp:list ((sexp:symbol 'APPLICATION _) (sexp:int tag _) . items) _)
    -> (ber:APPLICATION tag (map parse items))

    (sexp:list ((sexp:symbol 'INTEGER _) (sexp:symbol hex _)) _)
    -> (ber:BINTEGER (symbol->string hex))

    (sexp:list ((sexp:symbol 'ENUMERATED _) (sexp:int n _)) _)
    -> (ber:ENUMERATED n)

    (sexp:list ((sexp:symbol 'OID _) . vals) _)
    -> (parse-oid '() vals)

    (sexp:list ((sexp:symbol 'RAW _) (sexp:symbol class _) (sexp:bool structured? _) (sexp:int tag _) (sexp:symbol val _)) _)
    -> (ber:RAW (parse-class class) structured? tag (symbol->string val))

    (sexp:list ((sexp:symbol other _) (sexp:string val _)) _)
    -> (ber:STRING (parse-string other) val)

    x
    -> (raise (:BadBERSexp x))
    )

  (parse exp)
  )
