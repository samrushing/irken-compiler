;; -*- Mode: Irken -*-

;; (metadata
;;   (item0 data0 ...)
;;   (item1 data1 ...)
;;    ...)

(define (fetch-metadata key)
  (define search-sexp-list
    key ((sexp:list ((sexp:symbol name) . data)) . tl)
    -> (if (eq? name key)
           data
           (search-sexp-list key tl))
    key _ -> (raise (:Metadata/KeyError key))
    )
  (match (get-metadata) with
    (sexp:list ((sexp:symbol 'metadata) . items))
    -> (search-sexp-list key items)
    x -> (raise (:Metadata/Malformed x))
    ))

(define the-variant-label-map #())

(define (build-variant-label-map)
  (let ((variants (fetch-metadata 'variants)))
    (set! the-variant-label-map (make-vector (length variants) ""))
    (let loop ((vls variants))
      (match vls with
        ((sexp:list ((sexp:symbol name) (sexp:int tag))) . tl)
        -> (begin
             (set! the-variant-label-map[tag] name)
             (loop tl))
        _ -> #u))
    ))
