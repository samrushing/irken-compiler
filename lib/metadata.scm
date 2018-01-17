;; -*- Mode: Irken -*-

;; (metadata
;;   (item0 data0 ...)
;;   (item1 data1 ...)
;;    ...)

(define search-sexp-list
  key ((sexp:list ((sexp:symbol name) . data)) . tl)
  -> (if (eq? name key)
         data
         (search-sexp-list key tl))
  key _ -> (raise (:KeyError key))
  )

(define (fetch-metadata key)
  (match (get-metadata) with
    (sexp:list ((sexp:symbol 'metadata) . items))
    -> (search-sexp-list key items)
    x -> (raise (:MalformedMetadata x))
    ))

(define (lookup-datatype name)
  (let ((datatypes (fetch-metadata 'datatypes)))
    (printf (repr (car (search-sexp-list name datatypes))) "\n")
    ))

(define (lookup-variant name)
  (let ((variants (fetch-metadata 'variants)))
    (printf (repr (car (search-sexp-list name variants))) "\n")))

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
