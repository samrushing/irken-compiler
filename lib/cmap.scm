;; -*- Mode: Irken -*-

;; 'cmap' captures the common pattern of 'counting unique elements,
;; giving each an index starting at zero'.  It provides forward and
;; reverse maps, and a counter.

(define (cmap/make <)
  {map=(tree/empty) rev=(tree/empty) count=0 lt=<}
  )

(define (cmap/add m item)
  (match (tree/member m.map m.lt item) with
    (maybe:yes index) -> index
    (maybe:no) 
    -> (let ((index m.count))
	 (tree/insert! m.map m.lt item index)
	 (tree/insert! m.rev < index item)
	 (set! m.count (+ 1 m.count))
	 index)))

(define (cmap->index m item)
  (match (tree/member m.map m.lt item) with
    (maybe:yes index) -> index
    (maybe:no) -> (error1 "cmap->index: no such item" item)
    ))

(define (cmap->item m index)
  (match (tree/member m.rev < index) with
    (maybe:yes item) -> item
    (maybe:no) -> (error1 "cmap->item: no such index" index)
    ))
