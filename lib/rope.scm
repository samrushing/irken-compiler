;; -*- Mode: Irken -*-

;; https://en.wikipedia.org/wiki/Rope_(data_structure)

;; for now, this is just a simple implementation to help build/format
;;  strings like html.  Might consider changing the format macro to
;;  use it.

;; XXX we could use smart constructors to gain various properties,
;;   like most efficient packing (i.e., anything < 8 bytes is collapsed
;;   into a leaf node).

;; weight is the total size of the left side of the tree.
(datatype rope
  (:leaf string)
  (:node int rope rope) ;; weight left right
  )

;; computing total length of a given node is O(log n).
(define rope-weight
  (rope:leaf s)     acc -> (+ acc (string-length s))
  (rope:node w _ r) acc -> (rope-weight r (+ w acc))
  )

;; smart constructor [this could be wrapped with an nary macro]
(define (rope-make l r)
  (rope:node (rope-weight l 0) l r))

;; (list rope) -> rope
(define rope-cat
  ()        -> (rope:leaf "")
  (a)       -> a
  (a b)     -> (rope-make a b)
  (hd . tl) -> (rope-make hd (rope-cat tl))
  )

;; note: this makes a completely unbalanced rope.
;; (list string) -> rope
(define (list->rope l)
  (rope-cat (map rope:leaf l)))

;; rope -> (list string)
(define (rope->list r)
  (let ((result '()))
    (define walk
      (rope:leaf s)     -> (PUSH result s)
      (rope:node w l r) -> (begin (walk l) (walk r))
      )
    (walk r)
    (reverse result)
    ))

;; is there some advantage to using the weight param
;;   to place strings?
(define (rope->string r)
  (let ((len (rope-length r))
        (dst (make-string len))
        (pos 0))
    (define fill
      (rope:leaf s)
      -> (begin
           (buffer-copy s 0 (string-length s) dst pos)
           (inc! pos (string-length s)))
      (rope:node w l r)
      -> (begin (fill l) (fill r))
      )
    (fill r)
    dst))

(define (rope-length r)
  (rope-weight r 0))
