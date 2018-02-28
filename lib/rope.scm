;; -*- Mode: Irken -*-

;; https://en.wikipedia.org/wiki/Rope_(data_structure)

;; for now, this is just a simple implementation to help build/format
;;  strings like html.  Might consider changing the format macro to
;;  use it.

(datatype rope
  (:leaf string)
  (:node rope rope)
  )

(define rope-cat
  ()        -> (rope:leaf "") ;; or error?
  (a)       -> a
  (a b)     -> (rope:node a b)
  (hd . tl) -> (rope:node hd (rope-cat tl))
  )

(define (rope->list r)
  (let ((result '()))
    (define walk
      (rope:leaf s)   -> (PUSH result s)
      (rope:node l r) -> (begin (walk l) (walk r))
      )
    (walk r)
    (reverse result)
    ))

(define (rope->string r)
  (string-concat (rope->list r)))
