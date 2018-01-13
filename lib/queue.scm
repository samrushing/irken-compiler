;; -*- Mode: Irken -*-

;; okasaki, pure functional.

(datatype fqueue
  (:t (list 'a) (list 'a))
  )

(define (fqueue/empty)
  (fqueue:t '() '()))

(define fqueue/empty?
  (fqueue:t () _) -> #t
  _               -> #f
  )

(define fqueue/checkf
  () b -> (fqueue:t (reverse b) '())
  f  b -> (fqueue:t f b)
  )

(define fqueue/snoc
  x (fqueue:t f b) -> (fqueue/checkf f (list:cons x b))
  )

(define fqueue/hd
  (fqueue:t () _)       -> (maybe:no)
  (fqueue:t (hd . _) _) -> (maybe:yes hd)
  )

(define fqueue/tl
  (fqueue:t () _) -> (raise (:Queue/Empty))
  (fqueue:t f b)  -> (fqueue/checkf (rest f) b)
  )

(define list->fqueue*
  ()        acc -> acc
  (hd . tl) acc -> (list->fqueue* tl (fqueue/snoc hd acc))
  )

(define (list->fqueue l)
  (list->fqueue* l (fqueue/empty)))

;; -------- imperative interface -------

(define (queue/make)
  {len=0 q=(fqueue/empty)})

(define (queue/add! q x)
  (set! q.q (fqueue/snoc x q.q))
  (inc! q.len))

(define (queue/peek q)
  (fqueue/hd q.q))

(define (queue/pop! q)
  (match (fqueue/hd q.q) with
    (maybe:yes item)
    -> (begin
         (set! q.q (fqueue/tl q.q))
         (dec! q.len)
         (maybe:yes item))
    (maybe:no)
    -> (maybe:no)
    ))

;; todo: queue/iterate (currently not used anywhere).
