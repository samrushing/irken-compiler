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

(define fqueue/cons
  x (fqueue:t f b) -> (fqueue:t (list:cons x f) b)
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

;; returns oldest-first
(define fqueue->list
  (fqueue:t hd tl) -> (reverse-onto hd tl)
  )

;; -------- imperative interface -------

(define (queue/make)
  {len=0 q=(fqueue/empty)})

(define (queue/push! q x)
  (set! q.q (fqueue/snoc x q.q))
  (inc! q.len))

(define (queue/push-front! q x)
  (set! q.q (fqueue/cons x q.q))
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

;; note: newest first: i.e. (newest ... oldest)
(define (queue/gen q)
  (makegen emit
    (match q.q with
      (fqueue:t hd tl)
      -> (begin
           (for-list item (reverse tl)
             (emit item))
           (for-list item hd
             (emit item)))
      )))

;; note: oldest first: i.e. (oldest ... newest)
(define (queue->list q)
  (fqueue->list q.q))

(defmacro while-queue
  (while-queue item queue body ...)
  -> (while (> queue.len 0)
       (when-maybe item (queue/pop! queue)
         body ...)))
