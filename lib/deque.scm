;; -*- Mode: Irken -*-

;; persistent functional deque.
;;
;; hand-translated from ocaml 'batteries-included' library.
;;

(typealias deque {front = (list 'a) flen = int rear  = (list 'a) rlen = int })

(define (deque/empty) {front=(list:nil) flen = 0 rear = (list:nil) rlen = 0})
(define (dequeue/size q) (+ q.flen q.rlen))
(define (deque/cons x q) {front=(list:cons x q.front) flen=(+ q.flen 1) rear=q.rear rlen=q.rlen})
(define (deque/snoc x q) {front=q.front flen=q.flen rear=(list:cons x q.rear) rlen = (+ q.rlen 1)})
(define (deque/peek q)
  (match q.front with
    ()       -> (maybe:no)
    (hd . _) -> (maybe:yes hd)
    ))

;; split a list at point `n`.
(define split*
  acc 0 tl        -> (:tuple (reverse acc) tl)
  acc n (hd . tl) -> (split* (list:cons hd acc) (- n 1) tl)
  acc n ()        -> (raise (:ListIndexError))
  )

(define (deque/pop-front q)
  (match q.front q.rear with
    (hd . tl) _
    -> (maybe:yes (:tuple hd {front=tl flen=(- q.flen 1) rear=q.rear rlen=q.rlen}))
    () ()
    -> (maybe:no)
    ()         _
    -> (let ((flen0 (/ (+ q.rlen 1) 2))
             (rlen0 (/ q.rlen 2))
             ((rear front) (split* (list:nil) rlen0 q.rear))
             (front0 (reverse front)))
         (maybe:yes
          (:tuple
           (first front0)
           {front= (rest front0) flen=(- flen0 1) rear=rear rlen=rlen0})))
    ))

(define (deque/pop-rear q)
  (match q.front q.rear with
    _ (hd . rear)
    -> (maybe:yes (:tuple hd {front=q.front flen=q.flen rear=rear rlen=(- q.rlen 1)}))
    () ()
    -> (maybe:no)
    _  ()
    -> (let ((rlen0 (/ (+ q.flen 1) 2))
             (flen0 (/ q.flen 2))
             ((front rear) (split* (list:nil) flen0 q.front))
             (rear0 (reverse rear)))
         (maybe:yes
          (:tuple
           (first rear0)
           {front=front flen=flen0 rear=(rest rear0) rlen=(- rlen0 1)})))
    ))

(define (deque/rotate-forward q)
  (match (deque/pop-front q) with
    (maybe:yes (:tuple item q0)) -> (deque/snoc item q0)
    (maybe:no) -> q
    ))

(define (deque/rotate-backward q)
  (match (deque/pop-rear q) with
    (maybe:yes (:tuple item q0)) -> (deque/cons item q0)
    (maybe:no) -> q
    ))

(define (deque/peek-front q)
  (match q.front with
    (hd . _) -> (maybe:yes hd)
    ()       -> (maybe:no)
    ))

(define (deque/peek-rear q)
  (match q.rear with
    (hd . _) -> (maybe:yes hd)
    ()       -> (maybe:no)
    ))

(define (deque->list q)
  (append q.front (reverse q.rear)))

(define (deque/check q)
  (and (= (length q.front) q.flen)
       (= (length q.rear) q.rlen)))


(defmacro deque/push-front!
  (deque/push-front! q x)
  -> (let (($q0 (deque/cons x q)))
       (set! q $q0)
       ))

(defmacro deque/push-rear!
  (deque/push-rear! q x)
  -> (let (($q0 (deque/snoc x q)))
       (set! q $q0)
       ))
