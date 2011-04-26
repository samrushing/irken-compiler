;; -*- Mode: Irken -*-

;; based on ocaml/stdlib/queue.ml

(define (queue/make)
  {length = 0 tail = (maybe:no)}
  )

(define (queue/add q x)
  (set! q.length (+ 1 q.length))
  (match q.tail with
    (maybe:no)
    -> (let ((cell {content=x next=(magic #u)}))
	 (set! cell.next cell)
	 (set! q.tail (maybe:yes cell)))
    (maybe:yes tail)
    -> (let ((head tail.next)
	     (cell {content=x next=head}))
	 (set! tail.next cell)
	 (set! q.tail (maybe:yes cell)))))

;; ocaml raises an exception rather than using maybe
(define (queue/peek q)
  (match q.tail with
    (maybe:no) -> (maybe:no)
    (maybe:yes {content=x next=_}) -> (maybe:yes x)))

(define (queue/iterate p q)
  (match q.tail with
    (maybe:no) -> #u
    (maybe:yes tail)
    -> (let loop ((node tail.next))
	 (p node.content)
	 (if (eq? node tail)
	     #u
	     (loop node.next)))
    ))

(define (queue/pop q)
  (match q.tail with
    (maybe:no) -> (maybe:no)
    (maybe:yes tail)
    -> (let ((head tail.next))
	 (set! q.length (- q.length 1))
	 (if (= q.length 0)
	     (set! q.tail (maybe:no))
	     (set! tail.next head.next))
	 (maybe:yes head.content))))
