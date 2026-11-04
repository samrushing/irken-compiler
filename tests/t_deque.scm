;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/deque.scm")
(require "lib/mtwist.scm")

(let ((q0 (deque/empty))
      (q1 (deque/cons 1 q0))
      (q2 (deque/snoc 9 q1)))
  (printn q0)
  (printn q1)
  (printn q2)
  (printn (deque/pop-front q2))
  )

(let ((q (deque/empty))
      (alt #f))
  (for-range i 20
    (if alt
        (set! q (deque/cons i q))
        (set! q (deque/snoc i q)))
    (assert (deque/check q))
    (set! alt (not alt)))
  (printf "result: " (join int->string " " (deque->list q)) "\n")
  (match (split* (list:nil) 10 (deque->list q)) with
    (:tuple front rear)
    -> (begin (printf "split* front " (join int->string " " front) "\n")
              (printf "split*  rear " (join int->string " " rear)  "\n")))
  (let ((q0 q)
        (r (list:nil)))
    (for-range i 20
      (match (deque/pop-front q0) with
        (maybe:yes (:tuple hd q1)) -> (begin (push! r hd) (set! q0 q1))
        (maybe:no) -> (raise (:Oops))
        )
      (deque/check q0))
    (printf "result: " (join int->string " " (reverse r)) "\n")
    )
  )


(define sim/cons list:cons)
(define (sim/snoc x l)
  (reverse (list:cons x (reverse l))))
(define sim/pop-front
  (hd . tl) -> (maybe:yes (:tuple hd tl))
  () -> (maybe:no)
  )
(define sim/pop-rear
  () -> (maybe:no)
  l -> (let ((rl (reverse l)))
         (maybe:yes (:tuple (first rl) (reverse (rest rl)))))
  )

(defmacro qop!
  (qop! op q)
  -> (match (op q) with
       (maybe:yes (:tuple item q0)) -> (set! q q0)
       (maybe:no) -> #u
       )
  )


(define (random-test)
  (let ((q (deque/empty))
        (s (list:nil))
        (rng (generate-random-bits 2 314159265)))
    ;; a little fill
    (for-range i 100
      (set! q (deque/cons i q))
      (set! s (sim/cons i s)))
    ;; random operations
    (for-range j 10000
      (let ((op (probably (rng))))
        (match op with
          0 -> (begin
                 (set! q (deque/cons j q))
                 (set! s (sim/cons j s)))
          1 -> (begin
                 (set! q (deque/snoc j q))
                 (set! s (sim/snoc j s)))
          2 -> (begin
                 (qop! deque/pop-front q)
                 (qop! sim/pop-front s))
          3 -> (begin
                 (qop! deque/pop-rear q)
                 (qop! sim/pop-rear s))
          _ -> (raise (:SayWhat?))
          )
        ;; (printf "q " (join int->string " " (deque->list q)) "\n")
        ;; (printn q)
        ;; (printf "[" (int op) "]")
        (assert (deque/check q))
        (assert (magic=? (deque->list q) s))
        ))
    (printf "q " (join int->string " " (deque->list q)) "\n")
    (printf "s " (join int->string " " s) "\n")
    ))

(random-test)
