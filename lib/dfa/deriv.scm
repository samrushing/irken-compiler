;; -*- Mode: Irken -*-

(require "lib/dfa/rx.scm")

;; "Regular-expression derivatives reexamined"
;; https://www.mpi-sws.org/~turon/re-deriv.pdf

;; nullable? rx => rx
;; rather than answer #t/#f, this answers
;;  with ε (#t) or ∅ (#f) where ∅ is represented by the empty charset symbol.

(define n?
  (rx:eps)	-> (rx:eps)
  (rx:sym _)	-> rx-null
  (rx:star r)	-> (rx:eps)
  (rx:cat r s)
  -> (match (n? r) (n? s) with
       (rx:eps) (rx:eps) -> (rx:eps)
       _  _              -> rx-null)
  (rx:or r s)
  -> (match (n? r) (n? s) with
       (rx:eps) _ -> (rx:eps)
       _ (rx:eps) -> (rx:eps)
       _ _        -> rx-null)
  (rx:and r s)
  -> (match (n? r) (n? s) with
       (rx:eps) (rx:eps) -> (rx:eps)
       _  _              -> rx-null)
  (rx:not r)
  -> (match (n? r) with
       (rx:sym ()) -> (rx:eps)
       _           -> rx-null)
  (rx:group _ _ r)
  -> (n? r)
  )

(define (deriv a p r)

  (define (D-group n m r)
    (let ((dr (D r)))
      (if (eq? m.ms (mstate:before))
          (set! m {s=p e=0 ms=(mstate:start)}))
      (if (and (not (eq? m.ms (mstate:after)))
               (eq? (n? dr) (rx:eps))) ;; submatch is final
          (rx-group n {s=m.s e=p ms=(mstate:final)} dr)
          (rx-group n m dr))))

  (define D
    (rx:eps)     -> rx-null
    (rx:sym ())  -> rx-null
    (rx:sym b)   -> (if (charset/overlap? a b) (rx:eps) rx-null)

    ;; push submatch data rightward
    (rx:cat (rx:group n m r) s)
    -> (rx-or (rx-cat (D-group n m r) s)
              (rx-cat (n? r) (D-group n {s=m.s e=m.e ms=(mstate:after)} s)))

    (rx:cat r s) -> (rx-or (rx-cat (D r) s) (rx-cat (n? r) (D s)))
    (rx:star r)  -> (rx-cat (D r) (rx:star r))
    (rx:or r s)  -> (rx-or (D r) (D s))
    (rx:and r s) -> (rx-and (D r) (D s))
    (rx:not r)   -> (rx-not (D r))

    (rx:group n m r) -> (D-group n m r)
    )

  (D r)

  )

(define (find-matches d)
  (let ((ms '()))
    (define W
      (rx:cat r s) -> (begin (W r) (W s))
      (rx:or r s)  -> (begin (W r) (W s))
      (rx:and r s) -> (begin (W r) (W s))
      (rx:star r)  -> (W r)
      (rx:not r)   -> (W r)
      (rx:group n {s=_  e=_ ms=(mstate:before)} r) -> (W r)
      (rx:group n {s=_  e=_ ms=(mstate:start)} r) -> (W r)
      (rx:group n m r) -> (begin (push! ms (:tuple n m.s m.e)) (W r))
      _ -> #u
      )
    (W d)
    ms))

(define (print-matches txt ms)
  (printf "'" txt "'\n")
  (for-list m ms
    (match m with
      (:tuple n s e)
      -> (printf " " (repeat s " ") (repeat (+ 1 (- e s)) (int->string n)) "\n")
      )
    ))

;; match a string directly using derivatives.
(define (deriv-match rx s)
  (let loop ((i 0)
	     (rx0 rx))
    (let ((nullable (n? rx0)))
      (printf "deriv " (pp-rx rx0) "\n")
      (printf "   n? " (pp-rx nullable) "\n")
      (if (eq? nullable (rx:eps))
          (print-matches s (find-matches rx0)))
      (if (= i (string-length s))
          (eq? (n? rx0) (rx:eps))
          (let ((ch (string-ref s i)))
            (printf "------------\n")
            (printf "   ch " (char ch) "\n")
            (loop (+ i 1)
                  (deriv (charset/single
                          (char->ascii ch))
                         i
                         rx0))))
      )))
