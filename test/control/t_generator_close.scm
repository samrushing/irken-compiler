;; -*- Mode: Irken -*-

;; try/finally, closable generators, and for-closing.

(include "lib/basis.scm")

(define log '())
(define (note s) (push! log s))
(define show-maybe
  (maybe:yes n) -> (printf "yes " (int n) "\n")
  (maybe:no)    -> (printf "no\n"))

(define (show-log)
  (printf (join " " (reverse log)) "\n")
  (set! log '())
  #u)

;; --- try/finally ---

;; returns its value; cleanup runs once.
(printn (try (begin (note "body") 42) finally (note "cleanup")))
(show-log)
;; => 42
;; => body cleanup

;; an exception goes on, after the cleanup.
(printn (try
         (try (begin (note "body") (raise (:Oops 1)) 0) finally (note "cleanup"))
         except (:Oops n) -> (begin (note "caught") n)))
(show-log)
;; => 1
;; => body cleanup caught

;; nested: inner cleanup first.
(try
 (try (note "body") finally (note "inner"))
 finally (note "outer"))
(show-log)
;; => body inner outer

;; --- a producer that records its own cleanup ---

(define (counter n)
  (closable-gen emit
    (try
     (for-range i n
       (emit i))
     finally
     (note "producer cleanup"))))

;; to the end: the cleanup runs as the producer finishes.
(for-closing x (counter 3) (note (int->string x)))
(show-log)
;; => 0 1 2 producer cleanup

;; break: close resumes the producer, whose cleanup runs.
(for-closing x (counter 100)
  (note (int->string x))
  (when (= x 2) (break)))
(show-log)
;; => 0 1 2 producer cleanup

;; the body raises: the producer is closed, then the exception goes on.
(printn
 (try
  (begin
    (for-closing x (counter 100)
      (note (int->string x))
      (when (= x 1) (raise (:Oops 99))))
    0)
  except (:Oops n) -> n))
(show-log)
;; => 99
;; => 0 1 producer cleanup

;; the producer raises: its own cleanup runs, the exception reaches the
;;  loop's caller, and close doesn't resume it.
(define (bad)
  (closable-gen emit
    (try
     (begin (emit 1) (raise (:Oops 7)) (emit 2))
     finally (note "bad cleanup"))))

(printn
 (try
  (begin (for-closing x (bad) (note (int->string x))) 0)
  except (:Oops n) -> n))
(show-log)
;; => 7
;; => 1 bad cleanup

;; closing a generator that never started, or twice, does nothing.
(let ((g (counter 5)))
  (g.close)
  (g.close)
  (show-maybe (g.next)))
(show-log)
;; => no
;; =>

;; by hand: next, then close.
(let ((g (counter 5)))
  (show-maybe (g.next))
  (show-maybe (g.next))
  (g.close)
  (show-maybe (g.next)))
(show-log)
;; => yes 0
;; => yes 1
;; => no
;; => producer cleanup

;; a producer that catches the close and emits again.
(define (stubborn)
  (closable-gen emit
    (try
     (emit 1)
     except (:Generator/Close) -> (emit 2))))

(printn
 (try
  (begin (for-closing x (stubborn) (break)) "closed")
  except (:Generator/IgnoredClose) -> "ignored"))
;; => "ignored"

;; a producer that catches the close and just stops: fine.
(define (polite)
  (closable-gen emit
    (try
     (begin (emit 1) (emit 2))
     except (:Generator/Close) -> (note "stopping"))))

(for-closing x (polite) (break))
(show-log)
;; => stopping

;; a plain generator works too.
(for-closing x (gen->closable (list-generator '(a b c)))
  (note (symbol->string x)))
(show-log)
;; => a b c

;; for-closing over for-closing: each closes its own.
(for-closing x (counter 3)
  (for-closing y (counter 3)
    (when (= y 1) (break)))
  (when (= x 1) (break)))
(show-log)
;; => producer cleanup producer cleanup producer cleanup
