;; -*- Mode: Irken -*-

(require "lib/time.scm")

;; we maintain a priority queue of sleeping threads.

;; map from nanosecond -> thread-save
(define the-timeq (tree/empty))

;; sleep for `msec` milliseconds.
(define (timeq/sleep msec)
  (let ((k (getcc))
        (now (timespec->nanoseconds (gettime/monotonic)))
        (then (timeq/free-slot (+ now (* msec 1000000)))))
    (tree/insert! the-timeq int-cmp then (poller/make-save k))
    (poller/dispatch)
    ))

;; the tree keeps one entry per key: a second thread sleeping until
;;  the same nanosecond would never wake.  so take the next free one.
(define (timeq/free-slot then)
  (match (tree/member the-timeq int-cmp then) with
    (maybe:yes _) -> (timeq/free-slot (+ then 1))
    (maybe:no)    -> then))

;; return the time to the next event, in nanoseconds.
(define (timeq/time-to-next)
  (if (eq? the-timeq (tree:empty))
      (* 10 1000000000)
      (let ((now (timespec->nanoseconds (gettime/monotonic)))
            ((then _) (tree/min the-timeq))
            (wait (- then now)))
        (max wait 0))))

;; schedule any thread whose time has come.
(define (timeq/schedule)
  (let ((now (timespec->nanoseconds (gettime/monotonic))))
    (let/cc break
      (for-map when save the-timeq
        ;; (debugf "timeq/schedule " (int when) "\n")
        (cond ((> when now) (break #u))
              (else
               ;; note: the-timeq is a persistent map,
               ;;  so this is safe.
               (tree/pop-min! the-timeq int-cmp)
               (poller/schedule save)))))))

