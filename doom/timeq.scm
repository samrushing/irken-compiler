;; -*- Mode: Irken -*-

(require "lib/time.scm")

;; we maintain a priority queue of sleeping threads.

;; map from nanosecond -> thread-save
(define the-timeq (tree/empty))

;; sleep for `msec` milliseconds.
(define (timeq/sleep msec)
  (let ((k (getcc))
        (now (timespec->nanoseconds (gettime/monotonic)))
        (then (+ now (* msec 1000000))))
    (tree/insert! the-timeq int-cmp then (poller/make-save k))
    (poller/dispatch)
    ))

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

