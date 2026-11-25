;; -*- Mode: Irken -*-

;; waiting for a socket, but not forever.
;;
;; (poller/wait-for-read-timeout fd msec) -> #t if <fd> became readable,
;;  #f if <msec> passed first.  likewise poller/wait-for-write-timeout.
;;
;; a timer thread sleeps for <msec>; if this wait is still pending then,
;;  it cancels it (poller/cancel-read) and wakes the waiter itself.  only
;;  one of the two can find the wait registered, so the waiter can't be
;;  woken twice, even when both happen in the same pass of the scheduler.
;;
;; the timer thread sleeps its full time even when the fd wins, so a
;;  program that's otherwise finished waits for it before exiting.
;;
;; (a general with-timeout, interrupting any wait, would subsume this.)

(require "doom/scheduler.scm")
(require "doom/timeq.scm")

(define (poller/wait-timeout wait cancel fd msec)
  (let ((done? #f)
        (timed-out? #f))
    (poller/fork
     (lambda ()
       (timeq/sleep msec)
       (when (not done?)
         (match (cancel fd) with
           (maybe:yes save)
           -> (begin
                (set! timed-out? #t)
                (poller/schedule save))
           (maybe:no) -> #u))))
    (wait fd)
    (set! done? #t)
    (not timed-out?)))

(define (poller/wait-for-read-timeout fd msec)
  (poller/wait-timeout poller/wait-for-read poller/cancel-read fd msec))

(define (poller/wait-for-write-timeout fd msec)
  (poller/wait-timeout poller/wait-for-write poller/cancel-write fd msec))
