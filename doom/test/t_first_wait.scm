;; -*- Mode: Irken -*-

;; the first wait with a timeout, in the first thread main forks.
;;
;; main forks one thread and calls poller/wait-and-schedule.  the thread
;;  (after forking the echo thread) waits for a UDP reply with poller/wait-for-read-timeout, which forks
;;  its timer thread; the timer thread's dispatch resumes main, leaving
;;  the waiter runnable.  the scheduler used to block in kevent/epoll
;;  until the timer, with the waiter still in the run queue, so the
;;  first wait always timed out.  (doom/resolver's first query did.)
;;
;; a second thread replies after 80ms; each wait should take about that,
;;  and the whole run well under a second.  (with the old scheduler this
;;  arrangement stalled for the poller's 10s idle timeout instead.)

(require "lib/basis.scm")
(require "doom/doom.scm")
(require "doom/timeout.scm")

(define PORT 47331)   ;; the echo end
(define PORT2 47332)  ;; the client end (UDP, connected both ways)
(define NROUNDS 3)
(define failures 0)
(define t-start 0)

(define (now-ms)
  (/ (timespec->nanoseconds (gettime/monotonic)) 1000000))

;; read a datagram, wait 80ms, send it back.
(define (echo)
  (let ((s (doom/make (udp4-sock) 4096 1024)))
    (s.bind (address/make4 "127.0.0.1" PORT))
    (s.connect (address/make4 "127.0.0.1" PORT2))
    (let loop ((n 0))
      (when (< n NROUNDS)
        (let ((data (s.recv)))
          (timeq/sleep 80)
          (s.send data)
          (loop (+ n 1)))))))

(define (client)
  (poller/fork echo)
  (for-range i NROUNDS
    (let ((s (doom/make (udp4-sock) 4096 1024))
          (t0 (now-ms)))
      (s.bind (address/make4 "127.0.0.1" PORT2))
      (s.connect (address/make4 "127.0.0.1" PORT))
      (s.send "ping")
      (let ((ok (poller/wait-for-read-timeout (s.get-fd) 2000))
            (ms (- (now-ms) t0)))
        (cond ((and ok (< ms 1000))
               (printf "ok: wait " (int i) " answered\n"))
              (else
               (set! failures (+ failures 1))
               (printf "FAIL: wait " (int i) " ok " (bool ok) " after " (int ms) "ms\n"))))
      (s.close)))
  (let ((total (- (now-ms) t-start)))
    (when (> total 1500)
      (set! failures (+ failures 1))
      (printf "FAIL: the run took " (int total) "ms\n")))
  (printf (int failures) " failed\n")
  (%exit #f (if (= failures 0) 0 1)))

(define (main)
  (set! t-start (now-ms))
  (poller/fork client)
  (poller/wait-and-schedule))

(main)
