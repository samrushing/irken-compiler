;; -*- Mode: Irken -*-

(require-ffi 'posix)

(define (clock-gettime kind)
  (let ((ts* (halloc (struct timespec))))
    (posix/clock_gettime kind ts*)
    {sec  = (%c-get-int #f (%c-sref timespec.tv_sec  ts*))
     nsec = (%c-get-int #f (%c-sref timespec.tv_nsec ts*))}
    ))

(define (timespec->nanoseconds ts)
  (+ (* ts.sec 1000000000) ts.nsec))

(define (gettime/monotonic)
  (clock-gettime CLOCK_MONOTONIC))

(define (gettime/realtime)
  (clock-gettime CLOCK_REALTIME))

(define (gettime/process)
  (clock-gettime CLOCK_PROCESS_CPUTIME_ID))

