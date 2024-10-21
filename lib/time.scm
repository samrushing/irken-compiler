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

(define (tzset)
  (posix/tzset))

(define (unpack-struct-tm tm*)
  { ;;
   sec    = (c-get-int (%c-sref tm.tm_sec tm*))
   min    = (c-get-int (%c-sref tm.tm_min tm*))
   hour   = (c-get-int (%c-sref tm.tm_hour tm*))
   mday   = (c-get-int (%c-sref tm.tm_mday tm*))
   mon    = (c-get-int (%c-sref tm.tm_mon tm*))
   year   = (+ 1900 (c-get-int (%c-sref tm.tm_year tm*)))
   wday   = (c-get-int (%c-sref tm.tm_wday tm*))
   yday   = (c-get-int (%c-sref tm.tm_yday tm*))
   isdst  = (c-get-int (%c-sref tm.tm_isdst tm*))
   zone   = (get-cstring* (%c-sref tm.tm_zone tm*))
   gmtoff = (c-get-int (%c-sref tm.tm_gmtoff tm*))
   }
  )

(define (gmtime time_t)
  (let ((time_t* (halloc long))
	(tm_t* (halloc (struct tm))))
    (%c-set-int long time_t* time_t)
    (syscall-null (posix/gmtime_r time_t* tm_t*))
    (unpack-struct-tm tm_t*)))

(define (localtime time_t)
  (let ((time_t* (halloc long))
	(tm_t* (halloc (struct tm))))
    (%c-set-int long time_t* time_t)
    (syscall-null (posix/localtime_r time_t* tm_t*))
    (unpack-struct-tm tm_t*)))

