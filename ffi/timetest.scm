;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/ctype.scm")
(include "lib/map.scm")
(include "lib/lisp_reader.scm")

(require-ffi 'posix)

(%backend bytecode (update-sizeoff-table))

;; note: timezone.tz_dsttime is not really useful, since the DST rules
;;  are complex and variable.

(define (gettimeofday)
  (let ((tv (malloc (struct timeval)))
        (tz (malloc (struct timezone))))
    (posix/gettimeofday tv tz)
    (let ((result
           {sec         = (%c-get-int i64 (%c-sref timeval.tv_sec tv))
            usec        = (%c-get-int i32 (%c-sref timeval.tv_usec tv))
            minuteswest = (%c-get-int i32 (%c-sref timezone.tz_minuteswest tz))
            dsttime     = (%c-get-int i32 (%c-sref timezone.tz_dsttime tz))}))
      (free tv)
      (free tz)
      result)))

(define (get-c-string ref)
  (%cref->string #f ref (posix/strlen ref)))

(define (ctime)
  (let ((t0 (gettimeofday))
        (t1* (malloc long)))
    (%c-set-int long t0.sec t1*)
    (let ((r (get-c-string (posix/ctime t1*))))
      (free t1*))))

;; https://stackoverflow.com/questions/7136385/calculate-day-number-from-an-unix-timestamp-in-a-math-way
;; http://howardhinnant.github.io/date_algorithms.html#civil_from_days
;; note: this uses a year starting on march 1st in order to simplify the
;;   math. hence the +3/-9 adjustment to 'm'.

(define (civil-from-timestamp s)
  (let ((z   (+ (/ s 86400) 719468))
        (era (/ (if (>= z 0) z (- z 146096)) 146097))
        (doe (- z (* era 146097)))
        (yoe (/ (- doe (+ (/ doe 1460) (/ doe 36524)) (/ doe 146096)) 365))
        (y   (+ yoe (* era 400)))
        (doy (- doe (- (+ (* 365 yoe) (/ yoe 4)) (/ yoe 100))))
        (mp  (/ (+ (* 5 doy) 2) 153))
        (d   (+ (- doy (/ (+ (* 153 mp) 2) 5)) 1))
        (m   (+ mp (if (< mp 10) 3 -9)))
        (y1  (+ y (if (<= m 2) 1 0)))
        (s0  (mod s 86400))
        (ss0 (/ s0 60))
        (ss1 (- s0 (* ss0 60)))
        (hh  (/ ss0 60))
        (mm  (- ss0 (* hh 60))))
    {y=y1 m=m d=d hh=hh mm=mm ss=ss1}
    ))

(define (civil gmt?)
  (let ((t0 (gettimeofday)))
    (civil-from-timestamp
     (if gmt?
         t0.sec
         (- t0.sec (* t0.minuteswest 60))))))

(define (format-civil c)
  (format (int c.y) "/" (int c.m) "/" (int c.d) " "
          (zpad 2 (int c.hh)) ":"
          (zpad 2 (int c.mm)) ":"
          (zpad 2 (int c.ss))))

(printf (format-civil (civil #t)) "\n")
(printf (format-civil (civil #f)) "\n")

;(let ((c (civil-from-timestamp (string->int sys.argv[1]))))
;  (printf (int c.y) "/" (int c.m) "/" (int c.d) "\n"))



