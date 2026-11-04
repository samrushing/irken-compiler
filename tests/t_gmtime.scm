(require "lib/basis.scm")
(require "lib/posix.scm")
(require "lib/time.scm")

(define days-of-the-week #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define months #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (dump-tm tm)
  (format days-of-the-week[tm.wday]
          ", " (int tm.mday)
          " " months[tm.mon]
          " " (int tm.year)
          " " (int tm.hour)
          ":" (int tm.min)
          ":" (int tm.sec)
          " " tm.zone
          ))

(let ((now (clock-gettime CLOCK_REALTIME)))
  (printn now.sec)
  (printn (gmtime now.sec))
  (printf (dump-tm (gmtime now.sec)) "\n")
  (printf (dump-tm (localtime now.sec)) "\n")
  )


    
    
