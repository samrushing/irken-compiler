;; -*- Mode: Irken -*-

(require "lib/calendar.scm")

;; timestamp-related code.

(define lookup-month
  (let ((months #("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")))
    (lambda (month)
      (let loop ((i 0)
                 (m (downcase month)))
        (if (= i 12)
            (maybe:no)
            (if (string=? months[i] m)
                (maybe:yes (+ i 1))
                (loop (+ i 1) m)))))
    ))

;;  this is not really needed since we are using
;;  weak etags rather than if-modified-since/last-modified.

(define (decode-timestamp s)
  ;; example if-modified-since from chrome.
  ;; Fri, 17 Oct 2008 22:32:5 GMT
  (let ((parts (string-split s #\space)))
    (match parts with
      (dow day mon year hms "GMT")
      -> (match (lookup-month mon) with
           (maybe:yes mon0)
           -> (try
               (let ((day0 (string->int day))
                     (year0 (string->int year)))
                 (match (map string->int (string-split hms #\:)) with
                   (hh mm ss)
                   -> (maybe:yes {year=year0 month=mon0 day=day0 hour=hh minute=mm second=ss})
                   _
                   -> (maybe:no)
                   ))
               except (:String/BadInt _)
               -> (maybe:no)
               )
           (maybe:no)
           -> (maybe:no)
           )
      _
      -> (maybe:no)
      )))


;; --------------------------------------------------------------------------------
;; used in file listings, logs, etc.
;; --------------------------------------------------------------------------------

(define days-of-the-week #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define months #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;; this is in last-modified header format.
(define (format-timestamp time_t)
  (let ((tm (gmtime time_t)))
    (format days-of-the-week[tm.wday]
            ", " (int tm.mday)
            " " months[tm.mon]
            " " (int tm.year)
            " " (zpad 2 (int tm.hour))
            ":" (zpad 2 (int tm.min))
            ":" (zpad 2 (int tm.sec))
            " GMT" ;; UTC (as returned by gmtime) does not work with http.
            )))

(define (format-timestamp-8601 ts)
  (format (zpad 4 (int ts.year)) "-" (zpad 2 (int ts.month)) "-" (zpad 2 (int ts.day))
          "T" (zpad 2 (int ts.hour)) ":" (zpad 2 (int ts.minute)) ":" (zpad 2 (int ts.second))
          "Z\n"))

