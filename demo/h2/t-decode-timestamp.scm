;; -*- Mode: Irken -*-

(require "lib/basis.scm")

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

;; ISO 8601 format
(define (test s)
  (match (decode-timestamp s) with
    (maybe:yes ts)
    -> (printf
        (zpad 4 (int ts.year)) "-" (zpad 2 (int ts.month)) "-" (zpad 2 (int ts.day))
        "T" (zpad 2 (int ts.hour)) ":" (zpad 2 (int ts.minute)) ":" (zpad 2 (int ts.second))
        "Z\n")
    (maybe:no)
    -> (printf "NO.\n")
    ))

(test "Fri, 17 Oct 2008 22:32:5 GMT")
(test "Fri, 17 Oct 2008 22:32:ab GMT")
(test "Fri, 17 Blorb 2008 22:32:05 GMT")
(test "Fri, 17 Oct 20x8 22:32:05 GMT")
(test "Fri, 1x Oct 2008 22:32:05 GMT")
(test "Fri, 17 Oct 2008 22:32:5 UTC")

