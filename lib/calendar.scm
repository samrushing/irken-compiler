;; -*- Mode: Irken -*-

;; translated to Irken from https://reingold.co/calendar.l

;; The following Lisp code is from ``Calendrical
;; Calculations'' by Nachum Dershowitz and Edward
;; M. Reingold, Software---Practice & Experience, vol. 20,
;; no. 9 (September, 1990), pp. 899--928 and from
;; ``Calendrical Calculations, II: Three Historical
;; Calendars'' by Edward M.  Reingold, Nachum Dershowitz,
;; and Stewart M. Clamen, Software---Practice & Experience,
;; vol. 23, no. 4 (April, 1993), pp. 383--404.

;; This code is in the public domain, but any use of it
;; should publically acknowledge its source.

(require "lib/core.scm")
(require "lib/pair.scm")

;; SMR: changes to representation:
;; gregorian date is now {year, month, day}
;; julian date is now {year, month, day}
;; iso date is now {year, week, day}
;;
;; included in this translation are only gregorian-proleptic, julian, and iso.
;; islamic, hebrew, mayan, hindu, etc.. calendars & holidays elided.

;; Sum $expression$ for $index$ = $initial$ and successive integers,
;; as long as $condition$ holds.
(defmacro sum-until
  (sum-until exp index initial condition)
  -> (let loop (($temp 0)
                (index initial))
       (if condition
           (loop (+ $temp exp) (+ 1 index))
           $temp)))

(define (last-day-of-gregorian-month month year)
  ;; Last day in Gregorian $month$ during $year$.
  (if ;; February in a leap year
   (and (= month 2)
        (= (mod year 4) 0)
        (not (member-eq? (mod year 400) (list 100 200 300))))
   29
   #(31 28 31 30 31 30 31 31 30 31 30 31)[(- month 1)]
   ))

(define (absolute-from-gregorian date)
  ;; Absolute date equivalent to the Gregorian $date$.
  (let ((month date.month)
        (year date.year))
    (+ date.day            ;; Days so far this month.
       (sum-until          ;; Days in prior months this year.
        (last-day-of-gregorian-month m year) m 1 (< m month))
       (* 365 (- year 1))  ;; Days in prior years.
       (/ (- year 1) 4)    ;; Julian leap days in prior years...
       (-                  ;; ...minus prior century years...
        (/ (- year 1) 100))
       (/                  ;; ...plus prior years divisible...
        (- year 1) 400)))) ;; ...by 400.

(define (gregorian-from-absolute date)
  ;; Gregorian (month day year) corresponding absolute $date$.
  (let ((approx (/ date 366)) ;; Approximation from below.
        (year ;; Search forward from the approximation.
         (+ approx
            (sum-until
             1 y approx
             (>= date
                 (absolute-from-gregorian
                  {month=1 day=1 year=(+ y 1)}
                  )))))
         (month ;; Search forward from January.
          (+ (sum-until
              1 m 1
              (> date
                 (absolute-from-gregorian
                  {month=m day=(last-day-of-gregorian-month m year) year=year}
                  )))
             1))
         (day ;; Calculate the day by subtraction.
          (- date
             (- (absolute-from-gregorian
                 {month=month day=1 year=year})
                1))))
    ;;  Return
    {month=month day=day year=year}
    ))

(define (Kday-on-or-before abs k)
  ;; Absolute date of the $k$day on or before $absolute$.
  ;; $k=0$ means Sunday, $k=1$ means Monday, and so on.
  (- abs (mod (- abs k) 7)))

(define (absolute-from-iso isodate)
  ;; Absolute date equivalent to ISO $date$ = (week day year).
  (let ((week isodate.week)
        (day isodate.day)
        (year isodate.year))
    (+ (Kday-on-or-before
        (absolute-from-gregorian {month=1 day=4 year=year})
        1)                       ;;  Days in prior years.
       (* 7 (- week 1))          ;;  Days in prior weeks this year.
       (- day 1))))              ;;  Prior days this week.

(define (iso-from-absolute abs)
  ;; ISO (week day year) corresponding to the absolute $date$.
  (let ((approx-g (gregorian-from-absolute (- abs 3)))
        (approx approx-g.year)
        (year (if (>= abs (absolute-from-iso {week=1 day=1 year=(+ 1 approx)}))
                  (+ 1 approx)
                  approx))
        (week (+ 1 (/ (- abs (absolute-from-iso {week=1 day=1 year=year})) 7)))
        (day (if (= 0 (mod abs 7)) 7 (mod abs 7))))
    {week=week day=day year=year}
    ))

(define (last-day-of-julian-month month year)
  ;; Last day in Julian $month$ during $year$.
  (if ;; February in a leap year
   (and (= month 2) (= (mod year 4) 0))
      29
      #(31 28 31 30 31 30 31 31 30 31 30 31)[(- month 1)]
      ))

(define (absolute-from-julian jdate)
  ;; Absolute date equivalent to Julian $jdate$.
  (let ((month jdate.month)
        (year jdate.year))
    (+ jdate.day          ;; Days so far this month.
       (sum-until         ;; Days in prior months this year.
        (last-day-of-julian-month m year) m 1 (< m month))
       (* 365 (- year 1)) ;; Days in prior years.
       (/ (- year 1) 4)   ;; Leap days in prior years.
       -2)))              ;; Days elapsed before absolute date 1.

(define (julian-from-absolute abs)
  ;; Julian (month day year) corresponding to absolute $abs$.
  (let ((approx      ;; Approximation from below.
         (/ (+ abs 2) 366))
        (year        ;; Search forward from the approximation.
         (+ approx
            (sum-until 1 y approx
                 (>= abs
                     (absolute-from-julian {month=1 day=1 year=(+ 1 y)})))))
        (month       ;; Search forward from January.
         (+ 1 (sum-until 1 m 1
                  (> abs
                     (absolute-from-julian
                      {month=m day=(last-day-of-julian-month m year) year=year}
                      )))))
        (day         ;; Calculate the day by subtraction.
         (- abs (- (absolute-from-julian {month=month day=1 year=year}) 1))))
    {month=month day=day year=year}
    ))


;; irkenish aliases
(define gregorian->absolute absolute-from-gregorian)
(define absolute->gregorian gregorian-from-absolute)
(define julian->absolute absolute-from-julian)
(define absolute->julian julian-from-absolute)
(define iso->absolute absolute-from-iso)
(define absolute->iso iso-from-absolute)

(define (gregorian->unix ts)
  (assert (>= ts.year 1970))
  (let ((abs (absolute-from-gregorian ts))
        (days-since-1970 (- abs 719163)) ;; ordinal for unix epoch 1970/1/1
        (secs-since-1970 (* days-since-1970 86400))
        (secs-in-day (+ (* ts.hour 3600) (* ts.minute 60) ts.second)))
    (+ secs-since-1970 secs-in-day)
    ))

;; (printn (absolute-from-gregorian {year=1968 month=12 day=15}))
;; (printn (gregorian-from-absolute 718781))
;; (printn (gregorian-from-absolute 693596))
;; (printn (absolute-from-iso {day=4 week=30 year=2024}))
;; (printn (gregorian-from-absolute 739092))
;; (printn (iso-from-absolute 739092))
;; (printn (absolute-from-julian {month=7 day=12 year=2024}))
;; (printn (julian-from-absolute 739092))
;; (let ((abs (absolute-from-gregorian {year=2000 month=2 day=29}))
;;       (iso (iso-from-absolute abs))
;;       (jul (julian-from-absolute abs))
;;       (grg (gregorian-from-absolute abs)))
;;   (printn abs)
;;   (printn iso)
;;   (printn jul)
;;   (printn grg)
;;   )
;; (printn "1970-01-01 = ")
;; (printn (absolute-from-gregorian {year=1970 month=1 day=1}))
;; (printn (gregorian->unix {year=2024 month=7 day=25 hour=17 minute=6 second=33}))
