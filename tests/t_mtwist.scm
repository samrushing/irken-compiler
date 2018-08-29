;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/mtwist.scm")

(define expect0
  #(387812712 1730152793 2677971018 3648273174 1706625531
              154404169 3581438677 3127537881 3612611189 4080258349
              1068173871 4059832728 910447138 53247496 3404917499
              4098258988 3890827954 3126294650 1860735035 181389107))

(define expect1
  #(1763050459 3305519191 676608084 2915967477 2557916070 2448919081
               2173737063 2152835795 1060322924 947743517 1644064974 337881847
               1104283350 3804270769 3540304986 1795863450 3766365899 3565204128
               620496131 3556564351))

;; check that we get the correct values.
(let ((R0 (mt19937 3141)))
  (for-range i 20
    (assert (= (R0) expect0[i])))
  (for-range i 1000000
    (R0))
  (for-range i 20
    (assert (= (R0) expect1[i])))
  )

(for x (mt19937-generator 5489 20)
  (printf (zpad 8 (hex x)) "\n"))

;; verify that generate-random-bits creates an identical sequence
;;   of bits no matter what size pieces we chop it into.
(define (test-grb)
  (let ((s0 '())
        (s1 '())
        (s2 '()))
    ;; 7200 bits total
    (let ((gen32 (generate-random-bits 32 3141)))
      (for-range i 225
        (when-maybe x (gen32)
          (push! s0 (format (zpad 8 (hex x)))))))
    (let ((gen36 (generate-random-bits 36 3141)))
      (for-range i 200
        (when-maybe x (gen36)
          (push! s1 (format (zpad 9 (hex x)))))))
    (let ((gen24 (generate-random-bits 24 3141)))
      (for-range i 300
        (when-maybe x (gen24)
          (push! s2 (format (zpad 6 (hex x)))))))
    (let ((j0 (string-concat (reverse s0)))
          (j1 (string-concat (reverse s1)))
          (j2 (string-concat (reverse s2))))
      (assert (string=? j0 j1))
      (assert (string=? j1 j2))
      )))

(test-grb)
