;; -*- Mode: Irken -*-

;; many record shapes sharing labels, and a wide record.  where a label
;;  sits differs from shape to shape, so polymorphic access has to look
;;  it up.

(include "lib/basis.scm")

(define (get-k r) r.k)
(define (get-q r) r.q)
(define (set-k! r v) (set! r.k v))

(define s1 {k=1})
(define s2 {a=0 k=2})
(define s3 {k=3 z=0})
(define s4 {a=0 b=0 c=0 k=4 q=40})
(define s5 {b=0 d=0 f=0 h=0 j=0 k=5 l=0 n=0 p=0 q=50 r=0 t=0})
(define s6 {q=60 k=6})
(define s7 {aa=0 ab=0 ac=0 ad=0 ae=0 af=0 ag=0 ah=0 ai=0 aj=0 k=7 q=70})

(printn (list (get-k s1) (get-k s2) (get-k s3) (get-k s4) (get-k s5) (get-k s6) (get-k s7)))
(printn (list (get-q s4) (get-q s5) (get-q s6) (get-q s7)))
;; => (1 2 3 4 5 6 7)
;; => (40 50 60 70)

(set-k! s1 10) (set-k! s4 40) (set-k! s5 50) (set-k! s7 70)
(printn (list s1.k s4.k s5.k s7.k (get-q s5)))
;; => (10 40 50 70 50)

;; a wide record
(define w {f00=0 f01=1 f02=2 f03=3 f04=4 f05=5 f06=6 f07=7 f08=8 f09=9
           f10=10 f11=11 f12=12 f13=13 f14=14 f15=15 f16=16 f17=17 f18=18 f19=19
           f20=20 f21=21 f22=22 f23=23 f24=24 f25=25 f26=26 f27=27 f28=28 f29=29})
(printn (list w.f00 w.f07 w.f15 w.f16 w.f29))
(set! w.f16 160)
(printn (+ w.f15 w.f16 w.f17))
;; => (0 7 15 16 29)
;; => 192
