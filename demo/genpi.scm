;; -*- Mode: Irken -*-

(include "demo/bignum.scm")

;; This demo shows off the bignum library, but also irken's ability to do
;;  world dump/load.  Every 100 digits it saves the state of the computation
;;  to disk, where it can be restarted mid-computation.

;; Note: to use dump/load, you need to link with -no_pie to disable ASLR
;;
;; on OS X: add -Wl,-no_pie to the clang compilation args.
;;
(define (pi-digit-emitter)
  (let ((n 1))
    (printf (lpad 6 (int 0)) ":  ")
    (define (emit dig)
      (when (= 0 (mod n 100))
	    (callcc (lambda (k) (dump "pi.image" k)))
	    (printf "\n" (lpad 6 (int n)) ": ")
	    (flush)
	    #u
	    )
      (set! n (+ n 1))
      (printf (big->dec dig))
      (flush)
      )
    emit
    ))

;; http://damien-guichard.developpez.com/tutoriels/ocaml/?page=page_6
;; http://www.pi314.net/eng/schrogosper.php

(define (pi)
  (define B1 (int->big 1))
  (define B2 (int->big 2))
  (define B3 (int->big 3))
  (define B* big-mul)
  (define B+ big-add)
  (define B- big-sub)
  (define B/ big-div)
  (define IB int->big)
  (define emit (pi-digit-emitter))

  (define (B/ a b)
    (match (big-div a b) with
      (:tuple quo rem)
      -> quo))

  (define (g q r t i)
    (let ((i3 (B* i B3))
	  (u (B* B3 (B* (B+ i3 B1) (B+ B2 i3))))
	  (y (B/
	      (B+ (B* q (B- (B* (IB 27) i) (IB 12)))
		  (B* (IB 5) r))
	      (B* (IB 5) t)))
	  )
      (emit y)
      (g (B* (IB 10) (B* q (B* i (B- (B* B2 i) B1))))
	 (B* (IB 10)
	     (B* u
		 (B- (B+ (B* q (B- (B* (IB 5) i) B2)) r)
		     (B* y t))))
	 (B* t u)
	 (B+ i B1)
	 )
      )
    )
  (set-verbose-gc #f)
  (g B1 (IB 180) (IB 60) B2)
  )

(printf "Note: if this crashes upon restart you probably need to disable ASLR.")

;; invoke without an argument to start generating,
;; and with an argument to load it and run it.

(if (> sys.argc 1)
    (begin
      ;; note: verbose GC is on by default, so we have to turn it off again when restarting.
      (set-verbose-gc #f)
      (throw (load "pi.image") 0))
    (pi))
