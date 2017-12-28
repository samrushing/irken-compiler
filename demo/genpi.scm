;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "demo/bignum.scm")

;; This demo shows off the bignum library, but also irken's ability to do
;;  world dump/load.  Every 100 digits it saves the state of the computation
;;  to disk, where it can be restarted mid-computation.

;; Note: to use dump/load, you need to link with -no_pie to disable ASLR
;;
;; on macOS: add -Wl,-no_pie to the clang compilation args.
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
  (define emit (pi-digit-emitter))
  (define (g q r t i)
    (let ((i3 (big (* i (I 3))))
	  (u (big (* (I 3) (+ i3 (I 1))
                     (+ (I 2) i3))))
	  (y (big (/
                   (+ (* q (- (* (I 27) i) (I 12)))
                      (* (I 5) r))
                   (* (I 5) t)))))
      (emit y)
      (big
       (g (* (I 10) q i (- (* (I 2) i) (I 1)))
          (* (I 10) u
             (- (+ (* q (- (* (I 5) i) (I 2))) r)
                (* y t)))
          (* t u)
          (+ i (I 1))
          ))))
  (set-verbose-gc #f)
  (big (g (I 1) (I 180) (I 60) (I 2)))
  )

(printf "Note: if this crashes upon restart you probably need to disable ASLR.\n")

;; invoke without an argument to start generating,
;; and with an argument to load it and run it.

(if (> sys.argc 1)
    (begin
      ;; note: verbose GC is on by default, so we have to turn it off again when restarting.
      (set-verbose-gc #f)
      (throw (load "pi.image") 0))
    (pi))
