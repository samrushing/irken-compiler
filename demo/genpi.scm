;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "demo/bignum.scm")

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

;; avoid pointless consing.
(define big/27 (big (I 27)))
(define big/12 (big (I 12)))

(define (pi)
  (define emit (pi-digit-emitter))
  (define (g q r t i)
    (let ((i3 (big (* i big/3)))
	  (u (big (* big/3 (+ i3 big/1)
                     (+ big/2 i3))))
	  (y (big (/
                   (+ (* q (- (* big/27 i) big/12))
                      (* big/5 r))
                   (* big/5 t)))))
      (emit y)
      (big
       (g (* big/10 q i (- (* big/2 i) big/1))
          (* big/10 u
             (- (+ (* q (- (* big/5 i) big/2)) r)
                (* y t)))
          (* t u)
          (+ i big/1)
          ))))
  (set-verbose-gc #f)
  (big (g big/1 (I 180) (I 60) big/2))
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
