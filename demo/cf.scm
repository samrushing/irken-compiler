;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "demo/bignum.scm")

(define (ratio->cf a b)
  (makegen emit
    (let loop ((a a) (b b))
      (when (>0 b)
        (let (((q r) (divmod a b)))
          (emit q)
          (loop b r))))))

(define (cf->ratio cf n)
  (define loop
    a b ()        -> (:tuple b a)
    a b (hd . tl) -> (loop b (+ (* b hd) a) tl)
    )
  (loop 0 1 (reverse (gen-take n cf)))
  )

(define (cf->big-ratio cf n)
  (define loop
    a b ()        -> (:tuple b a)
    a b (hd . tl) -> (loop b (big (+ (* b (int->big hd)) a)) tl)
    )
  (loop big/0 big/1 (reverse (gen-take n cf)))
  )

(define (phi)
  (makegen emit
    (forever (emit 1))))

(define (sqrt-5)
  (makegen emit
    (emit 2)
    (forever (emit 4))))

(define (e)
  (makegen emit
    (emit 2) (emit 1)
    (let loop ((n 2))
      (emit n) (emit 1) (emit 1)
      (loop (+ n 2)))))

(define (t0)
  (printn (gen-take 8 (ratio->cf 415 93)))
  (printn (cf->ratio (ratio->cf 415 93) 8))
  (printn (cf->ratio (phi) 30))
  (let (((a b) (cf->big-ratio (phi) 100)))
    (printf (big->dec a) "\n")
    (printf (big->dec b) "\n")
    )
  (let (((a b) (cf->big-ratio (sqrt-5) 100)))
    (printf (big->dec a) "\n")
    (printf (big->dec b) "\n")
    )
  (let (((a b) (cf->big-ratio (e) 100)))
    (printf (big->dec a) "\n")
    (printf (big->dec b) "\n")
    ))

;; translation of Marcin Ciura's cf.py::digits() function.
;; converts a CF to an arbitrary base.
(define (cf->big-digits x base)

  (define mb=?
    (maybe:yes x) (maybe:yes y) -> (big= x y)
    _ _                         -> #f
    )

  (define (nonzero? n) (not (big-zero? n)))

  (let ((odigits #f)
        (ac (maybe:no))
        (bd (maybe:no))
        )
    (makegen emit
      (let/cc return
        (let loop ((a big/1)
                   (b big/0)
                   (c big/0)
                   (d big/1))
          (when (or (nonzero? a) (nonzero? b))
            ;; compute a/c & b/d (if they exist)
            (cond ((nonzero? c)
                   (set! ac (maybe:yes (big (/ a c))))
                   (cond ((nonzero? d)
                          (set! bd (maybe:yes (big (/ b d)))))
                         ((nonzero? b)
                          (set! bd (maybe:no)))
                         (else (set! bd ac))))
                  ((nonzero? d)
                   (cond ((nonzero? a)
                          (set! ac (maybe:no))
                          (set! bd (maybe:yes big/0)))
                         (else
                          (set! bd (maybe:yes (big (/ b d))))
                          (set! ac bd))))
                  (else
                   (return #u)))
            ;; maybe emit a digit...
            (cond ((mb=? ac bd)
                   (match ac bd with
                     (maybe:yes ac0) (maybe:yes bd0)
                     -> (begin
                          (emit ac0)
                          (set! odigits #t)
                          (loop (big (* base (- a (* c ac0))))
                                (big (* base (- b (* d bd0))))
                                c
                                d))
                     _ _ -> #u))
                  (else
                   (set! ac (x)) ;; get the next term
                   (match ac with
                     (maybe:yes ac0)
                     -> (loop (big (+ b (* a ac0)))
                              a
                              (big (+ d (* c ac0)))
                              c)
                     (maybe:no)
                     -> (loop a a c c)))))))
      (when (not odigits)
        (emit big/0)))))

;; (gen int) -> (gen big)
(define (embiggen g)
  (makegen emit
    (for item g
      (emit (int->big item)))))

(define (digits-10k gen)
  ;; take 2000 'digits' (in base 100,000)
  (for-list term (gen-take
                  2000
                  (cf->big-digits
                   (embiggen gen)
                   (int->big 100000)))
    (printf (zpad 5 (big->dec term)) " "))
  (printf "\n"))

;;(t0)
(digits-10k (sqrt-5))

