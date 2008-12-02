
;; make sure the inliner doesn't go all infinite on us

(define (dee x)
  (%+ 1 (dum x)))

(define (dum x)
  (%- 1 (dee x)))

(dee 10)
