;; -*- Mode: Irken -*-

(require "lib/basis.scm")

;; testing ensure-heap, which can annoyingly be called
;; in the middle of a function, requiring a GC with some
;; extra roots.

(define (get-freep)
  (%backend arm
    (%arm
     ((-> int)
      (com "fetch freep")
      (mov trg freep)
      (tag trg))))
  (%backend amd
    (%amd
     ((-> int)
      (com "fetch freep")
      (movq (rel "freep") trg)
      (tag trg)))))

(define (get-limit)
  (%backend arm
    (%arm
     ((-> int)
      (com "fetch limit")
      (ldr_addr trg "_limit")
      (tag trg))))
  (%backend amd
    (%amd
     ((-> int)
      (com "fetch limit")
      (movq (rel "limit") trg)
      (tag trg)))))

(let ((thing0 '(3 1 4 1 5 9))
      (thing1 (list:cons 99 thing0)))
  (for-range i 10000
    ;; yay, this creates 5 free registers before the make-vector call,
    ;; so a good test of nfree>0.
    (printf "i = " (int i)
            " freep = " (int (get-freep))
            " limit = " (int (get-limit))
            " vlen " (int (vector-length (make-vector 10000 2112)))
            "\n"))
  (printn thing1)
  )

