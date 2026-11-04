;; -*- Mode: Irken -*-

(require "lib/basis.scm")

;; See from Daan Leijen, "Division and Modulus for Computer Scientists", Section 1.3
;; https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf

(define tests
  (list (:tuple  8  3)
        (:tuple  8 -3)
        (:tuple -8  3)
        (:tuple -8 -3)
        (:tuple  1  2)
        (:tuple  1 -2)
        (:tuple -1  2)
        (:tuple -1 -2)
        ))

(define (test-one t)
  (let (((n d) t)
        ((qt rt) (divmod-truncated n d))
        ((qf rf) (divmod-floored n d))
        ((qe re) (divmod-euclidean n d)))
    (printf (lpad 3 (int n))
            (lpad 3 (int d))
            (lpad 3 (int qt))
            (lpad 3 (int rt))
            (lpad 3 (int qf))
            (lpad 3 (int rf))
            (lpad 3 (int qe))
            (lpad 3 (int re))
            "\n")))

(printf "  n  d TRUNC FLOOR EUCLID\n")
(for-list test tests
  (test-one test)
  )
