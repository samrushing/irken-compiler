;; -*- Mode: Irken -*-

;; big variable-sized allocations (strings, vectors), checked after the
;;  collections they cause.

;; skip: b -- ~150M string-set!s: minutes in the VM

(include "lib/basis.scm")

(define (fill s ch)
  (for-range i (string-length s)
    (string-set! s i ch)))

(define (check s ch)
  (let/cc return
    (for-range i (string-length s)
      (when (not (eq? (string-ref s i) ch))
        (return #f)))
    #t))

(let loop ((i 0) (recent (list:nil)) (ok #t))
  (if (= i 300)
      (printn ok)
      (let ((s (make-string 500000))
            (ch (int->char (+ 97 (mod i 26)))))
        (fill s ch)
        (let ((recent (list:cons (:tuple s ch)
                                 (match recent with
                                   (a b . _) -> (list a b)
                                   _ -> recent))))
          (loop (+ i 1) recent
                (and ok (every? (lambda (p) (match p with (:tuple s0 c0) -> (check s0 c0))) recent)))))))
;; => #t

(define vs (map (lambda (i) (make-vector 50000 i)) (range 20)))
(printn (map (lambda (v) v[49999]) vs))
;; => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
