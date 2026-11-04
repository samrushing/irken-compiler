;; -*- Mode: Irken -*-

;; big variable-sized allocations, whose only heap check is %ensure-heap.
;; if %ensure-heap doesn't collect when it should, the allocation runs
;;  past the end of the heap; the next collection then leaves that
;;  string (and anything allocated after it) in place, since it isn't
;;  in from-space, and the next one overwrites it.

(require "lib/basis.scm")

(define (fill s ch)
  (for-range i (string-length s)
    (string-set! s i ch)))

(define (check s ch)
  (for-range i (string-length s)
    (when (not (eq? (string-ref s i) ch))
      (raise (:Corrupt i)))))

;; keep the last few strings (and the conses holding them) alive.
(let loop ((i 0) (recent (list:nil)))
  (if (= i 600)
      (printf "ok\n")
      (let ((s (make-string 1000000))
            (ch (int->char (+ 97 (mod i 26)))))
        (fill s ch)
        (let ((recent (list:cons (:tuple s ch)
                                 (match recent with
                                   (a b . _) -> (list a b)
                                   _ -> recent))))
          (for-list item recent
            (match item with (:tuple s0 ch0) -> (check s0 ch0)))
          (loop (+ i 1) recent)))))
