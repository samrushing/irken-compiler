;; -*- Mode: Irken -*-

(require "lib/basis.scm")

;; emulates 'setdefault' to some extent - assumes v is a mutable list.
(define (tree/collect root cmp k v-to-append)
  (match (tree/member root cmp k) with
    (maybe:yes r) -> (begin (set! r.cell (list:cons v-to-append r.cell)) root)
    (maybe:no)    -> (tree/insert root cmp k {cell=(list v-to-append)})
    ))

(define (gen-primes)
  (makegen emit
    (let loop ((D (tree/empty))
               (q 2)
               (S 4)
               (r 2))
      (match (tree/member D int-cmp q) with
        (maybe:no)
        -> (cond ((= q S)
                  (inc! S r)
                  (set! D (tree/collect D int-cmp S r))
                  (inc! r)
                  (inc! S r))
                 (else
                  (emit q)))
        (maybe:yes rec)
        -> (begin
             (for-list p rec.cell
               (set! D (tree/collect D int-cmp (+ p q) p)))
             (set! D (tree/delete D int-cmp q))
             (when (= q S)
               (inc! S r)
               (inc! r 1)
               (inc! S r))))
      (loop D (+ q 1) S r)
      )))

(let ((i 0))
  (for p (gen-primes)
    (printf (lpad 15 (int p)) "\n"))
  (inc! i)
  (when (= i 10000)
    (%exit #f 0)))


