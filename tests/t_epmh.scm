;; -*- Mode: Irken -*-

;; easy perfect minimal hash
;; http://stevehanov.ca/blog/index.php?id=119

(include "lib/basis.scm")
(include "lib/map.scm")

(define hash
  ()       d -> d
  (k . tl) d -> (hash tl (logand #xffffffff (logxor k (* d #x01000193)))))

;; unrolled - our keys are always of length 2
(define (hash2 d k0 k1)
  (logand #xffffffff
    (logxor k1
      (* (logand #xffffffff
            (logxor k0 (* d #x01000193)))
         #x01000193))))

;; format: #(k0 k1 v) - the key is a pair of integers mapping to an integer.
(define input-table
  (literal
   #(#(2 6 0) #(5 6 1) #(7 6 1) #(7 16 2) #(8 16 1) #(9 16 7)
     #(9 21 0) #(9 22 1) #(9 27 6) #(12 16 1) #(14 76 0) #(15 21 0) #(15 22 1)
     #(15 27 3) #(15 78 4) #(17 16 1) #(17 82 3) #(20 76 4) #(20 93 0) #(21 76 1)
     #(22 21 0) #(22 27 1) #(22 78 4) #(23 21 0) #(23 22 1) #(23 27 4) #(23 78 9)
     #(23 101 3) #(23 105 11) #(26 22 0) #(27 82 3) #(27 93 1) #(28 22 1) #(30 21 0)
     #(30 22 1) #(30 27 4) #(30 78 7) #(30 101 3) #(30 105 8) #(31 124 0) #(32 124 1)
     )))

;; (for-vector item input-table
;;   (printf "  " (int item[0]) "." (int item[1]) " = "
;;           (zpad 10 (hex (hash2 #x01000193 item[0] item[1])))
;;           "\n")
;;   (printf "  " (int item[0]) "." (int item[1]) " = "
;;           (zpad 10 (hex (hash (list item[0] item[1]) #x01000193)))
;;           "\n")
;;   )

(define (hash-item d item size)
  (mod (hash2 d item[0] item[1]) size))

(define (create-minimal-perfect-hash table)

  (define not-there 500000)

  (let ((size (vector-length table))
        (buckets (make-vector size (list:nil)))
        (G (make-vector size 0))
        (V (make-vector size not-there))
        (split 0))
    (for-vector elem table
      (let ((index (hash-item #x01000193 elem size)))
        (set! buckets[index] (list:cons elem buckets[index]))))
    (set! buckets (sort-vector (lambda (a b) (> (length a) (length b))) buckets))
    (let/cc break
      (for-range i size
        (let ((bucket buckets[i])
              (blen (length bucket))
              (d 1)
              (item 0)
              (slots (list:nil)))
          (when (<= blen 1)
            (set! split i)
            (break #u))
          ;; Repeatedly try different values of d until we find a hash function
          ;; that places all items in the bucket into free slots
          (while (< item blen)
            (let ((slot (hash-item d (nth bucket item) size)))
              (if (or (not (= V[slot] not-there))
                      (member-eq? slot slots))
                  (begin
                    (set! d (+ 1 d))
                    (set! item 0)
                    (set! slots (list:nil)))
                  (begin
                    (push! slots slot)
                    (set! item (+ item 1))))))
          (set! slots (reverse slots))
          (set! G[(hash-item #x01000193 (nth bucket 0) size)] d)
          (for-range j blen
            (set! V[(nth slots j)] (nth bucket j)[2]))
          )))
    ;; Only buckets with 1 item remain. Process them more quickly by directly
    ;; placing them into a free slot. Use a negative value of d to indicate
    ;; this.
    (let ((freelist '()))
      (for-range i size
        (if (= V[i] not-there)
            (push! freelist i)))
      (for-range i size
        (let ((bucket buckets[i])
              (blen (length bucket)))
          (when (= blen 1)
            (let ((slot (pop! freelist))
                  (item (nth bucket 0)))
              ;; we subtract one to ensure it's negative even if the zeroeth slot was
              ;; used.
              (set! G[(hash-item #x01000193 item size)] (- 0 slot 1))
              (set! V[slot] item[2])))))
      )
    (:tuple G V)))

(define (lookup G V k0 k1)
  (let ((key (list->vector (list k0 k1 -1)))
        (size (vector-length G)))
    (let ((d G[(hash-item #x01000193 key size)]))
      (if (< d 0)
          V[(- 0 d 1)]
          V[(hash-item d key size)]))))

(let (((G V) (create-minimal-perfect-hash input-table)))
  (printn G)
  (printn V)
  (for-range i (vector-length input-table)
    (let ((item input-table[i]))
      (printf "lookup " (lpad 3 (int i)) " = "
              (int (lookup G V item[0] item[1])) " = " (int item[2]) "\n"))))

