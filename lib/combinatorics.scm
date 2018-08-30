;; -*- Mode: Irken -*-

;; (product '(0 1) '(#\a #\b)) => ((0 #\a) (0 #\b) (1 #\a) (1 #\b))
(define (product al bl)
  (makegen emit
    (for-list a al
      (for-list b bl
        (emit (:tuple a b))))))

;; XXX explore the factorial-number-system/lehmer approach.  sounds fun.
;; (permutations '(0 1 2)) => (0 1 2) (1 0 2) (1 2 0) (0 2 1) (2 0 1) (2 1 0)
(define (permutations al)
  (define (perm n al)
    (makegen emit
      (match n al with
        2 (a b)
        -> (begin (emit (list a b)) (emit (list b a)))
        n (hd . tl)
        -> (for sub (perm (- n 1) tl)
             (for-range i n ;; 0 (1 2) => (0 1 2) (1 0 2) (1 2 0)
               (emit (append (slice sub 0 i) (list hd) (slice sub i (- n 1))))))
        n l
        -> #u
        )))
  (perm (length al) al))

;; n choose k

;; from ocaml tutorial '99 problems'
;; # let rec extract k list =
;;     if k <= 0 then [ [] ]
;;     else match list with
;;          | [] -> []
;;          | h :: tl ->
;;             let with_h = List.map (fun l -> h :: l) (extract (k-1) tl) in
;;             let without_h = extract k tl in
;;             with_h @ without_h;;

(define (combinations xs k)
  (if (<= k 0)
      (list (list:nil))
      (match xs with
        () -> (list:nil)
        (hd . tl)
        -> (let ((yhd (map (lambda (x) (list:cons hd x))
                           (combinations tl (- k 1))))
                 (nhd (combinations tl k)))
             (append yhd nhd))
        )))

(define (combinations* xs k)
  (makegen emit
    (if (= k 0)
        (emit (list:nil))
        (match xs with
          () -> #u
          (hd . tl)
          -> (begin
               (for sub (combinations* tl (- k 1))
                 (emit (list:cons hd sub))) ;; with hd
               (for sub (combinations* tl k)
                 (emit sub))) ;; without hd
          ))))

(define (combinations** xs k)
  (makegen emit
    (if (= k 0)
        (emit (list:nil))
        (match xs with
          () -> #u
          (hd . tl)
          -> (begin
               (for sub (combinations** tl k)
                 (emit sub)) ;; without hd
               (for sub (combinations** tl (- k 1))
                 (emit (list:cons hd sub)))) ;; with hd
          ))))
