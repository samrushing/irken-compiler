;; -*- Mode: Irken -*-

;; Note: this code includes support for an aborted (but not abandoned)
;;  attempt to implement submatching (similar to TDFAs).

;; consider: using a set for :or and :and, rather than
;;  converting between list/set/datatype during canonicalization.

(datatype mstate
  (:before)
  (:start)
  (:final)
  (:after)
  )

(typealias match {s=int e=int ms=mstate})

(define match-repr
  {s=s e=e ms=(mstate:before)} -> ""
  {s=s e=e ms=(mstate:start)}  -> (format "/" (int s) "-/")
  {s=s e=e ms=(mstate:final)}  -> (format "/" (int s) "-" (int e) "/")
  {s=s e=e ms=(mstate:after)}  -> (format "<" (int s) "-" (int e) ">")
  )

(typealias mrange {lo=int hi=int})

(datatype rx
  (:eps)                ;; empty string
  (:sym (list mrange))  ;; symbol
  (:cat rx rx)          ;; concatentation
  (:star rx)            ;; kleene star
  (:or rx rx)           ;; union
  (:and rx rx)          ;; intersection
  (:not rx)             ;; complement
  (:group int match rx) ;; submatch
  )

(define rx-repr
  (rx:eps)         -> "ε"
  (rx:sym s)       -> (charset-repr s)
  (rx:cat a b)     -> (format "(cat " (rx-repr a) " " (rx-repr b) ")")
  (rx:star a)      -> (format "(star " (rx-repr a) ")")
  (rx:or a b)      -> (format "(or " (rx-repr a) " " (rx-repr b) ")")
  (rx:and a b)     -> (format "(and " (rx-repr a) " " (rx-repr b) ")")
  (rx:not a)       -> (format "(not " (rx-repr a) ")")
  (rx:group n m a) -> (format "(group " (int n) " " (match-repr m) " " (rx-repr a) ")")
  )

(define pp-rx
  (rx:eps)             -> "ε"
  (rx:sym s)           -> (charset-repr s)
  (rx:cat a b)         -> (format (pp-rx a) (pp-rx b))
  (rx:or a b)          -> (format "(" (join pp-rx "|" (or->list  (rx:or a b) '()))  ")")
  (rx:and a b)         -> (format "(" (join pp-rx "&" (and->list (rx:and a b) '())) ")")
  (rx:star (rx:sym s)) -> (format (charset-repr s) "*")
  (rx:star a)          -> (format "(" (pp-rx a) ")*")
  (rx:not a)           -> (format (pp-rx a) "~")
  (rx:group n m a)     -> (format "{" (int n) (match-repr m) " " (pp-rx a) "}")
  )

;; should probably be a primop that does this.
(define rx->id
  (rx:eps)         -> 0
  (rx:sym _)       -> 1
  (rx:cat _ _)     -> 2
  (rx:star _)      -> 3
  (rx:or _ _)      -> 4
  (rx:and _ _ )    -> 5
  (rx:not _)       -> 6
  (rx:group _ _ _) -> 7
  )

;; partial ordering on rx

;; note: this ordering is crafted to ignore range data inside group
;; matches. [i.e., two rx that are identical except for the specifics
;; of their match data are considered equal]

(define (rx2-cmp a0 a1 b0 b1)
  (match (rx-cmp a0 a1) with
    (cmp:=) -> (rx-cmp b0 b1)
    cmp     -> cmp
    ))

(define (rx-cmp a b)
  (match (int-cmp (rx->id a) (rx->id b)) with
    (cmp:=)
    -> (match a b with
         (rx:cat a0 b0) (rx:cat a1 b1) -> (rx2-cmp a0 a1 b0 b1)
         (rx:or  a0 b0) (rx:or  a1 b1) -> (rx2-cmp a0 a1 b0 b1)
         (rx:and a0 b0) (rx:and a1 b1) -> (rx2-cmp a0 a1 b0 b1)
         (rx:not a0)    (rx:not b0)    -> (rx-cmp a0 b0)
         (rx:star a0)   (rx:star b0)   -> (rx-cmp a0 b0)
         (rx:sym s0)    (rx:sym s1)    -> (charset-cmp s0 s1)
         ;; note how match data is ignored.
         (rx:group n0 m0 a0) (rx:group n1 m1 a1)
         -> (match (int-cmp n0 n1) with
              (cmp:=) -> (rx-cmp a0 a1)
              cmp     -> cmp
              )
         ;; e.g. (rx:eps) (rx:eps)
         _ _ -> (cmp:=)
         )
    cmp -> cmp
    ))

(define fold-or
  ()        -> (impossible)
  (hd)      -> hd
  (hd . tl) -> (rx:or hd (fold-or tl))
  )

(define fold-and
  ()        -> (impossible)
  (hd)      -> hd
  (hd . tl) -> (rx:and hd (fold-and tl))
  )

(define cat->list
  (rx:cat (rx:cat a b) c) acc -> (cat->list c (list:cons a (list:cons b acc)))
  (rx:cat a b) acc            -> (cat->list a (list:cons b acc))
  tail acc                    -> (list:cons tail acc)
  )

(define or->list
  (rx:or (rx:or a b) c) acc -> (or->list c (list:cons a (list:cons b acc)))
  (rx:or a b) acc           -> (or->list b (list:cons a acc))
  tail acc                  -> (list:cons tail acc)
  )

(define and->list
  (rx:and (rx:and a b) c) acc -> (and->list c (list:cons a (list:cons b acc)))
  (rx:and a b) acc            -> (and->list b (list:cons a acc))
  tail acc                    -> (list:cons tail acc)
  )

;; we need to walk an expression, and collect any string of
;; OR/AND into a list, sort that list, and return the appropriate
;; result.

;; this version will collapse two equal states with unequal
;;  match data...
;;
;; (define (canon-order-or* rx)
;;   (let ((rxs (or->list rx '()))
;;      (canon (list->set rxs rx< (set/empty))))
;;     (fold-or (set->list canon))))

;; so we need a version that merges submatch data when this happens.

;; given two rx (that we know are equal), set the submatch data
;;   in ``a`` to the leftmost match.
(define (merge-groups! a b)
  (define M
    (rx:cat a0 b0) (rx:cat a1 b1) -> (begin (M a0 a1) (M b0 b1))
    (rx:or a0 b0)  (rx:or a1 b1)  -> (begin (M a0 a1) (M b0 b1))
    (rx:and a0 b0) (rx:and a1 b1) -> (begin (M a0 a1) (M b0 b1))
    (rx:star a0)   (rx:star a1)   -> (M a0 a1)
    (rx:not a0)    (rx:not a1)    -> (M a0 a1)
    (rx:group n0 m0 a0) (rx:group n1 m1 a1)
    -> (begin
         (when (< m1.s m0.s)
         (set! m0.s m1.s)
         (set! m0.e m1.e)
         )
         (M a0 a1))
    a b -> #u
    )
  (M a b)
  )

(define (canon-order-or rx)
  (let ((rxs (or->list rx '()))
        (canon (set/empty)))
    (for-list rx rxs
      (match (set/getkey canon magic-cmp rx) with
        (maybe:no) -> (set/add! canon magic-cmp rx)
        ;; an equivalent rx has already been seen.
        ;;  merge any submatch data that may be present.
        (maybe:yes rx0)
        -> (merge-groups! rx0 rx)))
    ;; result
    (fold-or (set->list canon))))

;; debug: wrapper to print when COO kicks in.
;; (define (canon-order-or rx)
;;   (let ((rx0 rx)
;;         (rr0 (pp-rx rx0))
;;         (rx1 (canon-order-or* rx0))
;;         (rr1 (pp-rx rx1)))
;;     (when (not (string=? rr0 rr1))
;;       (printf "COO " rr0 "\n")
;;       (printf "    " rr1 "\n"))
;;     rx1))

;; XXX same work needs to be done here!
(define (canon-order-and rx)
  (let ((rxs (and->list rx '()))
        (canon (list->set rxs magic-cmp (set/empty))))
    (fold-and (set->list canon))))

;; 'smart' constructors enforce canonicalization.

(define (rx-eps) (rx:eps))
(define (rx-sym s) (rx:sym s))
(define rx-null (rx:sym '()))

(define rx-cat
  (rx:sym ()) _   -> rx-null
  _ (rx:sym ())   -> rx-null
  (rx:eps) r      -> r
  r (rx:eps)      -> r
  (rx:cat r s) t  -> (rx:cat r (rx-cat s t))
  r s             -> (rx:cat r s)
  )

(define rx-star
  (rx:eps)    -> (rx:eps)
  (rx:star a) -> (rx:star a)
  a           -> (rx:star a)
  )

(define rx-or
  (rx:sym ()) r          -> r
  (rx:not (rx:sym ())) r -> r
  r (rx:sym ())          -> r
  r (rx:not (rx:sym ())) -> r
  (rx:sym a) (rx:sym b)  -> (rx:sym (charset/merge a b))
  r (rx:sym ())          -> r
  r s                    -> (canon-order-or (rx:or r s))
  )

(define rx-and
  (rx:sym ()) r         -> rx-null
  r (rx:sym ())         -> rx-null
  (rx:eps) r            -> r
  r (rx:eps)            -> r
  (rx:and r s) t        -> (rx:and r (rx-and s t))
  (rx:sym a) (rx:sym b) -> (rx:sym (charset/intersection a b))
  r s                   -> (canon-order-and (rx:and r s))
  )

(define rx-not
  (rx:not (rx:sym s)) -> (rx:sym (charset/invert s))
  (rx:not a)          -> a
  a                   -> (rx:not a)
  )

;; XXX need to re-order groups by group number
;;  [i.e., (group 1 (group 0 r)) => (group 0 (group 1 r))
;;  [can they ever get out of order?]
(define rx-group
  _ _ (rx:sym ()) -> rx-null

  n0 m0 (rx:group n1 m1 r)
  -> (cond ((= n0 n1) (rx-group n1 m1 r)) ;; favor the last submatch of a '*'
           ((< n0 n1) (rx:group n0 m0 (rx-group n1 m1 r)))
           (else (rx:group n1 m1 (rx-group n0 m0 r))))

  n m r -> (rx:group n m r)
  )

;; syntax
(define (rx-plus a)
  (rx-cat a (rx-star a)))
(define (rx-optional a)
  (rx-or (rx:eps) a))
(define (rx-diff a b)
  (rx-and a (rx-not b)))

(define nullmatch {s=0 e=0 ms=(mstate:before)})

(define (number-groups a)
  (let ((counter (make-counter 0)))
    (define W
      (rx:group _ m r) -> (rx:group (counter.inc) m (W r))
      (rx:cat a b)     -> (rx:cat (W a) (W b))
      (rx:star a)      -> (rx:star (W a))
      (rx:or a b)      -> (rx:or (W a) (W b))
      (rx:and a b)     -> (rx:and (W a) (W b))
      (rx:not a)       -> (rx:not (W a))
      a                -> a
      )
    (W a)))

(define (find-and-parse-charset s pos0)
  (let loop ((chars '())
             (pos pos0))
    (match (string-ref s pos) with
      #\] -> (:tuple (parse-charset0 (reverse chars)) (+ 1 pos))
      #\\ -> (loop (list:cons (string-ref s (+ pos 1)) chars) (+ pos 2))
      ch  -> (loop (list:cons ch chars) (+ pos 1))
      )))

(define (p-rx rx pos0)

  ;; build a list of items to be concatenated.
  (let loop ((exp '())
             (pos pos0))

    ;; note: done in reverse
    (define concat
      ()       -> (error "empty concat?")
      (x)      -> x
      (a b)    -> (rx-cat b a)
      (a . tl) -> (rx-cat (concat tl) a)
      )

    (define (bracketed how)
      ;; consume one (bracketed) item, loop
      (let (((sub pos0) (p-rx rx (+ 1 pos))))
        (loop (list:cons (how sub) exp) pos0)))

    (define (postfix how)
      ;; apply an operator to previous item, loop
      (loop (list:cons (how (car exp)) (cdr exp)) (+ 1 pos)))

    (define (infix how)
      ;; combine previous items with the rest of the expression.
      (let (((sub pos0) (p-rx rx (+ 1 pos))))
        (:tuple (how (concat exp) sub) pos0)))

    (define (charset)
      (let (((set pos0) (find-and-parse-charset rx (+ 1 pos))))
        (loop (list:cons (rx-sym set) exp) pos0)))

    (if (= pos (string-length rx))
        (:tuple (concat exp) pos)
        (match (string-ref rx pos) with
          #\) -> (:tuple (concat exp) (+ 1 pos))
          #\} -> (:tuple (concat exp) (+ 1 pos))
          #\( -> (bracketed id)
          #\{ -> (bracketed (lambda (r) (rx:group 0 nullmatch r)))
          #\* -> (postfix rx-star)
          #\+ -> (postfix rx-plus)
          #\? -> (postfix rx-optional)
          #\~ -> (postfix rx-not)
          #\| -> (infix rx-or)
          #\^ -> (infix rx-and)
          #\- -> (infix rx-diff)
          #\[ -> (charset)
          #\\ -> (loop (list:cons
                        (rx:sym (charset/single (char->ascii (string-ref rx (+ pos 1)))))
                        exp) (+ pos 2))
          #\. -> (loop (list:cons (rx-sym charset/dot) exp) (+ pos 1))
          ch  -> (loop (list:cons (rx-sym (charset/single (char->ascii ch))) exp) (+ pos 1))
          ))
    ))

;; the following chars need escaping inside regexes.
(define rx-metachars (string->list "()[]{}*+?~|^-\\."))

(define (parse-rx r)
  (match (p-rx r 0) with
    (:tuple rx pos)
    -> (begin
         (assert (= pos (string-length r)))
         (number-groups rx))))
