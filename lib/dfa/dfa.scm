;; -*- Mode: Irken -*-

;; consider: modifying rx to carry along with it a value for C.
;;   use the constructors to populate this value.

;; intersection of cartesian products
;; [labeled '^' in the paper]
(define (C-intersect s0 s1)
  (let ((result (set/empty)))
    (for-set x s0
      (for-set y s1
        (set/add! result charset-cmp (charset/intersection x y))
        ))
    result))

;; computes an approximation to the derivative charsets of an rx.
(define C
  (rx:eps)         -> (set/add (set/empty) charset-cmp charset/dot)
  (rx:sym cs)      -> (set/add* (set/empty) charset-cmp cs (charset/invert cs))
  (rx:cat r s)     -> (if (eq? (rx:eps) (n? r)) (C-intersect (C r) (C s)) (C r))
  (rx:or r s)      -> (C-intersect (C r) (C s))
  (rx:and r s)     -> (C-intersect (C r) (C s))
  (rx:star r)      -> (C r)
  (rx:not r)       -> (C r)
  (rx:group _ _ r) -> (C r)
  )

;; sort only by (fs,ts) so we can quickly find trans where
;;  we need to merge with another charset.
(define (tran-cmp a b)
  (match (int-cmp a.fs b.fs) with
    (cmp:=) -> (int-cmp a.ts b.ts)
    cmp     -> cmp
    ))

(define (tran-repr t)
  (format (int t.fs) " " (charset-repr t.sym) " -> " (int t.ts))
  )

(define (rx->dfa rx)
  (let ((urx (cmap/make rx-cmp)) ;; unique derivatives
        (trans (set/empty))      ;; dfa transitions
        (finals (set/empty))
        (states (list:nil)))

    ;; maybe add a new transition.  if it matches (fs,ts) an existing
    ;;   transition, merge the new symbol with the old.
    (define (add-tran tran)
      (match (set/getkey trans tran-cmp tran) with
        (maybe:yes t) -> (set! t.sym (charset/merge t.sym tran.sym))
        (maybe:no)    -> (set/add! trans tran-cmp tran)
        ))

    ;; walk the rx with derivatives, finding new states.
    (define (explore fs rx depth)
      ;;(printf "explore " (pp-rx rx) "\n")
      (push! states rx)
      (let ((moves (C rx)))
        (for-set move moves
          (if (not (charset/empty? move))
              (let ((rxd (deriv move depth rx))
                    (c urx.count)
                    (ts (cmap/add urx rxd)) ;; find/add state
                    (s0 (cmap->item urx ts))
                    )
                ;; (printf "ch " (rpad 15 (charset-repr move)) "-> " (pp-rx rxd) "\n")
                ;; (printf "   " (rpad 15 " ")                 " = " (pp-rx s0) "\n")
                (add-tran {fs=fs ts=ts sym=move})
                (if (> urx.count c) ;; we found a new state
                    (explore ts rxd (+ depth 1)))
                )))))

    ;; add state 0
    (cmap/add urx rx)
    (explore 0 rx 0)
    ;; find final states
    (for-map index rxd urx.rev
      (if (eq? (rx:eps) (n? rxd))
          (set/add! finals int-cmp index)))
    ;; convert to vector form.
    (let ((map (make-vector urx.count '())))
      (for-set tran trans
        (push! map[tran.fs] {ts=tran.ts sym=tran.sym}))
      {map=map size=urx.count finals=finals states=(reverse states)}
      )))
