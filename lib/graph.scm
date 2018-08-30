;; -*- Mode: Irken -*-

;; XXX move this into lib/graph.scm, and remove the OO version from self/graph.scm.

;; graph algorithms using maps and sets.

;; graph: (tree 'a (set 'a))
;; a graph is a map from key-type to set-of-key-type.

(define (transpose g cmp)
  (let ((gt (tree/empty)))
    (for-map k v g
      ;; note the use of a record as a 'cell' here.
      (tree/insert! gt cmp k {s=(set/empty)}))
    (for-map k vs g
      (for-set v vs
        (match (tree/member gt cmp v) with
          (maybe:no)       -> (tree/insert! gt cmp v {s=(set/make cmp k)})
          (maybe:yes cell) -> (set/add! cell.s cmp k))))
    ;; unwrap the values from their cells.
    (let ((r (tree/empty)))
      (for-map k v gt
        (tree/insert! r cmp k v.s))
      r)))

;; http://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
;; returns the strongly-connected components in topological order.

(define (strongly g cmp) ;; (graph k) => (list (list k))
  (let ((s '())
        (visited (set/empty)))
    (define (visit0 u)
      (set/add! visited cmp u)
      (when-maybe vl (tree/member g cmp u)
        (for-set v vl
          (if (not (set/member? visited cmp v))
              (visit0 v))))
      (push! s u))
    ;; walk the graph forward, pushing finished nodes onto <s>
    (for-map u v g
      (if (not (set/member? visited cmp u))
          (visit0 u)))
    (let ((gt (transpose g cmp))
          (visited (set/empty))
          (r0 '())
          (r1 (set/empty)))
    (define (visit1 u)
      (set/add! visited cmp u)
      (when-maybe vl (tree/member gt cmp u)
        (for-set v vl
          (if (not (set/member? visited cmp v))
              (visit1 v))))
      (set/add! r1 cmp u))
    ;; walk backward, popping strongly connected components off <s>
    (while (not (null? s))
      (let ((u (pop! s)))
        (when (not (set/member? visited cmp u))
          (set! r1 (set/empty))
          (visit1 u)
          (push! r0 (set->list r1)))))
    r0)))
