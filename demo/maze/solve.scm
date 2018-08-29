;; -*- Mode: Irken -*-

;; breadth-first search will find the shortest path.

(define (BFS G m n)
  ;; we designate:
  ;;  (0, 0) as the start
  ;;  (m, n) as the goal

  (define (coord->node x y)
    (+ x (* m y)))

  (define (node->coord n)
    (let (((y x) (divmod n m)))
      (:tuple x y)))

  (define (add-boundary-walls)
    (let ((G0 (copy-vector G)))
      ;; UDLR
      (for-range x m
        (set-bit! G0[(coord->node x 0)] 3)
        (set-bit! G0[(coord->node x (- n 1))] 2))
      (for-range y n
        (set-bit! G0[(coord->node 0 y)] 1)
        (set-bit! G0[(coord->node (- m 1) y)] 0))
      G0))

  (let ((visited (make-vector (* m n) #f))
        (queue (set/empty))
        (goal (coord->node (- m 1) (- n 1)))
        (solution '()))

    (let/cc all-done

      (define move
        k #b0001 -> (+ k 1)
        k #b0010 -> (- k 1)
        k #b0100 -> (+ k m)
        k #b1000 -> (- k m)
        k v      -> (impossible)
        )

      (define (get-unvisited-neighbors node)
        (let ((walls G[node])
              (r '()))
          (for-range i 4
            (when (not (bit-set? walls i))
              (let ((node0 (move node (<< 1 i))))
                (when (not visited[node0])
                  (push! r node0)))))
          r))

      (define (search node dx path)
        (set! visited[node] #t)
        (let ((neighbors (get-unvisited-neighbors node)))
          (for-list node0 neighbors
            (set/add! queue magic-cmp (:tuple (+ dx 1) node0 (list:cons node0 path)))
            (when (= node0 goal)
              (all-done (reverse (list:cons goal path)))))
          (when (not (set/empty? queue))
            (let ((least (set/min queue))
                  ((dx0 node0 path0) least))
              (set/delete! queue magic-cmp least)
              (search node0 dx0 path0)))))

      ;; (printf "adding boundary walls...\n")
      (set! G (add-boundary-walls))
      ;; (printf "done.\n")
      (search 0 0 (LIST 0))
      '() ;; failure
      )))


