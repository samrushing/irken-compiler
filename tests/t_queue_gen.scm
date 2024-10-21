;; -*- Mode: Irken -*-


(require "lib/basis.scm")
(require "lib/queue.scm")

(let ((q (queue/make)))
  (for-range i 20
    (queue/push! q i))
  ;; 0 1 2 3 4 .. 19
  (for-range i 10
    (queue/pop! q))
  ;; 10 11 .. 19
  (for-range i 10
    (queue/push! q (* i i)))
  ;; 0 1 4 9 16 .. 81 10 11 .. 19
  (printf "q: (")
  (for item (queue/gen q)
    (printf " " (int item)))
  (printf ")\n")
  (printf "queue->list: (" (join int->string " " (queue->list q)) ")\n")
  )
