;; -*- Mode: Irken -*-

(require "lib/basis.scm")

;; (printn *the-exception-handler*)
;; (printn (%exn-raise #f (:thing)))
;; (*the-exception-handler* (%exn-raise #f (:thing)))

;;(error1 "uncaught exception" (:thing))

(printf "read cycle counter: " (int (read-cycle-counter)) "\n")

(pp (get-metadata) 120)

(printf "\n")

(printf "read cycle counter: " (int (read-cycle-counter)) "\n")

(printf "thing: " (int (/ 27 (- 5 5))) "\n")

