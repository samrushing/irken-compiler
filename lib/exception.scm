;; -*- Mode: Irken -*-

;; hope: this will eventually grow into a debugger.
;;
;; to get there, we will need to extend `raise` to capture
;; the current continuation so it can be examined.

(define (print-exception-exit e) : ((rsum 'a) -> 'b)
  (let ((name (variant->name e)))
    (printf "Exception \"" (sym name) "\" raised.\n")
    (printf "   values: ")
    (printn e)
    (%exit #f -1)
    ))

(set! *the-exception-handler* print-exception-exit)
