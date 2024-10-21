;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/dfa/dfa.scm")

;; For _now_, this uses the charset version of a DFA,
;;  where the DFA is represented as a list of (charset, to-state)
;;  rather than a complete table-of-tables.  Definitely worth
;;  revisiting and duplicating/merging-with the code form lexicon.scm
;;  that builds a flattened dfa.

;; (define (print-dfa dfa)
;;   (printf "dfa {\n")
;;   (for-range i dfa.size
;;     (printf (lpad 3 (int i)) ":\n")
;;     (for-list tran dfa.map[i]
;;       (printf "  " (lpad 30 (charset-repr tran.sym)) " -> " (int tran.ts) "\n"))
;;     )
;;   (printf " finals: " (join int->string "," (set->list dfa.finals)) "\n")
;;   (printf "}\n")
;;   )

(define (regex/compile s)
  (let ((rx (parse-rx s))
	(dfa (rx->dfa rx)))

    ;; (print-dfa dfa)

    (define (match? s)
      (let ((state 0))
	(for-string ch s
	  (set! state
		(let/cc break
		  (for-list tran dfa.map[state]
			    (when (charset/in? tran.sym ch)
			      (break tran.ts)))
		  0 ;; not reached if charsets are properly built.
		  )))
	(set/member? dfa.finals int-cmp state)
	))

    match?
    ))


	  
    
