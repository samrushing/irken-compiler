;; -*- Mode: Irken -*-

(define (print-dfa dfa)
  (printf "dfa {\n")
  (for-range i dfa.size
    (printf (lpad 3 (int i)) ":\n")
    (for-list tran dfa.map[i]
      (printf "  " (lpad 30 (charset-repr tran.sym)) " -> " (int tran.ts) "\n"))
    )
  (printf " finals: " (join int->string "," (set->list dfa.finals)) "\n")
  (printf "}\n")
  )

(define (dfa->dot dfa title)

  (define (escape-string s)
    (string-concat
     (map (lambda (ch)
            (cond ((eq? ch #\") "\\\"")
                  ((eq? ch #\\) "\\\\")
                  (else (char->string ch))))
          (string->list s))))

  (printf "digraph xxx {\n")
  (printf "label = \"" title "\";\n")
  (printf "size=\"8,5\"\n")
  (printf "node [shape = circle];\n")
  (printf "rankdir = LR;\n")
  (printf "edge [fontsize = 10];\n")
  (for-range i dfa.size
    (let ((rx (nth dfa.states i))
          (label (format (join "\\n|" (map escape-string (string-split (pp-rx rx) #\|)))))
          (shape (if (maybe? (tree/member dfa.finals int-cmp i))
                     "doublecircle"
                     "circle")))
      (printf "  node [ shape = " shape
              ", label = \"" (int i)
              ":" label "\" ] " (int i)
              ";\n")))
  (for-range i dfa.size
    (for-list tran dfa.map[i]
      (printf "  " (int i) " -> " (int tran.ts)
              " [ label = \"" (escape-string (charset-repr tran.sym)) "\" ];\n")))
  (printf "}\n")
  )
