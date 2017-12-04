;; -*- Mode: Irken -*-

;; An Earley Parser.
;;
;; This is based on the description from https://en.wikipedia.org/wiki/Earley_parser
;;

(datatype prod
  (:nt symbol)
  (:t symbol)
  )

(define terminal?
  (prod:t _) -> #t
  _          -> #f
  )

(define prod->name
  (prod:nt name) -> name
  (prod:t name)  -> name
  )

(define prod-repr
  (prod:nt name) -> (format "<" (sym name) ">")
  (prod:t name)  -> (format "{" (sym name) "}")
  )

(define EOF (prod:t 'eof))
(define NULTOK {kind='nul val=""})

(typealias token {kind=symbol val=string})

(datatype parse
  (:nt symbol (list parse))
  (:t token)
  )

(define parse-repr
  (parse:nt kind subs)
  -> (format "(" (sym kind) " " (join parse-repr " " subs) ")")
  (parse:t tok)
  -> (format "{" (sym tok.kind) ":" tok.val "}")
  )

(define parse->sexp
  (parse:nt kind subs)
  -> (sexp1 kind (map parse->sexp subs))
  (parse:t tok)
  -> (sexp:list (LIST (sexp:symbol tok.kind) (sexp:string tok.val)))
  )

(define (token-repr tok)
  (format "{" (sym tok.kind) ":" (string tok.val) "}"))

(define (state-repr s)
  (let ((prod (map prod-repr s.prod)))
    (insert! prod s.dot "•")
    (set! prod (list:cons (prod-repr s.nt) (list:cons "→" prod)))
    (format "(" (join " " prod) " @" (int s.start) ")")))

;; a growable vector object
(define (make-gvec len item)
  (let ((ob {v=(make-vector len item) len0=len len1=len}))
    (define (ref n) ob.v[n])
    (define (set! n item) (set! ob.v[n] item))
    (define (len) ob.len0)
    (define (list)
      (let ((r '()))
        (for-range i ob.len0
          (PUSH r ob.v[i]))
        (reverse r)))
    (define (append item)
      (if (= ob.len0 ob.len1)
          ;; we need to grow
          (let ((newlen (+ 1 (/ (* ob.len0 3) 2))) ;; 50%
                (newvec (make-vector newlen item)))
            (for-range i ob.len0
              (set! newvec[i] ob.v[i]))
            ;; note: done above.
            ;; (set! newvec[ob.len0] item)
            (set! ob.v newvec)
            (set! ob.len1 newlen)
            (set! ob.len0 (+ 1 ob.len0)))
          ;; room
          (begin
            (set! ob.v[ob.len0] item)
            (set! ob.len0 (+ 1 ob.len0)))))
    {ref=ref set=set! len=len append=append list=list}
    ))

(define (completed? state)
  (= state.dot (length state.prod)))

(define (prod=? p0 p1)
  (eq? (prod->name p0) (prod->name p1)))

(define (earley grammar nt0 tokgen)
  (let ((S (make-gvec 1 (make-gvec 1 {nt=nt0 dot=0 prod=(LIST nt0 EOF) start=0})))
        (k 0)
        (toks (make-gvec 0 NULTOK)))

    (define get-prod
      (prod:nt name)
      -> (alist/get grammar name "no such production")
      x -> (error1 "get-prod: not an NT?" x)
      )

    (define (maybe-add index state)
      (let ((states (S.ref index))
            (found #f))
        (for-range i (states.len)
          (if (magic=? state (states.ref i))
              (set! found #t)))
        (when (not found)
          ;;(printf "add " (int index) " " (state-repr state) "\n")
          (states.append state))))

    (define (bump-dot x)
      {nt=x.nt dot=(+ 1 x.dot) prod=x.prod start=x.start}
      )

    (define (completer nt start)
      ;; For every state in S(k) of the form (X → γ •, j), find states
      ;; in S(j) of the form (Y → α • X β, i) and add (Y → α X • β, i)
      ;; to S(k).
      ;;(printf "completer for " (prod-repr nt) "\n")
      (let ((states (S.ref start)))
        (for-range i (states.len)
          (let ((state (states.ref i)))
            ;; (printf "   --- " (state-repr state) "\n")
            ;; (printf "       " (int (+ state.dot 1)) "/" (int (length state.prod)) "\n")
            (if (and
                 (> (length state.prod) state.dot)
                 (prod=? nt (nth state.prod state.dot)))
                (maybe-add k (bump-dot state))
                )))))

    (define (predictor nt)
      ;; For every state in S(k) of the form (X → α • Y β, j) (where j
      ;; is the origin position as above), add (Y → • γ, k) to S(k)
      ;; for every production in the grammar with Y on the left-hand
      ;; side (Y → γ).
      ;;(printf "predictor\n")
      (for-list prod (get-prod nt)
        (maybe-add k {nt=nt dot=0 prod=prod start=k})))

    (define (add-next state)
      (if (= (S.len) (+ k 1))
          (S.append (make-gvec 1 state))
          (maybe-add (+ 1 k) state)))

    (define (step tok)
      (let ((states (S.ref k))
            (j 0)
            (scanned? #f))
        ;;(printf "----------------------- step " (int k) " " (token-repr tok) "----------\n")
        (while (< j (states.len))
          ;;(printf "while " (int j) "/" (int (states.len)) "\n")
          (let ((state (states.ref j)))
            ;;(printf "state " (int j) " " (state-repr state) "\n")
            (set! j (+ j 1))
            ;; each state in the set falls into one of three categories:
            ;; 1) the state is complete (i.e., the dot is at the end)
            ;; 2) the state expects a terminal
            ;; 3) the state expects a non-terminal
            (if (completed? state)
                ;; 1 state is complete
                (completer state.nt state.start)
                (let ((nextprod (nth state.prod state.dot)))
                  (if (terminal? nextprod)
                      ;; 2) expects a terminal
                      (when (eq? tok.kind (prod->name nextprod))
                        ;; If a is the next symbol in the input stream, for every state
                        ;; in S(k) of the form (X → α • a β, j), add (X → α a • β, j) to
                        ;; S(k+1).
                        ;;(printf "scanner " (token-repr tok) " @" (int k) "\n")
                        (set! scanned? #t)
                        (add-next (bump-dot state)))
                      ;; 3) a non-terminal - predict it
                      (predictor nextprod))))))
        (when (not scanned?)
          (raise (:NoParse)))
        ))

    (define (build-parse-tree)
      ;; this uses the technique described in "Parsing Techniques - A Practical Guide".
      ;; [https://dickgrune.com/Books/PTAPG_1st_Edition/]
      ;; the method described by Earley is confusing (and wrong in some cases).

      ;; convert to a vector of lists, including only completed states.
      ;; also: reverse the states since we want to visit them in that order.
      (define (complete-states)
        (list->vector
         (map (lambda (x)
                (reverse (filter completed? (x.list))))
              (S.list))))

      ;; remove non-completed states
      (let ((all (complete-states)))

        (define (walk d nt end)
          ;;(printf (repeat d "  ") "walk " (int end) " " (prod-repr nt) " " (int (length all[end])) " prods \n")
          (let/cc return
            (while (> (length all[end]) 0)
              ;; pop each state off of the list in order to avoid
              ;; infinite recursion on the same rule.
              (let ((item (pop all[end])))
                ;;(printf (repeat d "  ") "  pop " (state-repr item) "\n")
                (if (prod=? item.nt nt)
                    (let ((r '()))
                      (for-list x (reverse item.prod)
                        ;;(printf (repeat d "  ") "x " (prod-repr x) " prod = " (join prod-repr " " item.prod) "\n")
                        (if (not (terminal? x))
                            (let (((y end0) (walk (+ d 1) x end)))
                              (PUSH r y)
                              (set! end end0)
                              ;;(printf (repeat d "  ") "nt " (prod-repr x) " end " (int end) "\n")
                              )
                            (begin
                              (set! end (- end 1))
                              (PUSH r (parse:t (toks.ref end)))
                              ;;(printf (repeat d "  ") "token " (token-repr (toks.ref end)) " end := " (int end) "\n")
                              )))
                      (return (:tuple (parse:nt (prod->name nt) r) end)))))
              )
            ;;(printf "Unable to Parse\n")
            (raise (:NoParse))
            ))
        ;; (printf "completed {\n")
        ;; (for-range i (vector-length all)
        ;;   (printf "--- " (int i) "\n")
        ;;   (for-list item all[i]
        ;;      (printf (state-repr item) "\n")))
        ;; (printf "}\n")
        (let ((end0 (- (vector-length all) 1))
              ((r end) (walk 0 nt0 end0)))
          (match r with
            ;; strip off outer result caused by the fake production
            ;; rule we put in of `root := root EOF`.
            (parse:nt _ (root eof))
            -> root
            _ -> (impossible)
            ))))

    (for tok tokgen
      (step tok)
      (toks.append tok)
      (set! k (+ 1 k)))
    ;; (printf "k = " (int k) "\n")
    ;; (printf "----------------------\n")
    ;; (for-range i (S.len)
    ;;   (let ((states (S.ref i)))
    ;;     (printf "state " (int i) "\n")
    ;;     (for-range j (states.len)
    ;;       (printf "  " (state-repr (states.ref j)) "\n"))
    ;;     ))
    (build-parse-tree)
    ))
