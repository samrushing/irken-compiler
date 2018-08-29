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
  (prod:nt name) -> (format (ansi green "<" (sym name) ">"))
  (prod:t name)  -> (format (ansi blue "{" (sym name) "}"))
  )

(define EOF (prod:t 'EOF))
(define NULTOK {kind='nul val="" range=(range:f)})

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
          (push! r ob.v[i]))
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
  (let ((top-prod (prod:nt 'top))
        (S (make-gvec 1 (make-gvec 1 {nt=top-prod dot=0 prod=(LIST nt0 EOF) start=0})))
        (M (make-gvec 1 (cmap/make magic-cmp)))
        (k 0)
        (toks (make-gvec 0 NULTOK))
        )

    (define get-prod
      (prod:nt name)
      -> (alist/get grammar name "no such production")
      x -> (error1 "get-prod: not an NT?" x)
      )

    ;; (define (maybe-add index state)
    ;;   (let ((states (S.ref index))
    ;;         (found #f))
    ;;     (for-range i (states.len)
    ;;       (if (magic=? state (states.ref i))
    ;;           (set! found #t)))
    ;;     (when (not found)
    ;;       (states.append state))))

    ;; this version uses a counting-map to quickly determine if the
    ;;   state is already present.
    (define (maybe-add index state)
      (let ((states (S.ref index))
            (map (M.ref index))
            (count map.count))
        (cmap/add map state)
        (when (> map.count count)
          (states.append state))
        ))

    (define (bump-dot x)
      {nt=x.nt dot=(+ 1 x.dot) prod=x.prod start=x.start}
      )

    (define (completer nt start)
      ;; For every state in S(k) of the form (X → γ •, j), find states
      ;; in S(j) of the form (Y → α • X β, i) and add (Y → α X • β, i)
      ;; to S(k).
      ;; (printf (bold "complete " (prod-repr nt) " " (int start) "\n"))
      (let ((states (S.ref start)))
        (for-range i (states.len)
          (let ((state (states.ref i)))
            (when (and (> (length state.prod) state.dot)
                       (prod=? nt (nth state.prod state.dot)))
              (maybe-add k (bump-dot state))
              )))))

    (define (predictor nt)
      ;; For every state in S(k) of the form (X → α • Y β, j) (where j
      ;; is the origin position as above), add (Y → • γ, k) to S(k)
      ;; for every production in the grammar with Y on the left-hand
      ;; side (Y → γ).
      (for-list prod (get-prod nt)
        (maybe-add k {nt=nt dot=0 prod=prod start=k})))

    (define (add-next state)
      (cond ((= (S.len) (+ k 1))
             (S.append (make-gvec 1 state))
             (M.append (cmap/make magic-cmp)))
            (else
             (maybe-add (+ 1 k) state))))

    (define (step tok)
      (let ((states (S.ref k))
            (j 0)
            (scanned? #f))
        ;; (printf "step " (int k) " " (sym tok.kind) " " (string tok.val) "\n")
        (while (< j (states.len))
          (let ((state (states.ref j)))
            (set! j (+ j 1))
            ;; each state in the set falls into one of three categories:
            ;; 1) the state is complete (i.e., the dot is at the end)
            ;; 2) the state expects a terminal
            ;; 3) the state expects a non-terminal
            (if (completed? state)
                ;; 1) state is complete
                (completer state.nt state.start)
                (let ((nextprod (nth state.prod state.dot)))
                  ;; (printf "state: " (state-repr state) "\n")
                  (if (terminal? nextprod)
                      ;; 2) expects a terminal
                      (when (eq? tok.kind (prod->name nextprod))
                        ;; If a is the next symbol in the input stream, for every state
                        ;; in S(k) of the form (X → α • a β, j), add (X → α a • β, j) to
                        ;; S(k+1).
                        (set! scanned? #t)
                        (add-next (bump-dot state)))
                      ;; 3) a non-terminal - predict it
                      (predictor nextprod))))))
        (when (not scanned?)
          (raise (:NoParse tok)))
        ))

    (define (build-parse-tree)
      ;; this uses the technique described in "Parsing Techniques - A Practical Guide".
      ;; [https://dickgrune.com/Books/PTAPG_1st_Edition/]
      ;; the method described by Earley is confusing (and wrong in some cases).

      ;; convert to a vector of lists, including only completed states.
      ;; also: reverse the states since we want to visit them in that order.
      (define (complete-states)
        (list->vector
         (map
          (lambda (x)
            (filter completed? (x.list)))
          (S.list))))

      ;; we eliminate some items from consideration by computing
      ;; an earliest possible start position.  If we are looking
      ;; at a production: `thing : X Y Z sub0 @ 0`, then 'sub0' cannot
      ;; possibly start earlier than position 3 because of the three
      ;; terminals at the start of the rule.
      (define (earliest-start item)
        (let loop ((start item.start)
                   (prods item.prod))
          (match prods with
            ((prod:t _) . tl) -> (loop (+ 1 start) tl)
            _                 -> start
            )))

      ;; remove non-completed states
      (let ((all (complete-states)))

        (define (indent d)
          (printf (lpad 3 (int d)) " " (repeat d " ")))

        (defmacro indentf
          (indentf d x ...)
          -> (begin (indent d) (printf x ...))
          )

        ;; generate all possible parses of this NT.
        (define (walk-rule d subs start end visited)
          ;; (indentf d "walk* s=" (int start) " e=" (int end) " (" (join prod-repr ", " subs) ")\n")
          (makegen emit
            (match subs with
              () -> (emit (:tuple start end '()))
              (hd . tl) ;; NOTE: we are walking the items in reverse here!
              -> (cond ((terminal? hd)
                        (for tl-parse (walk-rule d tl start (- end 1) visited)
                          (match tl-parse with
                            (:tuple start0 end0 y)
                            ;; hd = Z tl = (Y X)
                            ;; start = tl_start
                            ;; end   = hd_end (in this case 'end')
                            -> (let ((e-1 (- end 1))
                                     (x (parse:t (toks.ref e-1)))
                                     (r (list:cons x y)))
                                 ;; (indentf d  "=>  T s=" (int start0)
                                 ;;          " e=" (int end)
                                 ;;          " (" (join parse-repr " " r)
                                 ;;          ")\n")
                                 (emit (:tuple start0 end r)))
                            )))
                       (else
                        ;; here we encode the following common-sense rule:
                        ;; if we are trying 'x -> TERM0 TERM1 TERM2 y @7',
                        ;; then we know that 'y' has a hard start at 10.
                        (let ((hard? (all? terminal? tl))
                              (start0 (if hard? (+ start (length tl)) start)))
                          (for hd-parse (walk (+ 1 d) hd start0 end visited hard?)
                            (match hd-parse with
                              (:tuple start1 end1 x)
                              -> (begin
                                   ;; (indentf d "hd s=" (int start1) " e=" (int end1)
                                   ;;          " hd=" (prod-repr hd) " " (parse-repr x) "\n")
                                   ;; (indentf d "tl = (" (join prod-repr ", " tl) ")\n")
                                   (for tl-parse (walk-rule d tl start start1 visited)
                                     (match tl-parse with
                                       (:tuple start0 end0 y)
                                       -> (let ((r (list:cons x y)))
                                            ;; (indentf d "=> NT s=" (int start0)
                                            ;;          " e=" (int end1)
                                            ;;          " (" (join parse-repr " " r)
                                            ;;          ")\n")
                                            (emit (:tuple start0 end1 r)))))
                                   )
                              )))))
              )))

        (define (candidate? item start end hard-start?)
          (cond (hard-start?
                 (= item.start start))
                ((terminal? (last item.prod))
                 #t)
                (else
                 ;; each sub in the prod must have length at least one.
                 (<= (length item.prod) (- end start)))))

        (define (walk d nt start end visited hard-start?)
          ;; (indentf d "walk  s=" (int start) " e=" (int end)
          ;;          " " (prod-repr nt) " hard? " (bool hard-start?) "\n")
          (makegen emit
            (for-list item all[end]
               (when (not (member-eq? item visited))
                 (let ((estart (earliest-start item)))
                   ;; (indentf d "try? " (state-repr item) "\n")
                   (when (and (prod=? item.nt nt)
                              (candidate? item start end hard-start?))
                     ;; this is a candidate production. we want to generate all
                     ;; possible parses from it.
                     (for parse (walk-rule d (reverse item.prod) item.start end (list:cons item visited))
                       (match parse with
                         (:tuple start0 end0 plist)
                         -> (let ((parse (parse:nt (prod->name nt) (reverse plist))))
                              ;; (indentf "=> s=" (int start0)
                              ;;          " e=" (int end0) " " (parse-repr parse) "\n")
                              (emit (:tuple start0 end0 parse)))
                         )))))
               )))

        ;; (printf "|all| = " (int (vector-length all)) "\n")
        ;; (for-range i (vector-length all)
        ;;   (printf "i " (bold (int i)) "\n")
        ;;   (for-list state all[i]
        ;;      (printf "  " (state-repr state) "\n")))

        (makegen emit
          (for parse (walk 0 top-prod 0 (- (vector-length all) 1) '() #t)
            (match parse with
              (:tuple start end parse)
              -> (begin
                   (if (= 0 start)
                       (emit parse)
                       (printf "rejecting " (parse-repr parse) "\n"))
                   )
              )))
        ))

    (for tok tokgen
      (step tok)
      (toks.append tok)
      (set! k (+ 1 k)))
    (let ((gen (build-parse-tree)))
      ;; return the first parse we find.
      (match (gen) with
        (maybe:yes (parse:nt 'top (parse EOF))) -> parse
        (maybe:yes weird) -> (raise (:NoParse NULTOK))
        (maybe:no) -> (raise (:NoParse NULTOK))
        ))
    ))

(define (sexp->grammar exp)

  ;; --------------------------------------------------
  ;; (grammar
  ;;   (e (e ADD t) t)
  ;;   (t (t MUL p) p)
  ;;   (p IDENT))

  (define p-term
    (sexp:symbol name)
    -> (if (upper? (string-ref (symbol->string name) 0))
           (prod:t name)
           (prod:nt name))
    exp -> (raise (:Parser/Error "p-term" exp))
    )

  (define p-alt
    (sexp:list terms)  -> (map p-term terms)
    (sexp:symbol term) -> (LIST (p-term (sexp:symbol term)))
    exp                -> (raise (:Parser/Error "p-alt" exp))
    )

  (define p-rule
    (sexp:list ((sexp:symbol nt) . alts))
    -> (:tuple nt (map p-alt alts))
    exp -> (raise (:Parser/Error "p-rule" exp))
    )

  (define p-grammar
    (sexp:list ((sexp:symbol 'grammar) . rules))
    -> (let ((rules0 (map p-rule rules)))
         (foldr (lambda (rule acc)
                  (match rule with
                    (:tuple name rule)
                    -> (alist:entry name rule acc)))
                (alist:nil)
                rules0))
    exp -> (raise (:Parser/Error "p-grammar" exp))
    )

  (p-grammar exp)
  )
