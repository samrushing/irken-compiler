;; -*- Mode: Irken -*-

;; this is tricky: I prefer a datatype that is incapable of
;;  representing an invalid charset.  Can this be done with
;;  a range-based representation?  re2 uses a "balanced binary
;;  tree of ranges" in order to handle unicode.  Can we represent
;;  a canonical charset like this?  I think a sorted list of ints
;;  (i.e., a set<int>) could do the trick.
;; so (A Z a z) would work.
;; but would (A Z a) have any meaning?  could it mean A-Za-255?
;; what would (A) mean?  A-255?

;; how about this: a map where the key represents the starting
;;  point and the value represents either the end of the range
;;  or the number of chars in the range.
;; we can iterate over the charset easily, and do only the work
;;  needed rather than a bunch of bit-twiddling.
;; it's still possible to have an invalid charset (with ranges
;;  that overlap) but I think this is a good compromise.

;; hi is not inclusive. this simplifies merging.

(typealias char-range {lo=int hi=int})
(typealias charset (list char-range))

;; Note: as written, this assumes 8-bit characters.  Supporting wider
;;  charsets should be trivial (simply replace the occurrences of "256" below).

;; XXX make a separate list of chars that need backslash-escaping.
(define printable-chars
  (string-concat
   '("0123456789"
     "abcdefghijklmnopqrstuvwxyz"
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
     "!\"#$%&'()*+,-./:;<=>?@[]^_`{|}~ ")))

;; XXX why is this not a vector[256]?  was I thinking of unicode?
(define printable-map
  (let ((chars (string->list printable-chars))
        (map (fold
              (lambda (ch t) (tree/insert t int-cmp (char->ascii ch) (char->string ch)))
              (tree:empty)
              chars)))
    (tree/insert! map int-cmp (char->int #\newline) "\\n")
    (tree/insert! map int-cmp (char->int #\return) "\\r")
    (tree/insert! map int-cmp (char->int #\tab) "\\t")
    (tree/insert! map int-cmp (char->int #\\) "\\\\")
    map))

(define (char-repr ch)
  (match (tree/member printable-map int-cmp ch) with
    (maybe:yes ch0) -> ch0
    (maybe:no)      -> (format "<" (zpad 2 (hex ch)) ">")
    ))

(define (charset/size s)
  (let ((size 0))
    (for-list r s
      (set! size (+ size (- r.hi r.lo))))
    size))

(define (charset-repr* s)
  (let ((ranges '()))
    (for-list r s
      (cond ((= r.hi (+ 1 r.lo))
	     (PUSH ranges (char-repr r.lo)))
	    ((= r.hi (+ 2 r.lo))
	     (PUSH ranges (char-repr (+ 1 r.lo)))
	     (PUSH ranges (char-repr r.lo)))
	    (else
	     (PUSH ranges (format (char-repr r.lo) "-" (char-repr (- r.hi 1)))))))
    (string-concat (reverse ranges))
    ))

(define (charset-repr s)
  (let ((size (charset/size s)))
    (cond ((= size 256) "•") ;; easily distinguishable from '.'
	  ((= size 1) (let ((s0 (car s))) (char-repr s0.lo)))
	  ((> size 128)
	   (format "[^" (charset-repr* (charset/invert s)) "]"))
	  (else
	   (format "[" (charset-repr* s) "]")))))

(define (charset-repr-raw s)
  (let ((r '()))
    (for-list x s
      (PUSH r (format (zpad 2 (hex x.lo)) "-" (zpad 2 (hex x.hi)))))
    (format "{" (join "," (reverse r)) "}")))

(define (charset->sexp* s)
  (let ((r '()))
    (for-list x s
      (cond ((= x.lo (- x.hi 1)) ;; single char
             (PUSH r (sexp:char (int->char x.lo))))
            ((and (printable? (int->char x.lo)) (printable? (int->char (- x.hi 1))))
             (PUSH r (sexp (char (int->char x.lo)) (char (int->char (- x.hi 1))))))
            (else
             (PUSH r (sexp (int x.lo) (int x.hi))))))
    (sexp1 'set (reverse r))
    ))

(define (charset->sexp s)
  (let ((size (charset/size s)))
    (cond ((= size 256)
           (sexp (sym 'set) (sym 'dot)))
          ((> size 128)
           (sexp (sym 'not) (charset->sexp* (charset/invert s))))
          (else
           (charset->sexp* s)))))

;; note: this is not meant for overlap detection, it is
;;  for maintaining sets/maps of charsets.
(define charset-cmp magic-cmp)

(define (charset/empty)
  (list:nil))

(define charset/empty?
  () -> #t
  _  -> #f
  )

(define (charset/single n)
  (LIST {lo=n hi=(+ n 1)}))

(define (charset/range lo hi)
  (assert (< lo hi))
  (LIST {lo=lo hi=hi}))

(define charset/dot (charset/range 0 256))

;;    -----XXX-----XXXX-----XXX----
;; a)  ch
;; b)      ch
;; c)          ch

;; XXX this could be smarter
(define (charset/in* s ch)
  (let/cc return
    (for-list r s
      (cond ((< ch r.lo) (return #f)) ;; a
	    ((<= r.hi ch) #f)	      ;; c [note: not a return]
	    (else (return #t))))      ;; b
    (return #f)))

(define (charset/in? set ch)
  (charset/in* set (char->ascii ch)))

;; cases                       merging              overlap
;; a) -----AAAAAA--------- alo <= blo <= ahi	alo <= blo < ahi
;;    ---------BBBBBB-----
;; b) -----AAAAAA--------- alo <= bhi <= ahi	alo < bhi <= ahi
;;    ----BBBBB-----------
;; c) -----AAAAAA--------- ahi < blo		ahi < blo
;;    ------------BBBB----
;; d) -----AAAAAA--------- bhi < alo		bhi < alo
;;    -BBB----------------

;; Note: for merging, only a & b imply overlap. so we need only
;;   test for c & d.
(define (cmp-range0 alo ahi blo bhi)
  (cond ((< ahi blo) (cmp:<)) ;; case c
	((< bhi alo) (cmp:>)) ;; case d
	(else (cmp:=))))

;; this version is used for merging
(define (cmp-range a b)
  (cmp-range0 a.lo a.hi b.lo b.hi))

;; this version is used for overlap detection
;; [note: the hi end of the range is open, so
;;  we need to decrement it to detect overlap]

(define (cmp-range-overlap a b)
  (cmp-range0 a.lo (- a.hi 1) b.lo (- b.hi 1)))

;; Note: this is O(#ranges), but I bet it's possible
;;  to do better using their sorted nature. [probably by adding
;;  matches like this: (ra) (rb . tlb) where ahi < blo

(define (charset/overlap? a b)
  (match a b with
    () _ -> #f
    _ () -> #f
    (ra . tla) (rb . tlb)
    -> (match (cmp-range-overlap ra rb) with
	 (cmp:=) -> #t
	 (cmp:<) -> (charset/overlap? tla b)
	 (cmp:>) -> (charset/overlap? a tlb)
	 )
    ))

;; merge two charsets.
;; merge both charsets into a single map, then iterate through
;;  the result.  Collect overlapping sets into an accumulator,
;;  which is flushed whenever it fails to overlap with the set
;;  in the front.

(define (charset/merge a b) : (charset charset -> charset)

  ;; merge-sort two range lists
  (define merge
    () lb -> lb
    la () -> la
    (ha . ta) (hb . tb)
    -> (if (< ha.lo hb.lo)
	   (list:cons ha (merge ta (list:cons hb tb)))
	   (list:cons hb (merge (list:cons ha ta) tb))))

  ;; we know these ranges overlap, return a new merged range.
  (define (range/merge ra rb)
    {lo=(min ra.lo rb.lo) hi=(max ra.hi rb.hi)})

  (let ((m (merge a b)))
    (if (null? m)
	(charset/empty)
    	(let loop ((acc (car m))
    		   (l (cdr m))
    		   (r (list:nil)))
	  (match l with
	    () -> (reverse (list:cons acc r))
	    (hd . tl)
	    -> (match (cmp-range acc hd) with
	  	 (cmp:=) -> (loop (range/merge acc hd) tl r)
	  	       _ -> (loop hd tl (list:cons acc r))
		 )
	    )
	  ))))

;;    a-b c-d e-f
;; -> \00-a b-c d-e f-\FF
;; XXX rewrite this so 256 doesn't show up on the left
;;   side of a match? [so we can make it a global constant]
(define charset/invert
  ()              -> (LIST {lo=0 hi=256})
  ({lo=0 hi=256}) -> (list:nil)
  s -> (let loop ((end 0)
		  (r (list:nil))
		  (s s))
	 (match s with
	   ()                   -> (reverse (list:cons {lo=end hi=256} r))
	   ({lo=lo hi=256})     -> (reverse (list:cons {lo=end hi=lo} r))
	   ({lo=0  hi=hi} . tl) -> (loop hi r tl)
	   ({lo=lo hi=hi} . tl) -> (loop hi (list:cons {lo=end hi=lo} r) tl)
	   )))

(define (charset/intersection a b) : (charset charset -> charset)
  ;; De Morgan's law.
  (charset/invert (charset/merge (charset/invert a) (charset/invert b))))

(define (parse-charset s) : (string -> charset)
  (parse-charset0 (string->list s)))

(define (parse-charset0 s)
  (let ((s s)
	(r (charset/empty)))
    (match s with
      (#\^ . s) -> (charset/invert (parse-charset* s r))
      _         -> (parse-charset* s r)
      )))

(define (parse-charset* s r)
  (match s with
    ()             -> r
    (a #\- b . tl) -> (parse-charset*
		       tl (charset/merge
			   r (charset/range (char->ascii a)
					    (+ 1 (char->ascii b)))))
    (a . tl)       -> (parse-charset*
		       tl (charset/merge
			   r (charset/single (char->ascii a))))
    ;; backslashes? hex-escapes, ']', '[', etc..
    ;; [note: partially handled by rx.scm:find-and-parse-charset]
    ))
