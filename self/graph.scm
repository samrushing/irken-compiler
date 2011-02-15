;; -*- Mode: Irken -*-

(include "lib/set2.scm")
(include "lib/alist2.scm")

(define (build-dependency-graph root)
  (let ((g (alist-maker)))
    (define (search exp current-fun)
      (match exp.t with
	(node:varref name) -> (current-fun::add name)
	(node:varset name) -> (current-fun::add name)
	(node:fix names) -> (begin
			      (for-range
				  i (length names)
				  (let ((name (nth names i))
					(init (nth exp.subs i))
					(fun (set-maker '())))
				    (g::add name fun)
				    (search init fun)))
			      (search (nth exp.subs (length names)) current-fun))
	_ -> (for-each (lambda (sub) (search sub current-fun)) exp.subs)))
    (let ((top (set-maker '())))
      (g::add 'top top)
      (search root top))
    (newline)
    (print-string "about to return from build:")
    (print-graph g)
    g))

(define (transpose g)
  (let ((gt (alist-maker)))
    (g::iterate
     (lambda (k _)
       (gt::add k (set-maker '()))))
    (g::iterate
     (lambda (k vl)
       (for-each
	(lambda (v)
	  (match (gt::get v) with
	    (maybe:no) -> (gt::add v (set-maker (LIST k)))
	    (maybe:yes s) -> (s::add k)))
	(vl::get))))
    gt))

(define (print-graph g)
  (print-string "graph = {\n")
  (g::iterate
   (lambda (k v)
     (print-string "  ")
     (print k)
     (print-string " ")
     (printn (v::get))))
  (print-string "}\n"))

;; http://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
;;
;; Finds the strongly-connected components of the graph.  We need this to find
;; out how a pedantic programmer might have grouped a set of functions carefully
;; into letrecs, so that we can isolate such groups - otherwise they're all typed
;; together as a single letrec.  That causes polymorphic instantiation to fail in
;; many cases, because HM disallows polymorphism in recursive functions.  [yes,
;; it's hard to explain]

;; See 6.2.8 in Peyton-Jones:
;; http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/

(define (strongly g)
  (let ((s '())
	(visited (set-maker '())))
    (define (visit0 u)
      (visited::add u)
      (match (g::get u) with
	(maybe:no)     -> #u
	(maybe:yes vl) -> (vl::iterate
			   (lambda (v)
			     (if (not (visited::in v))
				 (visit0 v)))))
      (PUSH s u))
    ;; walk the graph forward, pushing finished nodes onto <s>
    (g::iterate
     (lambda (u v)
       (if (not (visited::in u))
	   (visit0 u))))
    (let ((gt (transpose g))
	  (visited (set-maker '()))
	  (r0 '())
	  (r1 (set-maker '())))
      (define (visit1 u)
	(visited::add u)
	(match (gt::get u) with
	  (maybe:no) -> #u
	  (maybe:yes vl) -> (vl::iterate
			     (lambda (v)
			       (if (not (visited::in v))
				   (visit1 v)))))
	(r1::add u))
      ;; walk backward, popping strongly connected components off <s>
      (while
       (not (null? s))
       (let ((u (pop s)))
	 (if (not (visited::in u))
	     (begin
	       (set! r1 (set-maker '()))
	       (visit1 u)
	       (PUSH r0 (r1::get))))))
      ;; the subcomponents are in topological order
      r0)))

(define (partition-fix names scc-graph)
  ;; partition the functions of this fix into sets of mutually-recursive functions
  (let ((n (length names))
	(name-map (alist-maker))
	(leftover (range n))
	(parts '())
	(part '()))
    (for-range i n (name-map::add (nth names i) {index=i done=#f}))
    ;; XXX consider making scc-graph a map, since this iterates over the
    ;;   whole graph for every fix.
    (for-each
     (lambda (component)
       (cond ((> (length part) 0)
	      (PUSH parts part)
	      (set! part '())))
       (for-each
	(lambda (name)
	  (match (name-map::get name) with
	    (maybe:no) -> #u
	    (maybe:yes val) -> (if (not val.done)
				   (begin
				     (PUSH part val.index)
				     (set! val.done #t)
				     (remove-eq! val.index leftover)))))
	component))
     scc-graph)
    (if (> (length part) 0)
	(PUSH parts part))
    ;; the leftovers should all be non-functions
    (if (> (length leftover) 0)
	(PUSH parts leftover))
    ;; partitioned!
    (reverse parts)
    ))

(define (reorder-fix names inits scc-graph)
  (let ((partition (partition-fix names scc-graph))
	(n (length names))
	(names0 '())
	(inits0 '())
	(r '())
	(i 0)
	)
    (print-string "reorder-fix: partition= ") (printn partition)
    (for-each
     (lambda (part)
       (let ((r0 '()))
	 (for-each
	  (lambda (j)
	    (PUSH names0 (nth names j))
	    (PUSH inits0 (nth inits j))
	    (PUSH r0 i)
	    (set! i (+ i 1)))
	  part)
	 (PUSH r (reverse r0))
	 ))
     partition)
    (:reordered (reverse names0) (reverse inits0) (nth inits n) (reverse r))))
     
     
	    
	  
	  
