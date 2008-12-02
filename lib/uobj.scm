;; -*- Mode: Scheme -*-

;; need a simple user-object scheme
;; would be nice to avoid dynamic method lookup.

;; how can we do compile-time lookup?

;; (tree.lookup t k)
;; it'd be really really nice if we could just say
;; (t.lookup k), but that will require some kind of typing system.
;; so the question is - do we
;;  1) add manual typing
;;  2) add type inferencing
;;  3) just use dynamic dispatch
;;
;; pausing on this, I don't think I'm ready to tackle
;;   macros yet, and I really want them for this.
;; Also, should take a fresh look at all the OO systems
;;  written for scheme (why is there no SRFI?)...

;; let's just do the generic lisp/scheme thing,
;;  define a new low-level uobj type.
;; then have a define-method macro that automatically
;;  adds the necessary type check to the front.
;; this way we can keep known functions, and still
;;  optionally remove the type-checks at compile time.

;;

(define-class (tree < root)
  ((ivar0 init0)
   (ivar1 init1) ...)
  (define-method (lookup key)
    ...)
  (define-method (delete key)
    ...)
  )

;; (define-method (<name> <arg0> ...) <body>)
;; =>
;; (define (<class>.<name> <self> <arg0> ...)
;;   (verify-uobj 

   

;; by using a closure we can capture all kindsa neat
;; stuff.  but might we also capture things we don't want?
;; several half-oo things I did used vectors.
;; what might the uobj look like?
;; <tag> <class> <closure>
;; where <class>
;; is a self-referential uobj.
;;
;; how do you talk to a class?
;; I think you gotta send it a message
;; maybe better would be
;; <tag> <closure>
;; where (object 'class)
;; will give up the class
;; 
;; this could be made more compact...
;; how about a different closure type?
;; ... some way of identifying closures
;; that will accept messages?
;;
;; how about some sugar?
;; (ob.method arg0 arg1 ...)
;;    => ((ob 'method) arg0 arg1 ...)
;;
;; could this later be replaced with a compile-time
;;  system that avoided the lookup?
;;
;; ok, let's think about that
;;   we *could* build a compile-time lookup table
;;     that translated into vector-ref insns
;; so a class might consist of a vector of methods, or hell all slots?
;; so associated with the class (at compile-time) is a list of symbols
;; and each is automatically translated at get/set into the correct
;; vector code.  to get type safety we use a new uobj type and new get/set
;; primitives for it.
;; would variables need to have their type declared somehow...
;; obviously not in the class itself?
;; but users of the class... maybe like this:
;;  (let ((table:tree (make-table ...))) ...)
;;  [maybe it would be easier to just buckle down and do the type inference?]
;;
;; perhaps this mild form of voluntary typing would then be required wherever
;;  attribute access is used (in order for the compiler to know how to translate
;;  it into the correct tuple offset).
;;
;; hmm... wouldn't this allow us to then have known functions for methods?
;;   [rather than going through the vector-ref?]
;;   for example, (t.lookup key) where we know that <t> is a tree would let
;;   us call the method directly, but only if we pull it out of the closure
;;   and pass <self> as an arg, like this:
;;   (tree-lookup t key)
;;   This is an interesting problem - closures are cool, but I don't see any
;;    obvious way to use them at compile time to do the right thing...
;;
;; if we think about adding typing, we need to consider simplifying the
;; binding constructs.  Right now we have lambda, let_splat, and fix.
;; we could probably collapse let_splat and fix somehow... perhaps by
;; making 'fix' a flag?

;; thinking about using closures.  maybe we can use a couple of dirty hacks
;;   and get there.  maybe have a 'dispatcher' flag attached to a function,
;;   such that the compiler knows how to access closures within its immediate
;;   lexical contour.  hmm... but this would require data flow to follow the
;;   returned lambda.  Instead, just have the *class* (a known function) be
;;   the referenced object.  Then, attribute references on the *class* will
;;   give up the known method.  Nice!
;;
;; (define-class class (arg0 arg1 ...)
;;   (define (method0 ...) )
;;   (define (method1 ...) )
;;   )
;;
;; (class.method0 ob arg0 arg1 ...)
;;
;; [hmmm... looks like there's no way to do class/static methods. matters?]
;; so as long as <class> is in scope, then we'll catch the 'getattr' on it
;; and generate code to fetch the closure out of <ob>'s environment.  drat,
;; again we run into the typing problem.  how do we guarantee that it's the
;; right kind of closure?  oh, the humanity!  aight.  So we need to manually
;; declare its type.  Big whoop.  Now a call looks like this:
;;
;; (define (my-fun t:tree)
;;    (let ((probe (t.lookup 5)))
;;       ...))
;;
;; ok, current plan:
;;    (1) investigate a simple manual typing syntax
;;    (2) implement the above described closure-based OO
;;     
