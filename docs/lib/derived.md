
# Derived Expressions

These are mostly 'special forms' implemented with `defmacro`.

Some of them are standard Scheme/Lisp forms, others are unique to Irken.

## and
```scheme
(defmacro and
  (and)                 -> #t
  (and test)            -> test
  (and test1 test2 ...) -> (if test1 (and test2 ...) #f)
  )
```

## or
Note: this is _not_ the same as Scheme's `or`, this returns a boolean.

```scheme
(defmacro or
  (or)			-> #f
  (or test)		-> test
  (or test1 test2 ...)	-> (if test1 #t (or test2 ...))
  )
```

## let

There's only one `let` in Irken.  It corresponds to Scheme's `let*`.

    (let ((variable init) ...) expression expression ...)

This is the basic form of `let`.  It binds a new `variable` with the
value in `init`, and then processes `expression ...` in that new
environment.

Irken's let bindings are sequential, so the following code:

```scheme
(let ((v0 0)
      (v1 (+ v0 1)))
  (printf "v0=" (int v0) " v1=" (int v1) "\n")
 )
```

prints `v0=0 v1=1`.

## let loop

This is a general looping/recursion mechanism.  It introduces a new
recursive function along with bindings for its arguments, and then
calls that function.  You can distinguish it from 'normal let' by
the extra inserted name, after `let`.  It's often just `loop`.

It's easiest to explain with an example, which will count from 0 to
1000, and return "done":

```scheme
(let loop ((n 0))
  (if (= n 1000)
      "done"
      (loop (+ n 1))))
```

This creates a new recursive function `fact`, and calls it with the initial arguments `1` and `5`:

```scheme
(let fact ((acc 1) (n 5))
  (if (= n 0)
      acc
      (fact (* n acc) (- n 1))))
```

Note: this is more cleanly written with pattern matching:

```scheme
(define fact
  acc 0 -> acc
  acc n -> (fact (* acc n) (- n 1))
)
```

## cond

Generic conditional.

```scheme
(cond (test0 exp ...)
      (test1 exp ...)
      ...
      (else exp ...)
      )
```

`cond` is turned into a nested chain of `if` forms.  It's mostly
useful to clean up the look of embedded `if` statements that need to
use `begin`.

## inc!

increment something.
This can be used where `set!` can: variables, array elements, and record fields.

```scheme
(defmacro inc!
  (inc! n)   -> (set! n (+ n 1))
  (inc! n m) -> (set! n (+ n m))
  )
```

## dec!

decrement something.
This can be used where `set!` can: variables, array elements, and record fields.

```scheme
(defmacro dec!
  (dec! n)   -> (set! n (- n 1))
  (dec! n m) -> (set! n (- n m))
  )
```

## while

while loop.

```scheme
(defmacro while
  (while test body ...)
  -> (let $loop ()
       (if test
	   (begin body ... ($loop))
	   #u)))
```

Example:

```scheme
(while (thing-not-done?)
  (do-more-stuff))
```

## when

This construct is preferred over `if` with only a 'true' branch,
done for side-effect.

```scheme
(defmacro when
  (when test body ...)
  -> (if test (begin body ...)))
```

Example:

```scheme
(when (< n 0)
  (printf "n is negative!\n"))
```

## when-maybe

```scheme
(defmacro when-maybe
  (when-maybe var mob
    body ...)
  -> (match mob with
       (maybe:yes var) -> (begin body ...)
       (maybe:no) -> #u
       ))
```

Example:

```scheme
(when-maybe val (alist/lookup my-alist key)
  (printf "key present, val = " (int val) "\n"))
```

## if-maybe

```scheme
(defmacro if-maybe
  (if-maybe var mob then else)
  -> (match mob with
       (maybe:yes var) -> then
       (maybe:no) -> else
       ))
```

Example:

```scheme
(+ 23 (if-maybe val (alist/lookup int-list key) 0))
```

## while-maybe

```scheme
(defmacro while-maybe
  (while-maybe var mob
    body ...)
  -> (let $loop ()
       (match mob with
         (maybe:yes var) -> (begin body ... ($loop))
         (maybe:no) -> #u
         )))
```

## for-range*

Iterate from `[lo, hi)`.

```scheme
(defmacro for-range*
  (for-range* vname lo hi body ...)
  -> (let $loop ((vname lo)
                 ($stop hi))
       (if (= vname $stop)
           #u
           (begin
             body ...
             ($loop (+ vname 1) $stop)))))
```

Example:

```scheme
(for-range i 10 15
  (printf (int i) " "))
(printf "\n")
=> 10 11 12 13 14
```

## for-range

Iterate over the range `[0, num)`.

```scheme
(for-range i 5
  (printf (int i) " "))
(printf "\n")
=> 0 1 2 3 4
```

## for-range-rev*

Like `for-range*`, but in reverse. `(hi ... lo]`.

## for-range-rev

Like `for-range`, but in reverse. `(hi ... 0]`.

## for-vector

```scheme
(defmacro for-vector
  (for-vector vname vec body ...)
  -> (let (($v vec)) ;; avoid duplicating <vec> expression.
       (for-range $i (vector-length $v)
	 (let ((vname $v[$i]))
	   body ...))))
```

Example:

```scheme
(define (vector->list vec)
  (let ((r '()))
    (for-vector item vec
      (push! r item))
    (reverse r)))
```

## for-vector-rev

Like `for-vector`, but in reverse.

## forever

Perform `body ...` forever.

```scheme
(defmacro forever
  (forever body ...)
  -> (let $loop () body ... ($loop)))
```

## for

Iterate over a generator.

Example:

```scheme
(define (even-gen)
  (let loop ((n 0))
    (makegen emit
      (emit n))
    (loop (+ n 2))))

(for val (even-gen)
  (printf (int val) "\n"))

=> 0
   2
   4
   6
   ...
```

## pipe

Collapse a chain of function calls into a 'pipe'.

```scheme
(defmacro pipe
  (pipe a)       -> a
  (pipe a b)     -> (a b)
  (pipe a b ...) -> (a (pipe b ...))
  )
```

Example:

```scheme
(pipe emit html-ize parse lex input)
=>
(emit (html-ize (parse (lex input))))
```

