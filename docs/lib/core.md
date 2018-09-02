
The Core Library
----------------

The 'core' (in `lib/core.scm`) contains the lowest-level primitives, a
sort of minimal set of features needed by any program.

print
-----

type: `('a -> undefined)`

This is the low-level runtime printer (written in C).  It can print
any value, though it knows nothing of datatypes outside the primitive
types, lists, closures, etc.

`print` writes to `stdout`.

(printn x)
------
type: `('a -> int)`

Just like `print`, but adds a newline character at the end.

(newline)
---------
type: `(-> int)`

prints a newline to `stdout`.


(print-string s)
--------------
type: `(string -> undefined)`

prints the contents of `s` to `stdout`.

(flush)
-----
type: `(-> int)`

flushes `stdout`.

(= a b)
-------
type: `(int int -> bool)`

are `a` and `b` equal?

(zero? a)
---------
type: `(int -> bool)`

Is `a` zero?

(< a b)
-------
type `(int int -> bool)`

Is `a` less than `b`?

(> a b)
-------
type `(int int -> bool)`

Is `a` greater than `b`?

(>= a b)
--------
type `(int int -> bool)`

Is `a` greater than or equal to `b`?

(<= a b)
--------
type `(int int -> bool)`

Is `a` less than or equal to `b`?


(>0 a)
------
type: `(int -> bool)`

Is `a` greater than zero?

(<0 a)
------
type: `(int -> bool)`

Is `a` less than zero?

(binary+ a b)
-------------
type: `(int int -> int)`

Add `a` and `b`.

(binary- a b)
-------------
type: `(int int -> int)`

Subtract `a` and `b`.

(binary* a b)
-------------
type: `(int int -> int)`

multiply `a` and `b`.

(/ a b)
-------
type: `(int int -> int)`

divide `a` and `b`.

(+ a ...) [macro]
---------
```scheme
(defmacro +
  (+ x)       -> x
  (+ a b ...) -> (binary+ a (+ b ...)))
```

(- a ...) [macro]
-----------------
```scheme
(defmacro -
  (- a)         -> (binary- 0 a)
  (- a b)       -> (binary- a b)
  (- a b c ...) -> (- (binary- a b) c ...))
```

(* a ...) [macro]
-----------------
```scheme
(defmacro *
  (* x) -> x
  (* a b ...) -> (binary* a (* b ...)))
```

(/ a b)
-------
type: `(int int -> int)`

Divide `a` by `b`.

(unsafe-div a b)
----------------
type: `(int int -> int)`

Divide `a` by `b`, without checking for divide by zero.

(mod a b)
---------
type: `(int int -> int)`

`a` modulo `b`.  This uses the C definition of mod (`a % b`),
and will likely by fixed in the future.

(rem a b)
---------
An alias for mod.  [To be fixed].

(divmod a b)
------------
type: `(int int -> (:tuple int int))`

Divide `a` and `b`, returning the quotient and the remainder.

(<< a b)
--------
type: `(int int -> int)`

left-shift `a` by `b` bits.

(>> a b)
--------
type: `(int int -> int)`

right-shift `a` by `b` bits.

(bit-get n i)
-------------
type: `(int int -> int)`

fetch thing `i`th bit from `n` as an integer.

(bit-set n i)
-------------
type: `(int int -> int)`

set the `i`th bit in `n`.

(logior a b)
------------
type: `(int int -> int)`

logical 'inclusive or' of `a` and `b`. [in C, `a|b`].

(logior* a ...) [macro]
-----------------------
```scheme
(defmacro logior*
  (logior* x)       -> x
  (logior* a b ...) -> (logior a (logior* b ...))
  )
```

(logxor a b)
------------
type: `(int int -> int)`

logical 'exclusive or' of `a` and `b`. [in C, `a^b`].

(logxor* a ...) [macro]
-----------------------
```scheme
(defmacro logxor*
  (logxor* x)       -> x
  (logxor* a b ...) -> (logxor a (logxor* b ...))
  )
```

(logand a b)
------------
type: `(int int -> int)`

logical 'and' of `a` and `b`. [in C, `a&b`]

(lognot a)
------------
type: `(int -> int)`

logical 'not' of `a` and `b`. [in C, `~a`]

(popcount n)
------------
type: `(int -> int)`

returns the number of bits set in `n`.

(pow x n)
---------
type: `(int int -> int)`

raise `x` to the `n`th power.

(odd? a)
--------
type: `(int -> bool)`

Is `a` an odd number?

(even? a)
--------
type: `(int -> bool)`

Is `a` an even number?


(min a b)
---------
type: `(int int -> bool)`

return the smaller of `a` and `b`.

(max a b)
---------
type: `(int int -> bool)`

return the larger of `a` and `b`.

(abs a)
-------
type: `(int -> int)`

return the absolute value of `a`.

(how-many x n)
--------------
type: `(int int -> int)`

returns the minimum number of groups of size `n` to hold `x` items.
```scheme
(how-many 10 3)
=> 4
```

cmp [datatype]
--------------
```scheme
(datatype cmp
  (:<)
  (:=)
  (:>)
  )
```

## (cmp-repr c)
type: `(cmp -> string)`

## (int-cmp a b)
type: `(int int -> cmp)`

compare `a` and `b`.

## (magic-cmp a b)
type: `('a 'a -> cmp)`

provides a lexicographic ordering on all Irken objects.
This comparison is implemented in C by the runtime.
The ordering is roughly:

  * immediates < tuples.
  * symbols are compared by index.
  * tuples are compared recursively, in lexicograph order.

## (magic<? a b)
type: `('a 'a -> bool)`

```scheme
(eq? (cmp:<) (magic-cmp a b))
```

## (magic=? a b)
type `('a 'a -> bool)`

```scheme
(eq? (cmp:=) (magic-cmp a b))
```

## (not x)
type: `(bool -> bool)`

```scheme
(eq? x #f)
```

## (char=? a b)
type: `(char char -> bool)`

## (int->char n)
type: `(int -> char)`

## (char->int n)
type: `(char -> int)`

ASCII conversion.
The special character `#\eof` has value `256`.

## char->ascii [alias for char->int]
## ascii->char [alias for int->char]

## (make-vector n val)
type: `(int 'a -> (vector 'a))`

Create a new vector of size `n` containing `val`.

## (copy-vector v)
type: `((vector 'a) -> (vector 'a))`

## (vector-length v)
type: `((vector 'a) -> int)`

## (make-array (n ...) v) [macro]

create a multi-dimensional vector of vectors.
```scheme
(make-array (2 3) 0)
=> #(#(0 0 0) #(0 0 0))
```

## (error x)
type: `('a -> undefined)`

Print an error with the value `x` and exit the program.

## (error1 msg x)
type: `(string 'a -> undefined)`

Print an error message with the value `x` and exit.

## (error2 msg ob0 ob1)
type: `(string 'a 'b -> undefined)`

Print an error message and two objects, then exit.

## (impossible)
type: `(-> undefined)`

Used in pattern matches (or other code) to denote an impossible condition.
Exits with an error.

## (assert exp)
type: `(bool -> undefined)`

Assert an expression evaluates to `#t`, otherwise exit with error.

## (id a)
type: `('a -> 'a)`

'identity' - returns its argument.

## (discard x)
type: `('a -> undefined)`

discard the argument, return `#u`.

## (getcc) [macro]
type: `(-> (continuation 'a))`

Capture the current continuation.

## (putcc k r) [macro]
type: ((continuation 'a) 'a -> 'b)

Replace the current continuation.

## (callcc p)
type: `(((continuation 'a) -> 'a) -> 'a)`

Call the procedure `p` with the current continuation as its argument.

## (throw k v)
type: `((continuation 'a) 'a -> 'b)`

Replace the current continuation.

## let/cc [macro]
```scheme
(defmacro let/cc
  (let/cc name body ...)
  -> (callcc
      (lambda ($k)
	(let ((name (lambda ($val)
		      (throw $k $val))))
	  body ...))))
```

Usually used for non-local exit.  For example:

```scheme
(let/cc return
  (for-list item items
    ...
    (return item)
    ...))
```

## bool [datatype]

`bool` has only two constructors/values:
`(bool:true)` is another name for `#t`.
`(bool:false)` is another name for `#f`.

## maybe [datatype]

```scheme
(datatype maybe
  (:yes 'a)
  (:no)
  )
```

This is an [Option Type](https://en.wikipedia.org/wiki/Option_type).

## (maybe? m)
type: `((maybe 'a) -> bool)`

Is this a `maybe:yes`?

## (magic x)
type: `('a -> 'b)`

Dangerous. Cast/convert any type to any other type.  Don't use this
unless you really know what you're doing.  Then don't use it anyway.

## (dump file thunk)
type: `(string (continuation int) -> int)`

Dump a continuation into the file `file`.
Currently only supported with the C backend.

## (load file)
type: `(string -> (continuation int))`

Currently only supported with the C backend.
Load a continuation (previously dumped with `dump`).

Note: the dump/load facility may require disabling ASLR.
See the notes in `lib/core.scm` for more detail.

## (make-generator producer)
type: `((((maybe 'a) -> undefined) -> undefined) -> ( -> (maybe 'a)))`

Create a low-level generator function.
Don't use this.  Use the `makegen` macro instaed.

Generators use a 'protocol': the resulting function should return
`maybe:yes` objects of the result type.  When the generator has
finished, it forever returns `(maybe:no)`.  Creating a generator that
violates this protocol will lead to very, _very_ confusing outcomes.

## makegen [macro]

Create a new generator.  Example:

```scheme
(define (list-generator xs)
  (makegen emit
    (for-list x xs
      (emit x))))
```

This will generate all the items in a list.

Use `for` to iterate over a generator:

```scheme
(for item item-gen
  ...
  )
```

## (counting-gen gen) [generator]

Transforms a generator into one that generates `(:tuple index item)`,
starting the index at zero.

## (notify-last-gen gen) [generator]

Transforms a generator into one that generates `(:tuple last? item)`.

## (raise exn) [macro]

Raise an exception.  Exceptions are polymorphic variants, and must be monomorphic.
[i.e., every use of a particular exception must match the type of every other use].

Example:

```scheme
(raise (:ForbiddenFruit 'apple))
```

## (try <body ...> except <exception-patterns>) [macro]

Exception handling.  Example:

```scheme
(try
 (do-something)
 except
 (:DivideByZero _ _)
  -> (printf "why would you do that?\n")
 (:BeamsCrossed)
 -> (printf "do not cross the beams!\n")
  )
```

## (set-verbose-gc b)
type: `(bool -> bool)`

Turn verbose GC on or off.

## (get-word-size)
type: `(-> int)`

Returns the size of a machine word, in bytes.
e.g. on a 64-bit platform, returns 8.

## (get-int-size)
type: `(-> int)`

Returns the size of a C int.

## (get-metadata)
type: `(-> sexp)`

Return an s-expression containing metadata about this program.
Includes information about datatypes, tag values, etc.

## (read-cycle-counter)
type: `(-> int)`

Read the cycle counter, if available.
Note: not yet available on the VM.

## sys [record]
type: `{argv=(vector string) argc=int}`

A record containing the programs arguments upon invocation.
Note: when running under the VM, `argv[0]` is silently removed
so that the bytecode file appears as `argv[0]`.
