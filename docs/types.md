
# Types (and Inference).

Unlike Scheme, Irken uses static types and type inference.  From the
perspective of typing, it is much closer to OCaml or SML than to
Scheme.  It does this for several reasons:

### Performance

The absence of runtime type checking makes Irken run fast, often
nearly as fast as similar code in C.  There's still some overhead from
the runtime system, but not nearly as much as that of a dynamically
typed language like Lisp, Python, Ruby, etc...

### Safety

If your program survives the compiler's type solver (i.e., it compiles
without a type error), it is likely to run correctly, often the very
first time.  It's not that Irken programs don't crash, it's that they
don't crash in the _way_ that C programs crash.  'segfaults' are rare
with Irken, virtually always caused by either a bug in the compiler or
an unsafe use of the Foreign Function Interface (calling out to C).

For example, languages like C, C++, and Java all use the idea of a
'NULL' pointer.  It's easy, even for a seasoned programmer, to forget
to check a pointer for NULL.

In Irken, there is no such beast: its equivalent is the `maybe` type.
It captures the idea that a computation may or may not return a
result.  Since Irken's pattern matching requires that every case is
handled, every time, it's impossible to forget to handle the case
where a return value is `(maybe:no)`.


### Clarity of Thought

This is hard to explain to someone unfamiliar with this kind of type
system.  Irken (and ML/OCaml/Haskell) programming encourages a
different approach to problem-solving.  Usually the first step is to
think about the _types_, rather than the _algorithms_.  After many
years of experience, I believe this is a 'better' way to program.  It
often results in better code, sooner than the alternative approach of
thinking about the algorithms first, and data structures last.

Once you understand the problem well enough to have the data structures
laid out, the algorithms usually fall into place.

I've been programming for over 35 years, and I still experience a bit
of shock and joy when a program works perfectly the first time I run
it.

## Type Inference

The compiler will examine a program and try to solve all the types in
the program, using the
[Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
algorithm.

One of the goals of Irken is to reduce the use of type annotations.  I
believe they are one of the major turn-offs of functional languages,
especially for newcomers.  So I want Irken to _look_ like Scheme (or
many other high level languages), but be something completely
different underneath.

Irken supports type annotations, and in some cases they are necessary
for type safety, but for the most part they can be left off.

Note: the compiler is capable of dumping all the types in a program if
you need them for documentation or understanding. (give the compiler
the `-types` option).

### A simple example.

Here's a factorial function:

```scheme
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))
      ))
```

When the compiler walks this function, it begins with the innermost
expressions.  For example, the `(= n 0)` expression contains lots of
usable type information already.  We know that `0` is an `int`.  We
know that `=` takes two `int`s as arguments and returns a `bool`.  We
combine this information to assert that `n` is also an integer.

The `if` statement requires a `bool` as its first argument.  So that part
checks out - our test returns a `bool`.

The 'then' clause of the `if` returns `1`, which is an `int`.  So now
we know that the value returned by the `if` statement must be an
`int`.  That tells us that the function itself returns an `int`.  At
this point, we know the full type signature for the function: it takes
an `int` argument and returns an `int`.

The 'else' clause follows similarly.

### The generated code.

What does this mean for the generated code?  It means that it never
needs to check the type of anything it touches.  When the `if` becomes
a machine-level branch, it does an exact integer comparison to the
known value of `#f` to decide wether to take the branch:

Here's what the C backend produces for that `if`:

```C

  O r60 = (object *) 1; // representation of '0'
  O r61 = ((object*) lenv) [4]; // fetch of 'n'
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r60)<=UNTAG_INTEGER(r61))) { // (= n 0)
    ...
  }
```

After optimizing by the C compiler, that test turns into:

```asm
        movq    _lenv(%rip), %rax # load 'n'
        cmpq    $1, 16(%rax)      # compare to '0'
        ja      LBB54_4           # branch
```

## The Type System.

Irken uses a small set of 'base types':

  * integers (signed, 63-bit on a 64-bit machine)
  * booleans
  * characters
  * strings
  * symbols (a singleton for each unique string).
  * undefined (one value only: `#u`).

From these are built some container types:

  * lists ('cons' cells)
  * vectors (an array of one type).
  * records (a collection of named fields)

And then types declared by `datatype`.  If you haven't already, now
would be a good time to read about [datatypes](datatypes.md).

### Syntax of Types

In Irken, each type has a name.  For example, `int` is the type for integers.
A list of integers would have type `(list int)`.  A list of lists of integers
would have type `(list (list int))`.

Some types are polymorphic - they can be instantiated in multiple versions with
other types.  For example, `list` has type `(list 'a)`, where `'a` stands for
any type.  `(list int)` is an 'instantiation' of the list type with `'a` = `int`.

Some types might be polymorphic across several types, for example
`map` has the type `(map 'a 'b)`, capturing the idea that when used it
combines with a key type `'a` and a value type `'b`.

For example a map from integers to strings would have the type `(map int string)`.

### Function Types

The type of a function looks like this: `(type-of-arg0 type-of-arg1 ... -> type-of-result)`.
So the `+` function has type `(int int -> int)`.
A function of no arguments returning a boolean would be `(-> bool)`.



### A polymorphic function

When the compiler solves for a type, it searches for the 'most general type' it
can.  In other words, it tries to leave polymorphism in place whenever it can.

For example, here's a `length` function for lists:

```scheme
(define length
  ()        -> 0
  (hd . tl) -> (+ 1 (length tl))
  )
```

Note that this function never actually touches the _values_ in the list.
[in this case, the variable `hd`].  The compiler gives this function a
type of `((list 'a) -> int)`.  In other words, it takes a list of any type
and returns an integer.

Here's a function that is _not_ polymorphic:

```scheme
(define sum-list
  ()        -> 0
  (hd . tl) -> (+ hd (sum-list tl))
  )
```

In this case, the compiler learns that `hd` must be an `int`, and
therefore requires that the argument be of type `(list int)`.  Thus the
type signature for this function is `((list int) -> int)`.

### Higher-Order Types

Irken functions can take other functions as arguments, and return functions.

Ok, hang on for this one.

```scheme
(define (call-with-frob fun a)
  (fun (+ 1 a)))
```

Here, `call-with-frob` takes another function as its first argument.
That function takes an integer argument (because of the `(+ 1 a)`) and
returns some unknown type.  In plain English, `call-with-frob` makes a
modified version of any other `(int -> 'a)` function that adds one to
its argument.  Its type is: `((int -> 'a) int -> 'a)`.

We can call it with a function like `zero?`:

```scheme
(printn (call-with-frob zero? 0))
=> #f
(printn (call-with-frob zero? -1))
=> #t
```

Which instantiates `call-with-frob` as `((int -> bool) int -> bool)`.

We might combine it with a different function that takes a single `int` argument:

```scheme
(printn (call-with-frob abs -1))
=> 0
(printn (call-with-frob abs -10))
=> 9
```

Brace yourself.

[note: `lambda` is a way to create a function without giving it a name.]

```scheme
(define (twice fun)
  (lambda (x)
    (fun (fun x))))

(define (add5 n)
  (+ n 5))

(printn ((twice add5) 7))
=> 17
```

Here `twice` takes a function `fun` and returns a new function that
applies it to the argument once, and then again on the result.

We combine it with `add5` (which has type `(int -> int)`) to get a
double-applier that works with integer functions.

We can use it in a completely different context like this:

```scheme
(printn ((twice rest) '(1 2 3 4 5)))
=> (3 4 5)
```

[the function `rest` returns the 'rest' of a list minus the first item].

`twice` has this type: `(('a -> 'a) -> ('a -> 'a))`.


## Row Types

TBD.
