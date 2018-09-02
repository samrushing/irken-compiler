
Irken is a Functional Language
------------------------------

Irken is a member of the [Lisp family of languages](https://en.wikipedia.org/wiki/Lisp).
Within that family, it is a dialect of the language [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)).
Lisp languages use prefix-notation expressions in nested parentheses to
structure all code.

[Functional languages](https://en.wikipedia.org/wiki/Functional_programming)
emphasize the mathematical 'function'-ness of programming, and
discourage (Lisp, Scheme, OCaml), or even forbid (Haskell, Erlang) the
manipulation of state using assignment.

Unlike most Lisps, Irken is a _compiled-only_ language.  It uses a
whole-program compiler, so compilation is always a single step (i.e.,
there are no object files and no 'link' phase).  There is no
interactive prompt, or 'read-eval-print-loop'/'repl' (yet).

Compiling an Irken program:

    $ irken myfile.scm

Running it:

    $ ./myfile


Hello World
-----------

```scheme
(require "lib/basis.scm")
(printf "Hello, World!\n")
```

Builtin Types
-------------

The usual collection of simple types:

   * integers: `34`, `19`, `-27`, `#xdeadbeef`
   * strings: `"this is a string"`
   * characters: `#\A`, `#\b`, `#\newline`
   * symbols: `'color`, `'unknwon-unknowns`
   * booleans: `#t`, `#f` (for 'true' and 'false')
   * undefined: `#u` (or 'unit')

Builtin Containers
------------------

   * lists: `("x" "y" "z")`, `(0 2 4 8 16 32 64)`
   * vectors: `#(0 1 2 3 4)`
   * records: `{age=23 name="george"}`

Calling Functions
-----------------

Irken uses a prefix notation.  The first element of the list
is the function being called, the remaining elements are the arguments.

`x + 1` in most languages would be `(+ x 1)` in Irken.

```scheme
(+ 1 2)
(+ (- x 44) (- y 41))
```

Names
-----

In most languages, names of variables and functions are limited to
alphanumeric characters and the underscore.  In Irken (like most Lisps),
nearly any character is legal as part of a name.

Some common naming conventions:

  * `a?`: a boolean function or variable - `(odd? n)`.
  * `a!`: functions that change state, assignments - `(move! ob 3 4)`
  * `a->b` indicates conversion - `int->char`.
  * `a/b` indicates a function related to a type or subsystem: `zlib/deflate`, `sql/query`.
  * `*a*`: a global variable, e.g. `*the-symbol-table*`.
  * `%a`, `%%a`: irken-internal things.

Every Expression Has a Value
----------------------------

Imperative languages often divide the language into two categories,
'expressions' and 'statements'.  Irken is a functional language, and
in Irken, *everything* is an expression.  This means that every
expression has a 'value'.

Expressions that perform imperative operations often return the special
value `#u` (for 'undefined' or 'unit').

Special Forms
-------------

Most Irken expressions are of the form `(function arg0 arg1 ...)`.  However, there
are some 'special forms' that behave differently, like `if`, `let`, `cond`.

Simple Conditional
------------------

```scheme
(if (odd? n)
  (+ 1 n)
  n)
```

`if` is a special form that evaluates the test first, and chooses
one of the other two branches depending on that test.  Only one of the
two choices is evaluated.

```scheme
(if (odd? n)
    (printf "n is odd.\n")
    (printf "n is even.\n"))
```


Creating Variables
------------------

`let` is a special form that binds new variables:

```scheme
(let ((h (compute-height))
      (w (compute-width)))
  (* w h))
```

Here, `h` and `w` are new variables.  The value of the entire
`let` expression is `(* w h)`.

The general format is:

```scheme
(let ((<var0> <value0>)
      (<var1> <value1>)
      ...
      (<varn> <valuen>)
      )
  code using var0..varn
  )
```

Assigning Variables
-------------------

Variables are assigned with `set!`.

```scheme
(set! x 19)
```

Defining Functions
------------------

Functions are created with `define`:

```scheme
(define (double x)
  (+ x x))
```

Which can now be called:

```scheme
(+ 1 (double x))
(double (double x))
```

Here is a factorial function:

```scheme
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))
      ))
```

