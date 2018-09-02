
Defining New Types
==================

Unlike most Lisps, Irken's type system is based on 'algebraic
datatypes', where complex types are built from simpler types.

New types are created with the `datatype` special form:

    (datatype year
      (:known int)
      (:approx int)
      (:range int int)
      (:unknown)
      )

Here, we've created a new datatype `year`, which has four
different 'variants'.  To create a 'known' year:

    (year:known 1972)

[`year:known` is called a 'constructor'.  It builds a value of
of datatype `year`, with the variant `known`.]

To create a 'range' year...

    (year:range 1530 1540)

... and an 'unknown' year:

    (year:unknown)

Another example:

    (datatype monarch
      (:king bool)
      (:pharaoh bool)
      (:machine)
      )

Here, we use a `bool` to indicate male or female, where:

    (monarch:king #f)

would indicate a queen.

We can combine the two dataypes:

    (datatype rule
      (:reign monarch year year)
      (:chaos year year)
      )

The rule of our robot overlords:

    (rule:reign (monarch:machine) (year:approx 2050) (year:unknown))

Polymorphic Datatypes
---------------------

We need not use a specific type when defining a new datatype.
Instead, we can define it to work with any type:

    (datatype thing
      (:one 'a)
      (:two 'a 'a)
      )

Here, `'a` indicates 'any type'.  We can now create new `things` of
different types:

    (thing:one "a string")
    (thing:one #f)
    (thing:two 27 27)

When we create such a value, we 'instantiate' the polymorphic type
(i.e. 'fill in the blanks').  We say that `(thing:one "a string")` is
of type `(thing string)` and `(thing:two 27 27)` is of type `(thing int)`.

Note that we _cannot_ do this:

    (thing:two 10 #f)

Because `10` and `#f` are different types (`int` and `bool`,
respectively), and there's no way to 'fill in' two different types for
`'a`.

If we need a datatype that allowed this, we might use:

    (datatype thing2
      (:one 'a)
      (:two 'a 'b)
      )

We say that `thing2` has type `(thing2 'a 'b)`.

Note: there is still a constraint on `thing2` objects.  These two
are _not_ of the same type:

    (thing2:one 34)
    (thing2:two #f "string")

But these two _are_ of the same type:

    (thing2:one 34)
    (thing2:two 19 "string")

Recursive Datatypes
-------------------

A recursive datatype is one that can store other values of the same
type inside:

    (datatype int-list
      (:empty)
      (:node int int-list)
      )

Now we can create a list of ints:

    (int-list:node 1 (int-list:node 2 (int-list:empty)))

Recursive datatypes _must_ have at least one variant that's not
recursive, otherwise only infinite values are possible.

We can make a different version of `int-list` that disallows
empty lists:

    (datatype ints2
      (:last int)
      (:more int ints2)
      )

    (ints2:more 1 (ints2:more 2 (ints2:last 3)))

You can also have mutually recursive datatypes:

    (datatype crazy1
      (:one crazy2)
      (:two crazy2 crazy2)
      )

    (datatype crazy2
      (:stop)
      (:more crazy1 crazy1)
      )

    (crazy2:more
     (crazy1:two (crazy2:stop) (crazy2:stop))
     (crazy1:one (crazy2:stop)))

Recursive Polymorphic Datatypes
-------------------------------

Now, witness the firepower of this fully operational type system.

We can create a list of anything:

    (datatype list
      (:cons 'a (list 'a))
      (:nil)
      )

[this is the actual definition used in the Irken library]

    (list:cons 1 (list:cons 2 (list:cons 3 (list:nil))))

Or a binary tree of anything:

    (datatype btree
       (:empty)
       (:node 'a (btree 'a) (btree 'a))
       )

Here's a `(tree int)`:

    (btree:node 19
      (btree:node 34
        (btree:empty)
        (btree:empty))
      (btree:empty))

and a `(btree string)`:

    (btree:node "hey there" (btree:empty) (btree:empty))

and a `(btree (list int))`:

    (btree:node (list:cons 1 (list:nil)) (btree:empty) (btree:empty))

Complex Datatypes
-----------------

By combining recursion and polymorphism, you can build complex and useful new types:

    (datatype alist
      (:empty)
      (:node 'a 'b (alist 'a 'b))
      )

This is similar to an `alist` (or 'association list') from Lisp.

    (alist:node 3 "three"
      (alist:node 4 "four"
        (alist:empty)))

You could use this datatype to search for the value of type `'b`
associated with a key of type `'a`.

A Datatype For Mathematical Expressions
---------------------------------------

We can represent mathematical expressions symbolically.

First, we create a datatype of all the different operations we support:

    (datatype binop
      (:+)
      (:-)
      (:*)
      (:/)
      )

[Note that none of its variants have data attached.  This would be the equivalent
 of an `enum` type in C, Java, etc...]

Now, a datatype for expressions:

    (datatype exp
      (:var symbol)
      (:int int)
      (:binop binop exp exp)
      )

Here's a variable (named with a 'symbol'):

    (exp:var 'x)

Here's an integer:

    (exp:int 42)

Here we have `x + 42`:

    (exp:binop (binop:+) (exp:var 'x) (exp:int 42))

and `2*x-12`:

    (exp:binop
       (binop:-)
       (exp:binop (binop:*) (exp:int 2) (exp:var 'x))
       (exp:int 12))

