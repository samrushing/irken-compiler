
objects
-------

There are several different approaches possible:

  1. we use a record to hold instance data, and the full API consists
     of public functions that access that record.
  2. we use a closure to hold instance data, and the API consists of
     a record of functions.
  3. a record to hold instance data, and another record to hold 'methods'
     on that class (our quasi-OO support).
  4. another ideal approach not yet discovered.

pros/cons
---------

  1. pros: simple and efficient - functions can be inlined.
     cons: little data hiding, and user code must know exactly
           what kind of socket they are using.  large surface.

  2. pros: simple.  instance data is hidden.  type signatures are
           generic - they do not expose unnecessary details and
           are thus compatible between different users.
     cons: each object has a size proportional to the API size.
           functions cannot be inlined.

  3. pros: transparency of interface. ideal object size. (i.e., more like
           most OO languages)
     cons: self must be passed explicitly.  the types are visible everywhere,
           and are huge.

the ideal
---------

The ideal approach would combine the best features of each:

  1. interface transparency: API is a set of functions whose types
     hide any internal details.
  2. efficient object representation: the size of each object is proportional
     to the size of the instance data, not the size of the interface.
  3. functions can be inlined. (i.e., static rather than dynamic).
  4. no explicit 'self' parameter.

Now, I don't think meeting all these goals is possible in 'plain Irken',
to get them all will certainly require support from the compiler, and probably
some smarts in the type system.

One approach, non-OO, is to do something like ocaml/sml with 'interfaces', and
'cast' objects to a certain interface (I guess, having the compiler *remove*
information about types at a boundary?).
