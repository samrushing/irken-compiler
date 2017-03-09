
A Virtual Machine implements a high-level language by representing code
  as a series of low-level 'instructions' that emulate an actual machine.

The original goal of the Irken project was to implement a VM for a
  dynamically-typed language like Python (or Scheme).  The type system
  is ideal for this purpose, because a 'universal datatype' can
  represent the run-time typing necessary.

It is not ideal, however, to write a statically-typed VM *for* Irken
  *in* Irken, because it adds an extra layer of tagging/boxing and
  generally makes things difficult (nasty typing tricks would be
  needed to implement the actual low-level tagging and boxing that
  would defeat the purpose of using Irken).

This is an attempt to write a VM for Irken in C, one that can hopefully
  integrate with the Irken runtime seamlessly.

------------

Problems:

  * how to encode literals (and 'constructed' literals) into .byc
  * how to integrate datatypes from bytecode-compiled code.


