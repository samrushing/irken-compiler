
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

Ideas:

  * multiple independent VM's.
  * multiple compiled-code objects.
  * ability to run a VM for a given number of insns.
  * debugger.
  * %%cexp-like FFI that can be used by all backends.

------------

data structure compatibility.

Many data structures should be compatible between irken and irkvm.
An exception would be records, I think: one side has no way of knowing
the fields in a record from the other side, unless it is told explicitly.

------------

ok, now what?

how do we decide/communicate which functions are available to the VM.
 Is there any way to automate the process?  like, something that takes
 a list of function names and generates a full interface for them,
 ready to be used by irkvm?

we can start by writing the interfaces manually.  then worry about
automation.

first: can we provide an interface (via get-closure) that
  fetches other closures by name?

and how do we deal with polymorphic interfaces? or can we?  for
 example, it'd be nice to provide the underlying frb interface.  can
 it be done?

and take rubber-meets-the-road stuff like I/O: is it better to go up
 through irken and then back down to C?  or do we provide some other
 interface to low-level stuff, mirroring what %%cexp provides?

another question: how will continuations in the two universes
 interact?  Can we have more than one bytecode VM at once?  e.g.,
 could we have a socket server that provides independent VM's per
 connection? [regardless, we should

------------

Maybe an alternative to %%cexp for FFI?  If we had a 'spec' of sorts
this could be parsed and used separately by the C/LLVM backend and the
bytecode backend.  [this sounds a bit more like a cpython extension
module?]  Right now the 'spec' consists of the type attached to each
%%cexp.

An advantage to this approach could be dynamically-loaded modules?
[this would probably only work with the VM].

An important goal here should be user code that runs identically 
whether compiled by irken or for VM, for rapid devel.

Note: this also puts a toe into the llvm JIT waters.

