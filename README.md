The Irken Compiler
------------------

![irken logo](http://dark.nightmare.com/rushing/irken/irken/irken.svg "logo")

Irken is a statically-typed, simplified variant of Scheme.  Or... a
lisp-like variant of ML, depending on your point of view.  If you're
not familiar with either family of languages, Irken is a strongly
typed, compiled language with a lisp-like syntax extensible with
macros.  It uses type inference along with a powerful type system to
give you the speed of a compiled language with high-level data types
and unprecedented safety.

* Why: to host massively scalable systems scriptable via a python-like language.
* How: compile a vm using continuation-passing style to avoid using the C stack.
* Blog:         [http://alien.nightmare.com/](http://alien.nightmare.com/)
* GitHub:       [https://github.com/samrushing/irken-compiler/](https://github.com/samrushing/irken-compiler/)
* Older Source: [http://nightmare.com/rushing/irken/](http://nightmare.com/rushing/irken/)

News:
-----

130301: The 'ctailfun/noreg-ssa' branch has been merged into master.  This is
  a fairly radical rearchitecting of the compiler that will enable lots of fun
  new stuff like an LLVM backend, JIT, separate compilation, etc...


Introduction/Tutorial:
----------------------

  [http://dark.nightmare.com/rushing/irken/irken/lang.html](http://dark.nightmare.com/rushing/irken/irken/lang.html)

The best way to get familiar with the language is to read the source code in
the 'self' directory, and browse over the files in "tests".

Bootstrap:
----------

Irken is now written in itself, so you need to bootstrap it.  You can do this
via the python script, or manually.
[see https://github.com/samrushing/irken-compiler/wiki/bootstrapping-manually]

Just run the script "util/bootstrap.py":

    $ python util/bootstrap.py
    [to use clang:
      $ CC=clang python util/bootstrap.py ]

Which does the following:

1. compile the distributed version of self/compile.c
2. this binary will be used to recompile the compiler.
3. that binary will recompile the compiler again.
4. the output from steps 2 and 3 are compared, they should be identical.

Note: It is not possible to compile with optimization off, because
this disables the tail call optimization that Irken relies on -
otherwise the stack will overflow instantly.

At this time (early 2013), clang builds -O3 in about 30 seconds.
gcc 4.8 seems to do a better of job of optimizing than clang 3.2, but
takes about twice as long to compile.

Installation:
-------------

    $ python util/install.py

This will install support files into `/usr/local/lib/irken` and the binary as `/usr/local/bin/irken`.
If you want to use a different prefix (like `/usr`), edit `util/install.py` and `self/context.scm`, and
rebuild self/compile before installing:

    $ self/compile self/compile.scm

Usage:
------

Here's a sample - build the toy VM and test it out:

    $ cd vm
    $ irken vm.scm
    $ ./vm tests/t11.byc

The VM executes some bytecode that runs the 'tak' benchmark 20 times.

You might want to try looking at and understanding the 'verbose' output from the compiler,
using a relatively small example:

    $ irken -v tests/t_while.scm

Irken will use the CC and CFLAGS environment variables when compiling the C output.  You may
try out another compiler like this:

    $ CC="gcc" CFLAGS="-std=c99 -O2 -I." irken ...

