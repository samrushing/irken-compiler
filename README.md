The Irken Compiler
------------------

![irken logo](http://dark.nightmare.com/rushing/irken/irken/irken.svg "logo")

Irken is a statically-typed, simplified variant of Scheme.  Or... a
lisp-like variant of ML, depending on your point of view.  If you're
not familiar with either family of languages, Irken is a strongly
typed, compiled language with a lisp-like syntax extensible with
macros.  It uses type inference along with a powerful type system to
give you the speed of a compiled language with high-level data types
and a higher degree of safety than languages like C/C++.

* Why: to host massively scalable systems scriptable via a python-like language.
* How: compile using continuation-passing style to avoid using the C stack.
* Blog:         [http://alien.nightmare.com/](http://alien.nightmare.com/)
* GitHub:       [https://github.com/samrushing/irken-compiler/](https://github.com/samrushing/irken-compiler/)
* Older Source: [http://nightmare.com/rushing/irken/](http://nightmare.com/rushing/irken/)

News:
-----

20170329: Merged the bytecode backend and VM.

  This has been tested on four platforms: 

  * amd64 osx-10
  * amd64 freebsd-11
  * amd64 linux-ubuntu-xenial
  * aarch64 linux-debian rpi-3
    
Yes, this means that Irken works on ARMv8! (all three backends).
(for more detail see below).

20170217: Merged the LLVM backend, plus many new fixes & features.

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
    [the default compiler is 'clang', to use gcc:
      $ CC=gcc python util/bootstrap.py ]

Which does the following:

1. use self/bootstrap.byc to generate and compile self/compile.c.
2. this binary will be used to recompile the compiler.
3. that binary will recompile the compiler again.
4. the output from steps 2 and 3 are compared, they should be identical.

Note: It is not possible to compile with optimization off, because
this disables the tail call optimization that Irken relies on -
otherwise the stack will overflow instantly.

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

LLVM Backend:
-------------

To use the LLVM backend:

    $ irken vm.scm -llvm

Note: the llvm backend currently assumes that `-flto` can be fed to `clang`, this works
  on OS X, but seems to fail on FreeBSD & Linux.  I think some kind of plugin is needed.
  You can remove `-flto` from `flags.scm` if necessary.


Bytecode Backend:
-----------------

Irken now comes with a VM and bytecode backend.  The VM runs about
3-4X slower than compiled code, but can speed up development because
the edit-compile-run loop skips calling the C compiler.  Turnaround
while working on the compiler (on my machine) is ~5s.

To use the bytecode/VM:

    $ irken myfile.scm -b
    $ irkvm myfile.byc

You can also run the compiler in the VM:

    $ irkvm self/compile.byc myfile.scm -b
    $ irkvm myfile.byc

I am currently working on a universal FFI interface that should be
usable from all 3 backends.


ARMv8
-----

I was able to get Irken running on a Raspberry Pi 3, using
the [pi64](https://github.com/bamarni/pi64) distribution.  Good news:
the llvm `readcyclecounter` intrinsic seems to work just fine on ARM.
Bad news: user mode doesn't have permission to read it.  I was able to
get everything working by commenting all rdtsc-related code in
include/{header1.c,gc1.c}.  My understanding is that reading this
register is a Bad Idea on the ARM, so I may conditionalize it in the
source.
