The Irken Compiler
------------------

![irken logo](irken.svg "logo")

[![Build Status](https://travis-ci.com/samrushing/irken-compiler.svg?branch=master)](https://travis-ci.com/samrushing/irken-compiler)

Irken is a statically-typed, simplified variant of Scheme.  Or... a
lisp-like variant of ML, depending on your point of view.  If you're
not familiar with either family of languages, Irken is a strongly
typed, compiled language with a lisp-like syntax extensible with
macros.  It uses type inference along with a powerful type system to
give you the speed of a compiled language with high-level data types
and a higher degree of safety than languages like C/C++.

* Why: to host massively scalable systems without using threads.
* How: compile using continuation-passing style to avoid using the C stack.
* Blog: [http://alien.nightmare.com/](http://alien.nightmare.com/)
* GitHub: [https://github.com/samrushing/irken-compiler/](https://github.com/samrushing/irken-compiler/)

News:
-----

202611: Native backends, and a new bootstrap.

  * the compiler now emits assembly directly for arm64, amd64 and
    riscv64 (`-arm`, `-amd`, `-rv`), in addition to C and bytecode.
    the LLVM backend is gone.
  * `make bootstrap` builds a native compiler, using the VM only once;
    on an M2 Pro mac mini it takes about 40 seconds.
  * a new test suite in `test/` (`python3 util/test.py`).
  * the garbage collector no longer clears the heap after every
    collection: a small program uses ~10MB, not ~400MB.

  Tested on:

  * arm64 macOS (M1 Pro, M2 Pro)
  * amd64 macOS
  * arm64 freebsd-14
  * amd64 freebsd-14
  * arm64 linux
  * amd64 linux
  * riscv64 linux (StarFive)

20180829: Merged the FFI branch in.

  This includes a *lot* of work:

  * new FFI system that supports all three backends.
  * `genffi` uses a full C parser to generate an interface file.
  * ASN.1 codec
  * Lots of crypto support code (x509, hmac, hkdf, rsa-pss, libsodium, libdecaf, ...).
  * constant-time modular bignum arithmetic for crypto, based on Thomas Pornin's i31 code.
  * DFA lexer and regex library based on derivatives.
  * Earley parser.
  * TLS 1.3 implementation.
  * websockets/rfc6455.
  * HTTP/2 server.

20170329: Merged the bytecode backend and VM.

  This has been tested on four platforms:

  * amd64 osx-10
  * amd64 freebsd-11
  * amd64 linux-ubuntu-xenial
  * aarch64 linux-debian rpi-3

Introduction/Tutorial:
----------------------

  [https://dark.nightmare.com/rushing/irken/irken/lang.html](https://dark.nightmare.com/rushing/irken/irken/lang.html)

The best way to get familiar with the language is to read the source code in
the 'self' directory, and browse over the files in "tests".

Requirements:
-------------

* a 64-bit machine: arm64 or amd64 (macOS, Linux, FreeBSD), or riscv64 (Linux).
  Others should work with the C backend.
* python3, make, and a C compiler (clang or gcc).
* libffi, with its headers, for the bytecode VM:
  * macOS: nothing extra (the Xcode command line tools have it).
  * Debian/Ubuntu: `apt install libffi-dev`.  Fedora: `dnf install libffi-devel`.
  * FreeBSD: `pkg install libffi`.
* optional, for the crypto and HTTP/2 demos: libsodium and zlib
  (e.g. `apt install libsodium-dev zlib1g-dev`, `pkg install libsodium`,
  or `brew install libsodium`).  Without them, the bootstrap just skips
  their interfaces.

Bootstrap:
----------

Irken is written in itself, so you need to bootstrap it using a pre-compiled bytecode image.

Just run ``make bootstrap``:

    $ make bootstrap

It uses clang if it can find it, otherwise `cc`.  To choose:

    $ CC=gcc make bootstrap

It starts by printing the host, OS, git commit and compiler, then the
time at each step.  It does the following:

1. builds the bytecode VM, and uses it to run self/bootstrap.byc once:
   it compiles the native compiler for this machine (self/irken; arm64,
   amd64 or riscv64.  elsewhere, the C backend).
2. that binary recompiles the compiler, and the result recompiles it
   again: the output from these two steps should be identical.
3. compiles genffi (ffi/gen/genffi), and uses it to generate this
   platform's FFI interfaces (posix, sockets, kqueue/epoll, ...).
4. builds self/compile, which has all the backends.

The compiler itself only needs C's libc and stdio (see lib/base.scm),
so nothing platform-specific has to be generated before step 1.

You end up with two compilers: `self/irken`, which has only this
machine's native backend (and is the quickest to rebuild while working
on the compiler), and `self/compile`, which has them all.

Testing:
--------

    $ python3 util/test.py

runs the suite in `test/` with every backend this machine can run (C,
bytecode, and the native one), and should end with no failures: a few
known bugs are marked as expected failures.  See `test/README.md`.

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

Here's a sample - create a random ASCII maze:

    $ irken demo/maze/maze.scm
    $ demo/maze/maze 20 10 -ascii
    
     +--------+--------+-----+-----------+-----------------+-----+
     |        |        |     |           |                 |     |
     |  +---  |  ---+  +--+  |  +--------+  |  +-----+---  |  |  |
     |  |     |     |     |  |  |           |  |     |     |  |  |
     |  |  ---+---  +---  |  |  +--+  +---  +--+  |  +--+  |  +--+
     |  |           |     |  |     |  |     |     |     |  |     |
     |  +-----+--+--+  +--+  +---  |  |  ---+  ---+---  |  +---  |
     |  |     |  |     |  |  |        |     |     |     |  |     |
     |  |  |  |  |  +--+  |  +-----+--+-----+---  |  +--+  |  ---+
     |     |  |  |  |  |  |        |        |     |  |     |     |
     +--+--+  |  |  |  |  +------  |  ---+  |  +--+  |  ---+  |  |
     |  |     |  |        |        |     |     |     |        |  |
     |  |  |  |  +--+---  |  +---  +-----+-----+  ---+---  ---+  |
     |     |  |     |        |     |           |              |  |
     +-----+  +---  |  +-----+  +--+  ------+  +------  |     |  |
     |        |        |     |  |           |           |     |  |
     |  ---+--+--+-----+  |  |  |  +-----+  +-----------+  +--+  |
     |     |     |        |  |  |  |     |              |  |     |
     |  |  +---  |  |  ---+  |  +--+  |  +------------  |  |  ---+
     |  |        |  |     |           |                 |  |     |
     +--+--------+--+-----+-----------+-----------------+--+-----+


You might want to try looking at and understanding the 'verbose' output from the compiler,
using a relatively small example:

    $ irken -v tests/t_while.scm

Irken will use the CC and CFLAGS environment variables when compiling or assembling its
output.  You may try out another compiler like this:

    $ CC="gcc" CFLAGS="-std=c99 -O2 -I." irken ...

Backends:
---------

The compiler (`irken` once installed, `self/compile` before) emits native
code for the machine it's running on: arm64 (macOS, Linux, FreeBSD),
amd64 or riscv64 (Linux).  Elsewhere, it emits C.  The output is
assembled or compiled, and linked, with the C compiler, along with the
runtime in `include/`.  To choose a backend:

    $ irken -C demo/maze/maze.scm       # C
    $ irken -b demo/maze/maze.scm       # bytecode, for the VM (see below)
    $ irken -arm demo/maze/maze.scm     # arm64
    $ irken -amd demo/maze/maze.scm     # amd64
    $ irken -rv demo/maze/maze.scm      # riscv64

`self/irken`, left by the bootstrap, has only this machine's native
backend, which makes it quicker to rebuild while working on the compiler.


Bytecode Backend:
-----------------

Irken now comes with a VM and bytecode backend.  The VM runs about
3-4X slower than compiled code, but can speed up development because
the edit-compile-run loop skips calling the C compiler.  Turnaround
while working on the compiler (on my machine) is ~5s.

To use the bytecode/VM (before installing, the VM is `vm/irkvm`):

    $ irken myfile.scm -b
    $ irkvm myfile.byc

You can also run the compiler in the VM:

    $ irkvm self/compile.byc myfile.scm -b
    $ irkvm myfile.byc

32-bit platforms
----------------

Once upon a time Irken compiled and ran on 32-bit x86.  With the
SSA-style rewrite that relies on tail call elimination, this appears
to no longer be possible.  Neither gcc or llvm seem to support full
tail call elimination on this platform.  Even manually tagging all
functions with 'fastcc' (by compiling to llvm asm and editing the
result) does not fix the problem.  If you really need to run Irken on
a 32-bit platform, know that the VM seems to work just fine.
[that said, many of the library facilities assume 64-bit integers are
 available]
