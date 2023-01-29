
Irken's Foreign Function Interface (FFI)
----------------------------------------

The FFI makes it possible to call C libraries from Irken, in any of the
supported backends.  This facility bends over backwards to try to make
this as easy as possible.  If you're lucky, you'll never need to know how
it all works.

Steps to make a foreign-function call:

1. write a simple spec file (e.g. `mylib.ffi`).  This just tells
   `genffi` which options to pass to the compiler and linker, and
   which functions, enums, structs, constants, etc... you are
   want to use.

2. generate[^1] a detailed interface which the Irken compiler can use:

    ```$ ffi/gen/genffi -gen mylib.ffi```

3. install the generated file (`mylib_ffi.scm`) where the compiler can find it:

    ```$ cp mylib_ffi.scm /usr/local/lib/irken/ffi/```

4. add a `(require-ffi 'mylib)` form to your source.

5. call your function:

    ```(mylib/myfunction "testing" 1 2 3)```

[^1]: you may need to compile it - `irken ffi/gen/genffi.scm`.


FFI Generator
-------------

The purpose of the `genffi` tool is to automatically generate (as much as possible)
interfaces for C APIs.

There are many pitfalls in this forbidden land.  Often the API advertised
by the documentation does not match the actual function signatures.  One
of the biggest problems is created when macros are used in various ways
to rename functions, parameters, types, etc.  They are also used to add
extra (undocumented) parameters (e.g. zlib).

In general, my preference is to avoid linking against C libraries at
all, since a major focus of the Irken project is added security
against that world of razors and chainsaws.

FFI Specifications
------------------

The process starts with writing a 'spec' file.  This is a file of s-expressions
that describes what you need out of the API.  You list the include files needed,
along with a list of constants, functions you need signatures for, and structures
you need to access.  Hopefully `genffi` can do the rest.

sample.ffi:

    ;; -*- Mode: lisp -*-

    (mylib
      (includes "myheader.h" "other-header.h")
      (dll "libmine.so")
      (constants MYLIB_YES MYLIB_NO)
      (sigs my_fun1 my_fun2)
      (struct my_struct1)
      (enums my_enum)
      (verbatim
        (sig my_fun3 ((* char) int int -> int)))
      )

This describes an interface to `mylib`.  The only tricky part here is
the `verbatim` section: this is where you can place things directly into
the output that cannot be handled by the normal `genffi` process.  Or if
you'd rather take complete control over the output for whatever reason.

In the `struct` section you should list only structs you actually need
to allocate and/or access.

An `enum` section will cause `genffi` to emit a constant record for each
value in the enum.

The `dll` entry is only needed if you want to dynamically load the library
in order to use it with the VM (bytecode backend).  Note: the location and naming
of this library is somewhat platform-dependent.  You may need to carefully
read the man page for `dlopen(3)` on your platform.

Normal Usage
------------

You now feed this spec file into genffi:

    $ ffi/gen/ffi -gen ffi/mylib.ffi
    genc1... FFI = "ffi/mylib.ffi"
    CC: clang -E ffi/mylib_iface1.c > ffi/mylib_iface1.cpp
    processing 'ffi/mylib_iface1.cpp' ...
    ...done.
    CC: clang ffi/mylib_iface2.c -o ffi/mylib_iface2
    CC: clang ffi/mylib_iface1.c -o ffi/mylib_iface1

If everything goes well you now have a usable interface file in
`ffi/mylib_ffi.scm`.  You can move this into `/usr/local/lib/irken/ffi/`
and continue on your merry way.

Accessing FFIs from Irken
-------------------------

You need to tell the compiler and the runtime that you are using the interface:

    (require-ffi 'mylib)

You should now be able to call your functions:

    (mylib/my_fun3 name width height)

Varargs functions
-----------------

Varargs functions do not mesh well with Irken's type system, but they
mostly work without issue (though with reduced type safety).  Calling
a variadic function in the VM is a bit trickier, the `%%ffi` form must
be built manually.  See `posix/open` in `lib/io.scm` for an example.

When Trouble Strikes
--------------------

Various things can go wrong with `genffi`.  It may not be able to parse
some of the declarations, or it may not find a function it is looking for.
To solve the issue you will need to understand how `genffi` works.  Sorry.

First, it generates a C file (containing mostly include statements) that is
then fed to the C preprocessor.  The output from that is scanned by a parser
that knows enough about C to pull out typedefs and declarations.

Step one in problem-solving is to pass the `-v` argument to genffi.  This will
produce more detail about what is parsed, problem declarations, and will dump
the results in a pleasing manner.

You can also scan a pre-processed C file manually:

    $ ffi/gen/genffi -scan ffi/mylib_iface1.cpp

Recreational API browsing
-------------------------

You can also use `genffi` to dump lots of information about any
include file.  This is a handy way to see what's going on 'behind the
scenes' with your favorite library.


    $ ffi/gen/genffi -try ~/src/tweetnacl/tweetnacl.h
    base = tweetnacl
    CC: clang -E /tmp/tweetnacl.c > /tmp/tweetnacl.cpp
    processing '/tmp/tweetnacl.cpp' ...
    ...done.

    --------- typedefs -----------

    --------- structs -----------

    --------- functions -----------
    [...]
         crypto_core_salsa20_tweet (fun (* uchar) (* uchar) (* uchar) (* uchar) -> int)
        crypto_core_hsalsa20_tweet (fun (* uchar) (* uchar) (* uchar) (* uchar) -> int)
    crypto_hashblocks_sha512_tweet (fun (* uchar) (* uchar) ulonglong -> int)
    crypto_hashblocks_sha256_tweet (fun (* uchar) (* uchar) ulonglong -> int)
          crypto_hash_sha512_tweet (fun (* uchar) (* uchar) ulonglong -> int)
          crypto_hash_sha256_tweet (fun (* uchar) (* uchar) ulonglong -> int)
    [...]


Architecture
------------

The tool is built using Irken's new (as of 2017) lexing and parsing capabilites.
The lexicon for C is in `ffi/gen/c-lex.sg`, and the grammar is in `ffi/gen/cgram.scm`.
Because `genffi` is needed to bootstrap the Irken VM, both the grammar and lexer are
compiled directly into the program.  If you modify the lexer you need to re-run
the lexing tool.  After modifying the grammar simply recompile genffi:

    $ irken ffi/gen/genffi.scm
    [or]
    $ self/compile ffi/gen/genffi.scm
