FFI Generator
-------------

The purpose of this tool is to automatically (as much as possible)
generate interfaces for C APIs.

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
