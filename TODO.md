TODO for 2024 07

h2 server 'demo'
----------------
* compute address/unparse only once!
* maybe: content-encoding
* log host/authority
* alist/get raises error
* figure out O_APPEND on log file (getting wiped if already open?)
* getopt
* if-modified-since
* weak ETag (mtime & size combined) rather than i-m-s.
* port 80 redirector

redirector seemed to fail with: "GET / HTTP/1.0\r\nAccept-Language: en-us,en-gb,en;q=0.7,*;q=0.3\r\nAccept-Charset: utf-8,ISO-8859-1;q=0.7,*;q=0.7\r\nAccept: text/html,application/xml;q=0.9,application/xhtml+xml,text/xml;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\nAccept-Encoding: x-gzip, gzip, deflate\r\nUser-Agent: ZoominfoBot (zoominfobot at zoominfo dot com)\r\nHost: nightmare.com\r\n\r\n"


trailing slash 301 redirect for directories e.g. /thing/dir => /thing/dir/
status/stats handler
user auth handler
log tls/etc errors
redirect stderr to another file
investigate enabling p256-based kex (for tlsfuzzer maybe?)


h2 server future
----------------
some kind of scripting/template language?
status/debug/etc page
image browser!

compiler
--------
improve the error reporting.  thinking: crawl up the list of src until we find something
  not copied from elsewhere?


native code compiler
--------------------

issues remaining:

* whatever causes VM to spin if there's a failure in system(CC)
  - seems to be related to raising an error - probably the loop of lookup up error polyvars
* maybe make an irkenb that's similar to genffi in that it carefully avoids all FFI
* find a way to put c-call args into the right registers directly
* and/or do cycle decomposition to permute the registers
* source positions in the asm output: CPS insns don't carry source info
  yet (nodes do), so thread it through the cps continuations, then emit
  `// file.scm:line` comments and eventually `.file`/`.loc` directives so
  lldb/gdb/addr2line can map a pc back to Irken source.
* per-function metadata (e.g. for a more complex/generational gc)
  alongside the existing function symbols.
* riscv64: save/restore caller-saved irken registers around runtime
  calls, as amd64/arm64 now do.
* dead allocations: an unused record result gets target -1 and the C
  backend emits `r-1[3] = ...` (amd64 aborts with an out-of-range
  index).  repro: (try (begin (file/open-read "/nonexistent") #u)
  except (:OSError e) -> (printn e))
* OSError carries only errno; including the syscall name and
  strerror() would make failures much easier to read.
* file/close doesn't flush the write buffer, so anything written with
  file/write and not explicitly file/flush'd is lost.  Read and write
  files are different record types, so the fix probably means a
  separate close for each.
* string-split-string raises :String/SplitTooLong when the separator
  is longer than the string, so string-replace-all does too.

platforms to test
=================

         freebsd	linux	macos
    amd     X         X       X
    arm     X         X       X
    riscv   _         X       _

