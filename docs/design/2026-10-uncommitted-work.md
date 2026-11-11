# Irken uncommitted work: survey and plan

*October 2026.  Historical: this plan was carried out.  The work landed
as `ffi-cleanup` and `native-backends`, merged to master and pushed in
November 2026; LLVM was dropped, Euclidean division kept (and later given
SRFI 141 names), and p256/ left out.  Kept for the inventory and the
reasoning.*

## Summary

The working copy holds about 21 months of uncommitted work (Feb 2023 – Nov 2024), and it splits cleanly into about 20 commits across three eras. The recommendation: first push the 44 commits already sitting on `ffi-cleanup`, then commit the new work in dependency order on a fresh branch, keeping the three native backends as their own commits.

What's there:

- **108 modified tracked files** (+2,356 / −1,075 lines, ignoring whitespace) and **613 untracked files**. Of the untracked files, roughly 300 are source worth keeping. The rest are build outputs, scratch experiments and private keys.
- **Three native-code backends**, written Sep–Nov 2024: arm64 (`self/arm.scm`, written first), amd64 (`self/amd.scm`) and riscv64 (`self/rv.scm`). They share `self/asmlits.scm`, a per-platform assembly preamble each in `include/`, a per-backend compiler driver (`self/irkarm.scm`, `irkamd.scm`, `irkrv.scm`), and about 100 new small tests in `tests/t-*.scm`.
- **Compiler-core changes they depend on**: a real register allocator for native targets in `cps.scm`, platform detection and a `%%platform` form, better error reporting, and a rework of integer division semantics in `lib/core.scm`.
- **Two earlier bodies of work that are unrelated to the backends**: crypto/TLS/SSH from 2023 (P-256, BearSSL FFI, X.509/PEM, an SSH server under `doom/ssh/`), and an HTTP/2 web server demo from Jul–Aug 2024 (`demo/h2/`, DNS, mimetypes, calendar, regex).
- **One recent edit**: May 2026 float support in the JSON demo, which has a bug (see Risks).

Nothing has been changed in the folder yet. Everything below is a proposal.

## Repository state

GitHub has not moved since May 2021, so there is nothing to merge or rebase against. The local history is a clean fast-forward on top of it.

| Ref | Where | Last commit | Notes |
| --- | --- | --- | --- |
| `master` (GitHub) | `a36fc2c` | 2021-05-02 "new bootstraps." | Matches local `master` and `origin/master` |
| `tagsize` (GitHub) | `04299d3` | 2023-01-19 "new bootstraps at tagsize=16" | Already pushed; contained in `ffi-cleanup` |
| `symann` (local only) |  | 2023-02-13 | Fully merged into `ffi-cleanup` |
| `ffi-cleanup` (local only, checked out) | `be2895c` | 2023-02-13 "dump/disassemble byc files." | 44 commits ahead of `master`, never pushed |
| Working tree |  | Edits from Jan 2023 to Nov 2024, plus 3 files from May 2026 | 108 modified, 613 untracked; no stashes |

The clone is 138 MB on disk, of which the `.git` directory is only 20 MB. Most of the rest is generated `.s` files and binaries. The compiler binaries in `self/` (`irkarm`, `irkamd`, `irkrv`, `irkenb`) are all arm64 Mach-O, so the amd64 and riscv64 compilers were being run as cross-compilers on the Mac.

The repo sits at `~/src/irken/claude/irken-compiler`. If that's a copy made for this exercise, it's worth confirming no newer edits live in another checkout before committing anything.

## Inventory of the work

The work falls into seven themes. File modification times date them reliably, and they rarely overlap, which makes splitting easy. Newest first:

| Theme | When | Main files | What it is |
| --- | --- | --- | --- |
| JSON floats | May 2026 | `demo/parse/json.scm`, `json.sg`, `lib/dfa/lexicon.scm` | Adds a `FLOAT` token and a `json:float` variant to the JSON demo |
| **Native backends** | Sep – Nov 2024 | `self/arm.scm` (55 KB), `self/amd.scm` (61 KB), `self/rv.scm` (49 KB), `self/asmlits.scm`, `include/{arm,amd}-{elf,mach}-preamble.s`, `include/rv-preamble.s`, `self/irk{arm,amd,rv}.scm`, `util/bootstrap2.py`, `util/install2.py`, \~100 `tests/t-*.scm` | arm64 first, then amd64, then riscv64 (Linux only). Each emits assembly directly from CPS, with inline asm (`%asm-c-call`), peephole passes and perfect-hash field lookup. The TODO matrix shows amd and arm tested on FreeBSD, Linux and macOS, and riscv on Linux only |
| Compiler core (supports the backends) | Sep – Nov 2024 | `self/cps.scm`, `context.scm`, `compile.scm`, `nodes.scm`, `transform.scm`, `analyze.scm`, `typing.scm`, `match.scm`, `backend.scm`, `c.scm`, `include/header1.c`, `gc1.c`, `irken.h`, `rdtsc.h`, `vm/irkvm.c` | Real register allocation for native targets; liveness fix (target removed before refs); `-arm`/`-amd`/`-rv`/`-np` flags; platform detection, `platform-configure`, `%%platform`; LLVM backend commented out; errors raised as exceptions with source context; "incomplete match" lists missing cases; `SYMBOL_HEADER` change; arm64 `rdtsc` via `CNTVCT_EL0`; new bootstrap `.byc` |
| Library additions | 2023 – Nov 2024 | `lib/core.scm`, `string.scm`, `pair.scm`, `posix.scm`, `os.scm`, `io.scm`, `lib/deque.scm`, `lib/bignum.scm` | Euclidean/floored/truncated `divmod` family (`/` and `mod` become Euclidean), `string-trim-*`, `string-reverse`, `char->utf8`, `zip`/`unzip`, `stat`/`isdir?`, `path/join`, `file/open-append` |
| HTTP/2 server | Jul – Aug 2024 | `demo/h2/`, `doom/http/h2.scm`, `hpack.scm`, `hpack2.scm`, `doom/scheduler.scm`, `sync.scm`, `lib/net/dns.scm` (moved out of `doom/dns.scm`), `lib/calendar.scm`, `time.scm`, `mimetypes.scm`, `regex.scm`, `getopt.scm`, `TODO.md` | A file-serving HTTPS/h2 server with redirector, stats and auth handlers, plus the library pieces it needed |
| Crypto, TLS and SSH | Feb – May 2023 | `lib/asn1/`, `lib/crypto/{pem,x509,sig,sodium,bearssl,p256}.scm`, `ffi/{bearssl,p256_ecdsa}.ffi`, `doom/tls/`, `doom/ssh/`, `demo/crypto/`, `p256/`, \~25 `tests/t_*` crypto tests | BER/DER, PEM, X.509, BearSSL and libsodium bindings, P-256 via fiat-crypto, TLS 1.3 signer/kex updates, and an SSH server with a line-editing REPL demo |
| Small odds and ends | Jan 2023 – Aug 2023 | `tests/f_*.scm` (expected-failure tests), `ffi/{dl,vari,kqueue}_ffi.scm`, `ffi/gen/*`, `demo/repl/eval.scm`, `demo/maze/`, `demo/prime_sieve.scm` | FFI generator fixes, the REPL's evaluator split into its own file, maze demo and UTF-8 helper |

## What must not be committed

About half the untracked files should stay out of git. The private keys are the one item that matters for safety. The rest is clutter that a few `.gitignore` lines will handle.

**Private keys and certs (never commit, even to a private branch):**

- `doom/ssh/id_ed25519` (an SSH private key) and `id_ed25519.pub`
- `cert/key.pem`, `key.der`, `key.sexp`, `skey_p256.der`, `skey_p256_irk.pem`, `cert/p256/key.pem`
- `demo/h2/key.pem`, `demo/h2/skey.pem`, and the matching `cert.pem` files

If any of these are live keys (the h2 demo looks like it served `nightmare.com`), treat them as sensitive. Tests that need a key should generate a throwaway one or use a clearly labelled test fixture.

**Build outputs (add to `.gitignore`):**

- 186 generated `.s` files, mostly next to each `tests/t-*.scm` and demo, and about 100 compiled binaries
- `self/irk{arm,amd,rv}`, `irken`, `irkenb` and their `.s`/`.c`/`.byc` outputs (5 MB each)
- `util/__pycache__/`, `util/target.pyc`, `util/tmp*.ll`, `p256/lib/*.o`, `*.dylib`, `a.out`
- `self/irken.scm` and `self/amdamd.scm`, which are symlinks used to pick the active driver

**Scratch experiments at the repo root (about 60 files):** `bc.c`, `eq.c`, `gc.c`, `icmp.c`, `manyargs.c`, `mul2.c` and friends, each with a `.s` beside it. These look like C snippets compiled with `clang -S` to see what instructions the C compiler picks. They could be deleted, or moved to an ignored `scratch/` folder. `pi.image`, `test-file.txt`, `hm.py`, and the Emacs autosave files (`#t_berexp.scm#`, `#berexp.scm#`) fall in the same bucket.

**Judgment calls:** `p256/` holds your P-256 work mixed with third-party code (BoringSSL's `p256.c`, fiat-crypto output) plus build output and a `p256_ecdsa.tar.gz`. You also keep P-256 work at \`\~/src/irken/p256\`, so this copy may be stale. Check which is current, then either point to that one or vendor this one with its licenses, rather than committing it loose. The generated `ffi/*_ffi.scm` files follow existing practice (`libc_ffi.scm` and `stdio_ffi.scm` are already tracked), so committing them is consistent.

## Proposed commit plan

Push what's already committed, then land 22 new commits on a fresh branch, `native-backends`, cut from `ffi-cleanup`. The order runs from foundations to features, so each commit only uses things earlier commits added. Dates would be set from the file modification times, so the history reads in the order the work happened.

**Step 0: push `ffi-cleanup` unchanged.** Its 44 commits are already well-formed. Pushing them first means nothing below can disturb them.

**Housekeeping**

1. **Ignore build outputs, but keep the hand-written include/\*-preamble.s sources** — `.gitignore` entries for `*.s`, test/demo binaries, `self/irk*` binaries, `__pycache__`, `*.pyc`, `tmp*.ll`, `*.dylib`, and the key files.

**Foundations (2023 – 2024)**

2. **FFI generator and interface fixes** — `ffi/gen/*`, edits to `ffi/{posix,socket,sodium}.ffi`, the regenerated `*_ffi.scm` files, `self/autoffi.scm`, `*` pointer types in `types.scm`, the `typedef-struct-check` and `csig:obj` support in `c.scm`.
3. **Better compile-time error messages** — `node-get-var` in `analyze.scm`, `:SyntaxError` in `transform.scm`, `:MatchCompilerError` in `match.scm`, `:TypingError` in `typing.scm`, and "incomplete match" listing the missing cases in `cps.scm`. Lands with the `tests/f_*.scm` expected-failure tests.
4. **String, list and OS helpers** — `string-trim-*`, `string-reverse`, `char->utf8`, `zip`/`unzip`, `map-range-rev`, `stat`/`isdir?`/`isfile?`, `path/join`, `file/open-append`, with their tests.
5. **Euclidean division** — the `divmod` family in `lib/core.scm`, with `/` and `mod` switched to Euclidean semantics. A commit of its own because it changes existing behavior. Comes with `tests/t_divmod.scm` and `c/divmod.c`.
6. **New libraries** — `deque`, `bignum`, `calendar`, `time`, `mimetypes`, `regex`, `queue`, `getopt`, `rope`, `alist`, `zlib` and `base64` updates, with tests.

**Crypto and network (2023)**

7. **ASN.1, PEM, X.509 and signature updates** — `lib/asn1/`, `lib/crypto/{pem,x509,sig,sodium,ctbig}.scm`, the BearSSL bindings, and their tests.
8. **P-256** — `lib/crypto/p256.scm`, `ffi/p256*`, and `tests/t_p256*`. Depends on the `p256/` decision in the previous section.
9. **TLS 1.3 updates** — `doom/tls/{codec,kex,signer,tls13}.scm`.
10. **SSH server** — `doom/ssh/` (no keys) and `doom/test/t_ssh.*`.
11. **Crypto command-line demos** — `demo/crypto/` (`pemtool`, `x509tool`, `genpkey`).

**HTTP/2 server (2024)**

12. **Move DNS codec into the library** — `lib/net/dns.scm`, with `doom/dns.scm` reduced to the resolver.
13. **doom runtime and HTTP/2 updates** — `doom/{scheduler,sync,echo}.scm`, `doom/http/{h2,hpack,hpack2}.scm`, `lib/net/socket.scm`, and `t_h2`/`t_hpack*`.
14. **HTTP/2 file server demo** — `demo/h2/`, `demo/maze/h2/`, and `TODO.md`.

**Native backends (Sep – Nov 2024)**

15. **Runtime support for native code** — `include/header1.c`, `gc1.c`, `irken.h`, `rdtsc.h`, and `vm/irkvm.{c,h}`: `irk_buffer_copy`, `out_of_range_exception`, `dump_lenv`, `SYMBOL_HEADER`, and the arm64 cycle counter.
16. **Compiler plumbing for native targets** — platform detection and `platform-configure` in `context.scm`, `%%platform` in `transform.scm`, register allocation and the liveness fix in `cps.scm`, constructors as calls in `nodes.scm`, shared emitters in `backend.scm`, `self/asmlits.scm`, and the `-np` flag. LLVM is commented out here.
17. **arm64 backend** — `self/arm.scm`, `self/irkarm.scm`, `include/arm-{elf,mach}-preamble.s`, the `-arm` flag, the `%backend arm` blocks in `lib/core.scm`, and the `tests/t-*.scm` suite it was developed against.
18. **amd64 backend** — `self/amd.scm`, `self/irkamd.scm`, `include/amd-{elf,mach}-preamble.s`, `-amd`, and amd-specific tests (`t-inline-amd0`, `t-shift-amd`).
19. **riscv64 backend** — `self/rv.scm`, `self/irkrv.scm`, `include/rv-preamble.s`, `-rv`, and `t-int-cmp-rv`.
20. **Bootstrap and install for native compilers** — `util/bootstrap2.py`, `util/install2.py`, the `irkenb.scm` cleanup, `self/flags.scm`, `util/run_tests.py`, and the new `self/bootstrap.byc`.

**Leftovers**

21. **Demo and test touch-ups** — the remaining one- and two-line edits across `demo/` and `tests/`.
22. **JSON float literals** — the May 2026 change, once fixed (see below).

Commits 16–19 need hunk-level splitting. `compile.scm`, `context.scm`, `nodes.scm`, `transform.scm` and `lib/core.scm` each hold lines for all three backends, so each backend's lines go into that backend's commit.

## Risks and open questions

Decided Oct 25: drop LLVM in its own commit, keep Euclidean division as its own commit, leave p256/ out for now, and backdate commits from file times. The rest are known limits of the plan.

**Decisions for you:**

- **Is dropping LLVM intentional?** `self/llvm.scm` is commented out of `compile.scm`, `context.scm` and `transform.scm`, and `LLVM_TRIPLE` is gone from `flags.scm`. If it's temporary, commit 16 should say so; if it's permanent, `llvm.scm` and `include/preamble.ll` could be removed in a separate commit.
- **Should `/` and `mod` change meaning?** Making them Euclidean changes results for negative operands in every existing program. Keeping it as its own commit (5) makes it easy to find and revert, but the message should state the reason.
- **Where does `p256/` belong?** See the previous section.

**Known bug:** the May 2026 JSON float support stores the digits after the decimal point as an integer, so `0.05` and `0.5` both parse as `json:float 0 5` and print as `0.5`. Its regex also overlaps `NUMBER`: `-?[0-9]+(\.[0-9]+)?` matches plain integers too, so `FLOAT` may capture them depending on lexer priority. Simple fixes are storing the fraction as a string, or making the fraction part required.

**Limits of the plan:**

- **Not every commit can be test-built.** Your computer's Linux VM is arm64 with `gcc` but no `clang`, so I can build the C backend and check the arm64 backend on ELF at each step. macOS (Mach-O) and FreeBSD checks, plus amd64 and riscv64 runs, need your machines or emulators. The plan aims for each commit to build, but I'd confirm only what I can run.
- **The bootstrap is self-referential.** `self/bootstrap.byc` grew from 403 KB to 497 KB and depends on runtime changes like `SYMBOL_HEADER`. It belongs in the same commit as, or right after, the runtime changes it needs, or the VM bootstrap will break mid-history.
- **Debug leftovers.** Some changes are commented-out `printf` tracing (in `graph.scm`, `analyze.scm`, `typing.scm`) or marked `XXX unused` (`cint->c`, `ctype->c` in `c.scm`). I'd leave them in the commit they arrived with and note them, rather than editing your code during archaeology. Say if you'd rather they were dropped.
- **Reasoning I can't recover.** Where the code doesn't say why (for example, the `< 7` register limit in `safe-for-let-reg`, which already carries an `XXX need to do some testing` note), the commit message will say what changed and flag the why as unknown.

## Next steps

Once you've answered the three questions above, the work goes in this order. Nothing touches your working copy until step 3.

- [ ] Answer the LLVM, division and `p256/` questions, and mark up the commit plan if you'd like a different grouping
- [ ] Confirm this checkout is the only one with uncommitted work, and whether commits should be backdated
- [ ] I make a backup (a `git bundle` plus a tarball of the untracked files) inside the folder, then create the `native-backends` branch
- [ ] I write each commit with its full message and pause after commit 6 so you can check the tone and level of detail
- [ ] I build-test what the Linux VM can run, and list what needs your Mac or FreeBSD box
- [ ] You review the branch, then push `ffi-cleanup` and `native-backends` to GitHub

## Where to go next

Once the history is committed, five long-term goals remain. Suggested order: the bootstrap VM first, then the value restriction, error reporting, a generational GC, and finally modules or type classes. Each step makes the next one safer.

1. **Give the VM one job: bootstrapping.** Build an `irkenb` that avoids FFI entirely, as `genffi` does, so the bootstrap never touches header scanning or the sizeoff table. That closes the class of bug behind the Linux `malloc(0)` crash and keeps the small `.byc` portable. Full-FFI VM use for development stays best-effort.
2. **Implement the value restriction.** Only generalize syntactic values: lambdas, constants, and constructors applied to values. Garrigue's relaxed variant keeps more polymorphism by still generalizing covariant type variables. Expect some library code to need eta-expansion; the test suite will show where. This closes the soundness hole that functions-in-records keeps exposing, and modules or type classes would build on it.
3. **Finish the error-reporting work.** Commit 3 starts it: exceptions carry source context, and incomplete matches list the missing cases. The next step is the `TODO.md` idea: make every node from a macro expansion inherit its call site's source, so messages stop pointing at copied code.
4. **Add a generational GC.** CPS allocates many short-lived continuations, so a copying nursery would collect most garbage cheaply. The cost is a write barrier on mutation of older objects. The compiler emits every mutation primitive, so a card-marking barrier is a few instructions in each of the three asm backends and the C backend.
5. **Choose modules or type classes.** Whole-program compilation favors SML-style modules: as in MLton, functors can be elaborated away at no runtime cost, and they stay clear of row-polymorphic records. Type classes would also fit through monomorphization, but they interact awkwardly with row types.

**Debugging native code** doesn't have to wait on the VM. The asm backends could emit `.loc`/`.file` directives so lldb and gdb show Irken source lines. A small ring buffer of recent function entries, printed with `dump_lenv` by a fatal-error hook, would stand in for backtraces, which CPS makes meaningless.
