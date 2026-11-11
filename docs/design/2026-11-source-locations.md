# Irken: source locations in s-expressions

*November 2026.  Implemented: the `srcloc` branch (merged Nov 10), then
`srcloc-match` and `issue-sweep`, which gave locations to the errors the
Status section below still lists as missing.*

Give every arm of the `sexp` datatype a `src` field, made by the reader and carried through macro expansion, `transform` and the nodes, so every compile error can name a file, line and column. It reuses the `src` chain Irken already has, and the type checker finds every place the change touches.

## Problem

Today only `sexp:symbol` carries a source location, so anything the compiler builds from a form without a symbol at hand reports `<no-src-location>`. Getting a location onto every s-expression is the goal; the rest of this note is how.

What is missing today:

- **Lists.** A call or special form has a location only through its operator symbol, and only where the code that builds the node remembers to fetch it.
- **Literals.** Strings, numbers, characters, `#t`/`#f` and vectors carry nothing, so an error on `(+ 1 "x")` cannot point at `"x"`.
- **Forms the compiler makes.** `transform` builds `let`, `lambda`, `%%fatbar` and friends from nothing, and their nodes have no location unless one is copied in by hand.
- **Type errors.** The unifier reports the node it failed on; when that node came from a macro expansion or an inlined call, the location is often the null one.

What a good error needs: the file, line and column of the innermost form at fault; the chain of macro expansions and inlined calls that produced it (the existing `src:macro` / `src:inline` arms); and enough of the source text to show the offending line.

## Prior attempts

Three designs exist; the proposal below keeps the typed approach of nirken and the `src` chain Irken already has, and drops gaz's wrapper forms.

| Attempt | Where the location lives | Kept | Dropped |
| --- | --- | --- | --- |
| Irken today (symann, Jan 2023) | `(sexp:symbol name src)` only; `src` is `:nil`, `:base`, `:inline name src src`, `:macro name src src` | the `src` chain, already threaded through `mbe.scm` and `analyze.scm` inlining | symbols-only coverage |
| gaz | `(%ann (src path line pos) exp)` wrapped around every form, as plain sexp data | every form annotated; macros can see annotations | the wrappers: each pass must unwrap them (`unpack-ann`, `ann-list?`, `keep-one-annotation`), and a missed unwrap falls through to another match arm silently |
| nirken (May–Jun 2026, unfinished) | a new `irsexp` type with an `irloc` on every arm; a lexer, a parser and a lowering pass | a location on every arm, checked by the type system | a second sexp type and a separate `irloc` chain parallel to `src`; never connected to macros or `transform` |

nirken also added structured arms for `foo.x`, `foo[i]`, `dt:alt` and `:alt`, lowered after macro expansion. That is a separate question (see Open questions); this change keeps today's representation of those forms.

## Proposal

Every arm of `sexp` gets a `src` as its last field, and the existing `src` type is the only location type. Putting it last means most patterns change by one trailing `_`.

```
(datatype sexp
  (:list (list sexp) src)
  (:symbol symbol src)            ;; unchanged
  (:string string src)
  (:char char src)
  (:bool bool src)
  (:int int src)
  (:undef src)
  (:vector (list sexp) src)
  (:record (list field) src)
  (:cons symbol symbol src)       ;; dt:alt
  (:attr sexp symbol src)         ;; x.name
  )

(datatype field
  (:t symbol sexp src)            ;; name=value; src at the name
  )

(datatype src
  (:nil)
  (:base {path=string line=int pos=int})
  (:inline symbol src src)
  (:macro symbol src src)
  )
```

Rules that come with it:

- **Location means the first character.** A list's `src` is its open paren; a symbol's, its first letter. Spans (an end position) are left out until an error message needs one.
- **Equality ignores `src`.** `sexp=?`, `repr` and printing never look at it, so two reads of the same text are equal.
- **Construction stays short.** The `sexp` builder macro fills in `(src:nil)`. A new `(sexp/at src ...)` builds a list carrying a given location, and `(sexp/src x)` reads one off any arm.
- **The type does the auditing.** Every pattern and constructor that misses the new field is a compile error, so nothing is silently skipped.

Cost: one word per node, plus one `src:base` record per token the reader makes. Nothing at run time changes for programs that never use `sexp`.

## How locations flow

One rule covers every stage: a form built from another form inherits that form's `src`, and only text the reader saw gets a `src:base`.

1. **Reader** (`lib/read.scm`). Every datum gets the `src:base` of its first character. `'x` becomes `(quote x)`, with the list and the `quote` symbol at the quote mark. `foo.x`, `dt:alt` and `{...}` take the location of where they start.
2. **Macro expansion** (`mbe.scm`). Matching ignores `src`. In the output, pieces bound to pattern variables keep the caller's locations; pieces from the template keep the template's. `annotate-macroexpansion` then wraps every `src` in the output, on all arms now rather than symbols only, as `(src:macro name call-site original)`. It keeps today's rule of not repeating the same macro name in a chain (`let` expanding into `let`).
3. **Transform** (`transform.scm`). Each rewrite (`if` without else, `set!` on a field, `let` into `let-splat`, `define` into `function`) builds its result with `sexp/at` and the original form's `src`. This replaces today's scattered `symsrc` and `sexp1src`.
4. **Nodes** (`nodes.scm`). Every `node/...` constructor takes its form's `src`. Today `node/literal`, `node/fix`, `node/let` and `node/nvcase` pass `(src:nil)`; they would get the literal's or the form's location.
5. **Inlining** (`analyze.scm`). Unchanged: an inlined body's nodes get `(src:inline fun body-src call-src)`.
6. **Typing and later passes.** Unchanged in shape: errors take `noderec->src` of the node at fault, which now always has one unless the compiler made the node from nothing.

Quoted data stays data: `quote` turns its argument into a literal, and the `src` is dropped there. Programs that read sexps at run time (metadata, `genffi`, `csi`) get real locations from the reader, which their own errors can use.

## Error messages

Errors use the GNU `file:line:col:` form, so Emacs's compilation mode and other editors jump straight to them, followed by the source line and the expansion chain.

```
test/core/f_example.scm:12:16: type error: expected int, got string
   12 |   (printn (+ 1 "x"))
      |                ^
  in expansion of macro `when` at test/core/f_example.scm:10:3
  in `bar`, inlined at test/core/f_example.scm:30:5
```

- **Columns are 1-based**, as the GNU convention and editors expect. The reader's `pos` today is not; it changes.
- **The source line** is read back from the file when the error is printed; the reader keeps no text.
- **The chain** prints innermost first, one line per `src:macro` or `src:inline` step, cut off at `-sl` lines as today.
- **`(src:nil)`** prints as `<no source location>` and should become rare enough to treat as a compiler bug worth fixing.

## Migration

About 550 uses of the changed `sexp` arms need a trailing field, in 35 files; the compiler's type checker finds each one, and source syntax does not change, so the bootstrap needs no special steps.

| Area | Files | Uses | Notes |
| --- | --- | --- | --- |
| Compiler front end | `transform`, `nodes`, `mbe`, `match`, `typing`, `types`, `compile`, `cps`, `analyze`, `autoffi` | 224 | where locations matter; most of the real work |
| Compiler backends | `amd`, `arm`, `rv`, `c`, `bytecode` | 82 | inline-asm and `%%cexp` templates; a trailing `_` each |
| Reader and sexp library | `lib/read.scm`, `lib/sexp.scm` | 81 | the reader makes locations; helpers gain `sexp/at` and `sexp/src` |
| Other libraries | `ctype`, `dfa/lexicon`, `asn1/berexp`, `getopt`, `parse/*`, `metadata`, `csi`, `dfa/charset` | 81 | mostly build sexps through the `sexp` macro, which needs no change |
| doom, demos, ffi | `http/html`, `tls/codec`, `ssh/codec`, `tls13`, `repl/eval`, `antlr2*`, `genffi` | 49 | same |
| `lib/lisp_reader.scm` | 1 | 32 | the old reader; retire it rather than update it |

Bootstrap: the existing `bootstrap.byc` reads the new source unchanged and builds stage0. As with #86, a new `bootstrap.byc` may be needed for stage1 and stage2 to match.

Outside this repo, gaz, irken-tdfa and irken-h2d use `sexp` too and will need the same trailing fields once they move to the new library.

## Plan

Four steps on one branch, each its own commit; the first two change no behavior and must leave the test suite and the bootstrap fixpoint exactly as they were.

1. **Datatype and reader.** `lib/sexp.scm` gets the new arms and the `sexp/at` and `sexp/src` helpers; `lib/read.scm` records a location on every datum, with 1-based columns.
2. **Mechanical pass.** Every pattern and constructor in the compiler, libraries, doom, demos and `genffi` gets its field, until the bootstrap and test suite pass unchanged. Retire `lib/lisp_reader.scm`.
3. **Thread the locations.** `mbe` annotates all arms; `transform` builds with `sexp/at`; `nodes` passes real locations where it passes `(src:nil)` today.
4. **Messages.** The `file:line:col:` format, the source line with a caret, and the chain.

How success is measured: before step 1, collect every program that is meant to fail to compile (`tests/f_*.scm`, the `f_` tests under `test/`, plus one new case for each error kind that lacks one). Record which errors print `<no-src-location>` or point at the wrong form. After step 4, none should, and each becomes an `expect-error` test that checks the location.

### Decisions

| Question | Decision |
| --- | --- |
| Structured arms for `foo.x`, `foo[i]`, `dt:alt` | Later, as its own change. |
| Spans | Start only: the end can always be found by walking the source from the start. |
| Record field names | Each field gets its own location, `(field:t name value src)`, so errors can point at the field itself. |
| gaz | Left as it is for now. |

## Status

Done on branch `srcloc` (4 commits on master `ccded68`), with steps 3 and 4 in one commit since they touch the same code. Of 46 programs that fail to compile, 41 now report `file:line:col: error:` with the source line, up from none; the bootstrap reaches a fixpoint and the test suite passes on arm64, x86 and riscv.

| Step | Commit |
| --- | --- |
| 1–2: every arm carries a `src`; mechanical pass | `f9a3d07` |
| Reader records locations, 1-based columns | `09a0666` |
| 3–4: locations threaded through mbe, transform, nodes; `report-error` | `745a0bc` |
| `test/errors`: 19 programs that check where each error lands | `32e76a4` |

Still without a location: an unknown constructor (`t:c`), an unknown datatype in a type (`KeyError`), an over-exhaustive match, and two internal errors (`car`, `%match-error`).

Worth improving next: a type error names both types (`int vs. string`) but not which is expected, and a missing record field reads `abs vs. (pre t276)`.
