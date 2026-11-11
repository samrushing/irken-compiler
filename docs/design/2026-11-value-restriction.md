# Irken: the value restriction

*November 2026.  Implemented on the `value-restriction` branch (commit
`c0b3561`, fixes #39).*

A top-level (or `letrec`) binding is generalized only if its init is a
syntactic value and the variable is never assigned.  Anything else keeps a
monomorphic type, which its uses decide.  This closes the soundness hole that
let a program store an int and read it back as a string.

## The hole

`let` never generalized, so it was already safe.  `define` and `letrec`
(a `fix` in the compiler) generalized every binding, whatever its init:

```
(define r {v=(list:nil)})                 ;; r : {v=(list 'a)}, for all 'a
(set! r.v (list:cons 1 (list:nil)))       ;; 'a = int here
(match r.v with (list:cons s _) -> (string-length s) ...)   ;; 'a = string here: crash
```

The same works with a vector, or with `set!` on a global initialized to
`(list:nil)`.

## The rule

A binding in a `fix` is generalized when both hold:

- its init is a **syntactic value**:
    - a function;
    - a constant, except a vector literal (vectors are mutable);
    - a variable;
    - a constructor applied to values (`%dtcon`, `%vcon`, or on the native
      backends a call to a `dt:alt` constructor function);
    - `%int->cref` of a value (`NULL`: a constant pointer).

  Building a record is not a value: records are mutable.
- the variable is **never assigned** (`set!`).

Otherwise the binding's type stays monomorphic.  Irken compiles whole
programs, so there are no "weak" type variables to manage: the uses
throughout the program settle the type.

## The one exemption

`*the-exception-handler*` is assigned by every `try`, but stays
`((rsum 'a) -> 'b)`.  That is sound for the same reason ML's `exn` is: each
exception label's payload type is already fixed in the compiler's global
exception table (`unify-exception-types` in `typing.scm`), and the handler
never returns, so `'b` is never a real value.

(Making the handler monomorphic was tried first: every `try`'s row would have
to unify into one, and `raise` would lose its any-type result.)

## What it took

Nothing in the compiler, the library, doom or the demos relied on the hole,
apart from the exception handler and `NULL` (used at two pointer types by the
zlib code).  It found one real bug: `the-variant-label-map` in
`lib/metadata.scm` was a vector of strings filled with symbols, which
type-checked only because it began as `#()`.

`-dt` lists every binding the rule keeps monomorphic that would otherwise have
been polymorphic; in the compiler itself there are about ten (`the-context`,
`the-poller`, the hpack tables, ...), all fine because their uses agree.

## Related

Type annotations (`: type`) were ignored by the type checker until the
`issue-sweep` branch (#38, #24).  They are now checked against the inferred
type, with fresh type variables per node, so a polymorphic annotation such as
`('a -> 'a)` is checked more loosely than a concrete one.  Typing before
inlining would tighten that.
