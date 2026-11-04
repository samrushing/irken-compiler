# tests

A fresh test suite, organized by area.  (The older `tests/` directory is
kept as it was; many of those predate current Irken.)

    core/      integers, comparison, chars, binding forms
    data/      lists, vectors, strings, symbols, maps (lib/frb)
    types/     datatypes, polymorphic variants, let-polymorphism
    records/   records and row polymorphism
    control/   closures, tail calls, exceptions, continuations, generators
    gc/        allocation heavy enough to force collections

Run them from the top of the tree:

    python3 util/test.py                 # everything, every backend this machine can run
    python3 util/test.py test/records    # one area
    python3 util/test.py -B c,arm -v     # chosen backends, every result listed

The backends are `c`, `b` (bytecode, run with `vm/irkvm`), and the native one
for the machine: `arm`, `amd` or `rv`.  Each test is compiled once per backend,
in a scratch directory.

A test is an ordinary program.  Comments say what to expect:

    ;; => text               a line of expected output, in order
    ;; expect-error: text    must fail to compile, with text in the output
    ;; skip: b -- why        don't run on these backends (or platforms)
    ;; xfail: arm -- why     a known bug: reported, but doesn't fail the run

`f_*.scm` files are tests that must fail to compile.  Note that a program's
exit status comes from its final value, so it isn't checked; output, signals
and timeouts are.

Known bugs (xfail):

* `core/t_bigliteral`: integer constants outside +/-2^61 wrap around at
  compile time (c and native backends; the VM is fine).
* `types/t_polyvar_open`: matching a polymorphic variant with a `_` default
  gives the matched value a closed row, so other variants can't be passed.
* `control/t_getcc_toplevel`: the compiler dies ("car") on a top-level
  `(getcc)`.
