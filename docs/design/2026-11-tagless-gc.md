# Irken: a tagless GC (layouts instead of tags)

*November 2026.  A proposal; nothing is implemented yet.*

The proposal: keep one header word per heap object, but make it an index into
a per-type layout table that says which words are pointers; then drop the tag
bits from immediates.  Ints become plain 64-bit words, floats can be unboxed,
and the same layout table serves a generational GC's write barrier and a
WasmGC backend.  The way there is monomorphization, done in steps that each
pay for themselves.

## Today

Every word the GC sees describes itself:

- **Immediates** carry tag bits in the low bits: an int `n` is `2n+1` (so ints
  are 63 bits: -2^62 to 2^62-1), chars, bools, `#u`, `nil` and nullary
  constructors are tagged with `TC_CHAR`, `TC_BOOL`, `TC_USERIMM`, ...
  (`include/irken.h`).
- **Heap objects** start with a header: a type code in the low `TAGSIZE` (16)
  bits (`TC_TUPLE`, `TC_STRING`, `TC_VECTOR`, `UOTAG(n)` per constructor, ...)
  and the length above it.
- The Cheney collector (`include/gc1.c`) copies an object and scans its words,
  deciding from each word's low bits whether it is a pointer.  Strings and
  buffers are opaque: their contents are never scanned.

That design is simple and generic, and it is why `printn` can print anything.
Its costs: 63-bit ints and the 2^61 constant trouble (fixed in `8d246cd`), a
tag/untag around integer arithmetic, and no unboxed floats, since a double's
bit pattern would look like anything.

## Two different things called "tagless"

| | What goes | Needs | Saves |
| --- | --- | --- | --- |
| **Untagged immediates** | the low tag bits on ints, chars, bools | the GC told, per object type, which words are pointers | 64-bit ints, unboxed floats, the tag/untag instructions |
| **No headers** | the header word on heap objects | the static type of every pointer, at every root (Goldberg 1991; Appel) | one word per object |

The first is the one worth doing.  The header stays, but as a **type index**
into a layout table rather than a self-describing code: that's how MLton does
it, and Irken's `UOTAG(n)` headers are most of the way there already.

## The obstacle: polymorphism

A `(list 'a)` cons cell's `car` may be an int or a pointer, so no fixed layout
describes it.  Ways around it:

- **Monomorphization** (MLton): specialize polymorphic functions and datatypes
  per type, so every slot has a concrete type.  Irken is whole-program and
  already inlines heavily, so code growth should be modest.  Row-polymorphic
  records need care: each row instantiation becomes its own layout.
- **Uniform representation where polymorphic** (#74): box or tag a value only
  when it is stored into an `'a` slot.  Less code growth; two representations
  and the conversions between them.
- **Runtime type passing / reconstruction**: the GC recovers types from frames.
  Not worth it here.

Monomorphization is the best fit, and useful on its own: type annotations like
`('a -> 'a)` could be checked properly (today each inlined copy gets fresh
type variables), and code is specialized.

## Roots

With headers kept, heap objects describe themselves (via the table), so only
the **roots** need typing: the registers live at each GC point.  CPS helps a
great deal: continuations and environments are heap objects, so there is no
native stack to walk.  The compiler already knows the live registers at each
`%ensure-heap` (`k.free` in `cps.scm`); if node types survive into CPS, each GC
point can carry a small root map (a bitmap: which live registers are pointers).

## A generational GC

The same table does double duty:

- **Write barrier only on pointer stores.**  `(set! r.count (+ r.count 1))` on
  an int field needs no barrier; the compiler knows at each store.
- **Card scanning** of old objects needs their layouts: the same table.
- **Nursery copying** is today's Cheney copy, driven by the table.

## Floating point

With tags, the only safe floats are boxed: a raw heap object like a string,
allocated for every intermediate result.  (The other tagged route is NaN-boxing,
as JavaScript engines do, which squeezes pointers and ints into the NaN space:
48-bit pointers, ints no wider than 32 or 51 bits.  Not attractive here.)

With layouts, a `float` slot is simply "not a pointer":

- **Unboxed in records, vectors, constructors and registers**, by type.
- **Boxed only in `'a` slots**, until monomorphization removes those.
- **Backends**: the first version can keep doubles in general registers and
  move them to FP registers for each operation (`fmov`/`movq`); a later one can
  allocate FP registers properly.
- **Front end**: a `float` type, literals in the reader (an `sexp:float` arm,
  now with a location), `+.`-style operators or overloading (another reason to
  think about type classes vs. modules), printing and parsing.

## WebAssembly

Two very different targets:

- **Linear memory with Irken's own GC.**  Wasm locals can't be scanned, so
  roots must live in memory: a register file, much like the amd64 backend's
  `%rbp` file.  Tags or layouts both work; layouts need the root maps above.
- **WasmGC** (structs and arrays managed by the host, standard since Wasm 3.0):
  every struct field must have a static type, with `anyref` and `i31ref` for
  uniform slots.  Monomorphized, layout-typed Irken objects map straight onto
  WasmGC structs: this work is most of what a WasmGC backend needs.  `i31ref`
  is 31 bits, so polymorphic ints would be boxed or limited.

Either way, CPS is an advantage.  Tail calls (`return_call`, also standard in
Wasm 3.0) handle CPS's calls directly, and since continuations are heap data,
`callcc` and Irken's generators work without stack-switching extensions.

## What gives way

- **Generic printing and introspection**: `printn`, `dump_object`,
  `variant->name` and the exception printer read tags at run time.  They would
  become type-directed: generated printers, or names in the layout table.
- **The VM** is generic by nature and would keep tagged values.  Now that its
  job is bootstrapping, that's acceptable: the native and C backends share the
  new representation; the bytecode backend keeps the old one.
- **The FFI** and C runtime code that build or read Irken objects (`include/`,
  `%%cexp` templates) must follow the new layouts.

## A possible order

1. **Layout tables** generated from datatypes, records and the runtime's own
   objects; the GC uses them instead of reading tags, while values stay tagged.
   No behavior change; it tests the plumbing.
2. **Types through CPS**, and root maps at GC points.
3. **Monomorphization.**
4. **Untagged immediates**, slot by slot: 64-bit ints first.
5. **Floats**, unboxed by type.
6. **A generational GC**, with barriers only on pointer stores.

Steps 1 and 2 are needed by the generational GC anyway, so they're not wasted
if the rest waits.

## Open questions

- How much code does monomorphization add to the compiler itself (the biggest
  Irken program), and to compile time?
- Do chars and bools stay tagged (cheap, and printing stays easy) once ints
  are untagged?
- Row polymorphism: one layout per row instantiation, or a dictionary-passing
  fallback for records used polymorphically?
