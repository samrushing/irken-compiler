
The Goal: An FFI that can be safely and efficiently used by the three backends.

Possible Approaches
-------------------

  * ctypesgen (which is used for this purpose with Lua)
  * clang -cc1 -fdump-record-layouts
  * write a parser for C includes. (avoid).
  * code generation.

Right now, I'm leaning toward using clang, since it is likely to be present, and looks to be easy to parse.

Input
-----

At a minimum, we need the names of needed structures, and the functions we wish to expose.

For example:

This is the current interface to inet_pton/ntop from doom/socket.scm:

    (define (inet_pton af ascii buf)
      (syscall
       (%%cexp (int string (buffer (struct sockaddr_in)) -> int)
    	   "inet_pton (%0, %1, &(%2->sin_addr))"
    	   af ascii buf)))

    (define (inet_ntop af buf)
      (let ((ascii (make-string 100)))
        (%%cexp (int (buffer (struct sockaddr_in)) string int -> int)
    		   "inet_ntop (%0, &(%1->sin_addr), %2, %3)"
    		   af buf ascii (string-length ascii))
        ;; should strip this to NUL
        ascii))

Here we need the definition of (struct sockaddr_in).  It would be nice if we could access the various fields:

    $ clang -E -I./include ffi/t0.c | clang -cc1 -fdump-record-layouts
    [...]
    *** Dumping AST Record Layout
             0 | struct sockaddr_in
             0 |   __uint8_t sin_len
             1 |   sa_family_t sin_family
             2 |   in_port_t sin_port
             4 |   struct in_addr sin_addr
             4 |     in_addr_t s_addr
             8 |   char [8] sin_zero
               | [sizeof=16, align=4]

This covers many of the features we will need: sub-structures, arrays.

Now one thing missing from this output are the typedefs.  Here, we have no idea what `sa_family_t` is unless it is a
struct.  Maybe we should consider looking at llvm output instead?

Hmm... this doesn't help us, we lose the field names:

    %struct.sockaddr_in = type { i8, i8, i16, %struct.in_addr, [8 x i8] }
    %struct.in_addr = type { i32 }

    @ob0_size = global i64 16, align 8
    @ob0 = common global %struct.sockaddr_in zeroinitializer, align 4
    @ob1 = common global i8 0, align 1


Maybe we can do this: if we see `mystery_thing_t` we can assume it is an unsigned integer of the size indicated by its
size?  [will alignment rules throw this off?]

For example, `sa_family_t` has a size of 1 byte above, so it is most likely a `char` or `uint8_t`.  And indeed, in
`/usr/include/sys/_types/_sa_family_t.h` I find it is a `uint8_t`.  Same procedure shows that `in_port_t` is a
`uint16_t`.


Sample Function
---------------

Let's start with `inet_ntop`:

    char * inet_ntop (int af, void * src, char * dst, socklen_t size);

Actually that signature kinda sucks, because it is 'c polymorphic'.

Let's instead pretend that it is:

    char * inet_ntop (int af, struct in_addr * src, char * dst, int size);


Getting Function Signatures
--------------------------

The clang-query tool seems also very useful.

    $ clang-query ifaces.c

    clang-query> set output print
    clang-query> match functionDecl()
    Match #47:

    Binding for "root":
    extern int inet_net_pton(int __af, const char *__cp, void *__buf, size_t __len) __attribute__((nothrow))

However it is not present on all systems with clang (XCode doesn't seem to provide it).

     int kqueue(void);

     int kevent(int kq, const struct kevent *changelist,
                int nchanges, struct kevent *eventlist,
                int nevents, const struct timespec *timeout);

Considering that many libc/posix api's use void* for polymorphism, we are probably going
to need to either declare funs with specific types, or have some kind of casting mechanism.

We now have the ability to describe these structures.  Once we can build and ref/set them,
we need to be able to call the functions. (anything to be done about variadic funs?)

Another thing: we want all ctype info to be processed at compile-time, not runtime.
In this case we will need to replace the current %%cexp abuse with a real %ffi that
knows about ctypes.

Processing -fdump-record-layouts
--------------------------------

What information do we need?

For this sample problem - interfacing with `struct sockaddr` - we need:

 * the size and layout of all the contained structs.
 * the ability to allocate, reference, and modify the fields in those structs.

`gen.py` can process the output of `dump-record-layouts`.  First, we scan all structs/unions/etc in the output.  Then,
we create a table of each, which includes the offsets of the outermost layer.  Recursively follow any included
structs/unions and create a set of declarations in irken that can be imported by the compiler.

For example, if we have a `sockaddr_in` struct, we need to be able to reach `a.sin_addr.s_addr`, which entails walking
through `sockaddr_in`, finding the `struct in_addr sin_addr` slot, and walking that to find the `s_addr` slot.

The end result of such a lookup will be an offset and size for any path through the data.
Paths can include fields, pointer dereference, and array index.  Then we need ref/set primitives that can write
to a slot, possibly along with automatic data conversion (like currently exists for %%cexp).

so, `gen.py` needs to output this:

    (define s0
      (cdef:struct
       16
       'sockaddr_in
       (LIST (cfield:t 0 'sin_len    (ctype:int 1))
             (cfield:t 1 'sin_family (ctype:int 1))
             (cfield:t 2 'sin_port   (ctype:int 2))
             (cfield:t 4 'sin_addr   (ctype:struct 'in_addr))
             (cfield:t 8 'sin_zero   (ctype:array 8 (ctype:name 'char)))
             )))

Problems
--------

1) For anonymous union/struct, we will have to invent a unique name for each.

2) For unknown datatypes, we have no way of knowing if the size given by the offsets contains padding or not.  On
   little-endian arch, we can access a padded smaller int without any issues, but on big-endian this will not work.


'or' types
----------

Looking at the `sockaddr` struct: the standard socket interfaces declare the parameter as `struct sockaddr`
but require that it be case to an address-specific struct.  We could just use some kind of `cast` operator,
or we could try to be better about it and specify the set of structs that will work with an `or` type:

    (* (or (struct sockaddr) (struct sockaddr_in) (struct sockaddr_in6))

Code Generation
---------------

One big problem with the current approach: bytecode will no longer be portable.  Since the bytecode generated
will include offsets into structures, and different constants, etc... it will have to be re-generated for
each platform.  We *could* declare a core feature set that's not dependent on FFI (so we can minimally ship
a bootstrap compiler).  Or...

Another possible approach: generate 'extension module' like interfaces that can then be called in a type-safe
way from the three backends.  Again, we will need the ability to create, ref, and set structures/buffers/etc.
With the VM it introduces a new problem: do we link these with the VM, or do we generate shared libraries?
[the latter can be a *real pain* to do cross-platform].

What might this look like from a bytecode perspective?  We would no longer malloc a fixed number of bytes, instead
we will ask to create a structure named `xxx`.  References to fields will also have to be done by name.

What might the extension module code look like?

Loadable Descriptors
--------------------

Another approach to the 'portable bytecode' problem: generate an
interface descriptor file that can be loaded at run time from the file
system, which is then used to generate functions for make/ref/set/call.

This would require the VM to find and load a file from the filesystem
(probably /usr/local/lib/irken/).  This will saddle any ffi-using program
with the lisp reader.  Might we use a simpler lisp reader just for this
purpose?  Or a binary file format with an even *simpler* format?  Hmmmm...
I think sexp is nearly as simple as something could get.

[this might be an eventual use for an asn1-ber facility]

Then we could use closures for ref/set of structures.  For example:

    (let ((desc0 (lookup-descriptor 'sockaddr_in6 'in6_addr 'in6_addr '__u6_addr32)))
      ...)

Hah, we can't do file i/o without the FFI, unless we build something very
simple into the VM. (done: 'readf' opcode).

The current plan: the spec file is read at compile-time to manage types.
It is read again at run-time to build the tables allowing make/ref/set of
structs.  This means that at compile-time we must build a `ctype` object
that will be reachable at *run-time*, and pass this to `%callocate`.
Wait.  No.  `%callocate` will have to transform into code that will do
this lookup at runtime.  The `calloc` opcode will have to be passed a
size argument as an integer in a register.  So we need a special form
(or macro?) to do this.

A new issue: with s2n, I've noticed that we don't have a way of handling
opaque structs.  We need a way to take a pointer to a random struct and
wrap it so that it can be passed in a type-safe manner.  We might as well
make this the main mechanism for accessing things that are not in the heap
[e.g. lib/malloc.scm].  Should we use TC_BUFFER for this?

Hmmm... how about this: a `(buffer (struct thing_t))` means the struct is
in the buffer (i.e., the irken heap).  But a `(buffer (* (struct thing_t)))`
means the buffer holds only a pointer.  That's perfect!


Malloc/Free
-----------

I think there's a bit of danger involved in allocating C structures into
the Irken heap.  If any C function remembers such an address across a GC,
then we'll be in trouble.  I think we may need to move to a system where
C structures/pointers/etc are kept in memory managed by malloc.

C pointers should be storable as Irken integers - but we can avoid
losing the highest bit by simply setting/clearing the lowest bit
rather than TAG/UNTAG.  This should all be managed by the type system
and the backends.

I think it should be possible to have a form of weak gc for malloc'd
objects... though it's only just an idea as of yet:  when a buffer is
malloc'd, we store that as a new datatype, with a reference-count like
slot and the swizzled pointer.  Whenever GC copies such an object, it
raises the reference count.  The runtime keeps a map of all such
addresses, and on occasion will sweep through and `free` any that have
generation numbers lower than the current one. [might this require an
stl map, or can we do it within irken?]


Descriptor Plan
---------------

We want the best possible solution in VM-vs-C backends.  In the C backend,
we want a descriptor that accesses a structure element via compile-time
expressions, like this: `x.sockaddr_in6.in6_addr.in6_addr.u6_addr32[0]`,
and on the VM we need to emit a literal describing that path which is used
to compute the offset at runtime.

So I think the cps phase should generate a prim - like %cget/%cset, that
includes that literal as its parameter.  For the C backend, this will result
in a %%cexp-style line.

`TC_BUFFER`: maybe we can continue to use TC_BUFFER exactly as it is? In this
case a TC_BUFFER will hold a 'pointer'?  Or do we want a compile-time type
that holds a raw pointer with the swizzled low bit?

Let's lean toward this: a C pointer is swizzled and the runtime sees it as
an integer.  Next step: testing this representation with a simple FFI.
I need a simple C function that returns a pointer of some kind.  strerror?

Hmmm.. now I'm thinking we need to use TC_BUFFER.  Because there's no way
to pass this kind of pointer arg back into the ffi unless it's a plain old
int.  Maybe we give up on the high bit for pointers?



compile-time vs run-time types
------------------------------

Can we view this as a partial evaluation thing?

For example, say we have a vector of some numeric ctype, like i256.
We can view (vector-ref v 12) as a runtime operation, or we can treat
it (at compile time) as a partial evaluation, returning an lval.
Do we need a special type to represent an lval (i.e., a 'ref' type in other
MLs).

In order to do this at compile time, we need the type system to be able to
unify the types.  So we need a representation of ctypes that works with the
other types.  So maybe we need a 'cref' predicate?  And it's a special 'kind',
like row types.

Then we need to think about the runtime representation of such a thing.
In C the runtime representation is simply a pointer.

Ok, compiler: type representation.

(cref (ctype:array 100 (ctype:int 256 #t)))

Now, we immediately have a problem since the predicate design can't fully model ctype.
Do we need to introduce ctype as a 'kind'?

But let's ignore that for now, assume we've done it.

Then we need dereferencing operators on crefs.  If we create these as
prims, then they *probably* have to be considered a runtime operation.

But we want a compile-time operation, just like C does.  [is this
really true? maybe not: C can compute an offset into an array at
compile-time, but walking pointers requires runtime dereferencing].

[an aside. can we address the VM offset issue like this: compiler
emits pointer arithmetic in abstract-size terms, like "add N *
sizeof-struct to <p>", and sizeof-struct is set at load time?]

---

Let's think more about representing C types in the predicate type language.
The main issue is type variables.  Can they point only directly to a ctype,
or do we want them to point to internals like size-of-array?  I think not.
A type variable can only point directly to a ctype.  Do we need a different
space for these tvars?

So all ctypes must be wrapped with 'cref', and 'cref' becomes the new kind.

So what is the type of malloc?

I think malloc takes two args: a ctype and a size, and always returns
a (cref (ctype:array ...)).  If we deref this array, we end up with
(cref (ctype:int 256 #t)), yes?  Or, we can pass the array ref directly
to llvm vector prims like add, mul, etc.

What about NULL?  The proper way to model this is to consider a
pointer a union type of 'null' or 'not null', and thus always check
for it.  Can we consider NULL an invalid value, that is only
explicitly checked upon dereference?  [does this apply to the return
value from malloc itself?]


----
Can we cheat and provide a mapping from ctype to irken-type?

(:name)
  predicate
(:int size signed?)
  here we could introduce new predicates i8,u8,i16,u16,etc...?
(:array size ctype)
  here we are hosed, since no size.  but C doesn't have that either?
(:pointer ctype)
  predicate
(:struct name)
  predicate predicate
(:union
  predicate predicate

So the only difficulty is around array.
Could we just translate it into pointer?
[XXX we could just treat 'size' as a symbol.  i.e., (array u32 100) where '100' is a symbol/predicate]

Ok, so assume we do this.  Sample code?

    (let ((v (malloc (ctype:int 32 #f) 100)))
      (for-range i 100
        (set! (cref-aref v i) (* i i))))

Is this possible?  Or do we need `set-cref!`?

Now what about an 'obviously weird' type like u256?  We can model this
directly in llvm... but how does it work?  I think we have to do
`getelementptr` to ref each element, then we need to have a temporary
that contains the result.  We can't just use `*`.  We need a function
like `mul-u256-ref`.  And it needs somewhere to put its result.  So
`mul-u256-ref` needs a result cref:

    (mul-ref u256 a b c)

where `c` is the destination, `a` and `b` are the operands.  `c` could be
equal to `a` or `b`, but the llvm would have to do a `store`.

We could theoretically have a `mul` that allocated and returned a place to
store the result... but I think that should be left to macros.

Now, back to the example code.  The `cref-aref` bit feels like a compile-time
thing, but it's not: the index `i` is runtime.  So all we know at compile-time
is that `cref-aref` returns a `(cref u32)`.

llvm note
---------
llvm has vector versions of add/mul/etc, but llvm distinguishes between
a vector (of fixed size) and an array.  Those insns only work on vectors.
Though I'm sure it's possible to cast.


buf vs malloc
-------------

This is something I've been struggling with for a while.  It'd be nice
to be able to put C types onto the irken heap.  But if we model lvals
the way I've suggested above, it becomes much more difficult.  We'd
have to then have 'cref' be made of two parts: a base address and an
offset.  Because the GC will move the data around.  And every access
operation will be done via base+offset.  This is suitably difficult
that I think trying to solve all the problems at once means I will
continue to struggle and not make progress.  So let's put the 'buf'
issue aside for a bit, and come back to it.


vm ffi prims
------------

ok, to implement each of these prims, the runtime will
 need to know some sizes.
%malloc - needs to know the size of its argument
%c-aref - needs to know the size of its elements
%cref-get/set-int - int size

%cref-sref: the whopper.
For this, we have a chain of refs starting from some struct.
for each field ref (whether struct or union field), we need
to know the offset.

My plan is this: for every point where we need to know a size,
we add it to a table.  When done, we'll have a list of all needed
unique sizes.  When the bytecode is loaded, the VM fills in all
those sizes (somehow).

The table of integer sizes is filled in when the VM is built.
The other sizes/offsets... how can we do that?

for example:

    (%cref-sref in6_addr.__u6_addr.__u6_addr16 paddr0)

Here, the VM will need to know the offset of `__u6_addr` in `in6_addr`,
and then the offset of `__u6_addr16` in the union.

Can we do this mostly/entirely in irken?  If so, then we need to call
code that will fill in this table _before_ any user code is run.

This is basically following a chain of struct/union, should be easy.

So, in summary: only two 'difficult' problems:

 1) sref (chain of field refs - gives an offset)
 2) aref (size of object)

What does the table look like?  It's a vector of ints.
How do we populate the table?

  prim             output                    input
------------------------------------------------------
'%malloc       sizeof ctype                ctype
'%c-aref       sizeof ctype                ctype
'%cref-sref    offset of field ref chain   (list symbol)
'%cref-get-int sizeof int-type             size
'%cref-set-int sizeof int-type             size

In each of these prims, we make an entry in the cmap.

Note: get/set-int - in most cases we already know the size.
exceptions are for 'int', 'unsigned', 'long', etc...

We _could_ just emit straight code to set these sizes/offsets,
like this:

    (set! ffi-offsets[12] (cref (ctype:struct 'thing) 'field0 'field1))

But on second thought, this is a bad idea, because we have to scan for
  prims while still in sexp form. (in order to insert this code), and
  so we miss the tree shaker.

How about this instead: we build a table that is inserted into the
constant vector, and the size/offset vector is built from information
in that table?

What info is needed in this table, at runtime?  Basically, we need a
full ctype, and a list of symbols.  But the backend doesn't have a
ctype, it has an sexp.  So we need things representable by
irken->ctype?  What does that consist of?

(struct name)
(array <sub>) ;; does not happen in normal C (2d array is an array of pointers to 1d arrays).
(cref type) ;; always pointer-sized

the fixed-size ints we already know (like i8, char, etc), that leaves:

0 short
1 int
2 long
3 longlong
4 pointer

These have predefined name, so we can give them known indices.

So how can we make a single opcode work with both known and unknown sizes?
How about this: if we know the size, then include it as a normal int.
Otherwise, use a negative size, or an offset size, like 50+index, where
the index tells us the size.

So to populate the table we need the sizeof each struct encountered to fill
each table.  [Once this works we can return to 'sref'].

[looking at how to implement this table in the bytecode... I think we need
 a 'bytecode object' struct in the VM in order to support multiple bytecodes
 running at the same time...]

;; data fed to runtime sizeoff-setting code:
(struct thing)             ;; sizeof (struct thing)
(union thing)              ;; sizeof (union thing)
thing.field0.field1        ;; sref thing.field0.field1
integer                    ;; fixed-size int or short|int|long|etc

So the compiler builds a literal to represent this.  Can it build an sexp literal?
[currently, no.  consider something else? list of symbols?]

Ok, getting closer now.  Yes, we can emit sexp literals, using nodes::unsexp.
Now we need 1) a new prim to update the sizeoff table and 2) code to call that
prim at the right time (presumably after all require-ffi are done but before user
code is called.  wouldn't it be nice if the topo sort did that for us automatically?).

Got sexp literal working... problem now is: the point where %malloc is recognized
(and the entry added to sizeoff-map) is *after* the call to emit-literals.  Aiiiieee.

Solved with an extra pass over the cps that scans for prims that have sizeoff info.

Many tests working.

Remaining issue: typedefs.  For example, `time_t` is not required to be any particular
type, size, or sign.  So we can't know anything about a `time_t` until runtime.
So how do we read/write `time_t` refs?

*OR* do we punt on this problem and use `int` in place of `time_t` in the .ffi file?
[will this screw us up when `time_t` is a member of a struct?]

`cref` design
-------------

Here I need to describe exactly what was done:

 1) the sizeoff table
 2) `cref` and TC_FOREIGN.
