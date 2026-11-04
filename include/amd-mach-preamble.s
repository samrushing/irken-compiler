// -*- Mode: assembler -*-

// WARNING: there's some non-obvious stuff going on with
//  macros on darwin/amd64.  As best I understand:
//  If your macro takes zero args, then a special syntax
//    change is made that uses the '$' character to indicate
//    positional argument substitution - it applies to $0..$9.
//  Because of this change, in those macros ONLY, you have to
//    double up the '$' character on any immediates, otherwise
//    the output will become very confused.

	.text
	.global _prof_dump
_prof_dump:
	ret

	.macro kreturn
        movq _k(%rip), %rdi
        jmp *24(%rdi)
        .endm

        .macro untag dst
        sarq $1, \dst
        .endm

        .macro tag dst
        shlq $1, \dst
        orq $1, \dst
        .endm

        .macro tagh x
        movzhq \x, \x
        shlq $1, \x
        orq $1, \x
        .endm

        .macro tagw x
        movq \x, %rax
        movslq %eax, \x
        shlq $1, \x
        orq $1, \x
        .endm

        .macro alloc tc, len, size, dst
	// (len << TAGSIZE) | TC
        movq \len, %rax
        shlq $16, %rax
        orq \tc, %rax
        // write header
        movq _freep(%rip), %rcx
        movq %rax, (%rcx)
        movq %rcx, \dst
        addq \size, %rcx
        movq %rcx, _freep(%rip)
        .endm

        .macro zero dst
        xorq \dst, \dst
        .endm

        .macro heapcheck
        movq _freep(%rip), %rax
        cmpq _limit(%rip), %rax
        jl 0f
        movq $$0, %rdi
        callq _gc_flip
        0:
        .endm

	.macro rangecheck vec index
        .endm

	// these two are used by nvcase to get the index into a jump table.

	// this one is specific for lists, hence it only checks against TC_NIL (0xa)
        // assumes arg in %rdi aka "c0"
        .macro getltag
        cmpq $$10, %rdi
        setne %dil
        movzx %dil, %rdi
        .endm

	// this is for any user datatype, immediate or tuple.
	// assumes arg in %rdi aka "c0"
        .macro getutag
        testb $$3, %dil
        je 0f
        shrq $$16, %rdi
        jmp 1f
	0:
        movzwl (%rdi), %edi
        addq $$-40, %rdi
        shrq $$2, %rdi
	1:
        .endm

	.macro tlen dst, t
        movq (\t), \dst
        shrq $16, \dst
        .endm

        .macro vlen dst, v
          cmp $18, \v
          je 0f
          tlen \dst, \v
          tag \dst
          jmp 1f
	0:
          movq $1, \dst
	1:
        .endm

        .macro unstring x
        addq $12, \x
        .endm

        .macro make_foreign x, dst
	pushq \x
        alloc $36, $1, $16, %rdi
	popq \x
        movq \x, 8(%rdi)
        movq %rdi, \dst
        .endm

        .macro unforeign x
        movq \x, %rbx
        callq amd_get_foreign
        movq %rax, \x
        .endm

        .macro unbool x
        cmpq $6, \x
        setne %al
        movsbq %al, \x
        .endm

// need our own version of get_foreign, because
// it is used in a 'register sensitive' area: while
// building arguments for C calls. [so, we can't touch
// rsi/rdi/rdx/rcx/r8/r9 - that leaves rax & rbx only!]

// arg goes in %rbx (not %rdi!) and result is returned in %rax.
amd_get_foreign:
        movq $-65536, %rax
        andq (%rbx), %rax
        cmpq $65536, %rax
        jne .Lgf0
        movq 8(%rbx), %rax
        retq
.Lgf0:
        movq 8(%rbx), %rax
        movq 16(%rbx), %rbx
        sarq %rbx
        addq %rbx, %rax
        addq $8, %rax
        retq
