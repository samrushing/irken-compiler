// -*- Mode: assembler -*-


// [temporary] all dummies to make clang happy

	.global prof_dump
prof_dump:
	ret

	.macro kreturn
        movq k(%rip), %rdi
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
        movq freep(%rip), %rcx
        movq %rax, (%rcx)
        movq %rcx, \dst
        addq \size, %rcx
        movq %rcx, freep(%rip)
        .endm

        .macro zero dst
        xorq \dst, \dst
        .endm

        .macro heapcheck
          movq freep(%rip), %rax
          cmpq limit(%rip), %rax
          jl L0\@
          movq $0, %rdi
          callq gc_flip
        L0\@:
        .endm

	.macro rangecheck vec index
        .endm

	// these two are used by nvcase to get the index into a jump table.

	// this one is specific for lists, hence it only checks against TC_NIL (0xa)
        // assumes arg in %rdi aka "c0"
        .macro getltag
        cmpq $10, %rdi
        setne %dil
        movzx %dil, %rdi
        .endm

	// this is for any user datatype, immediate or tuple.
	// assumes arg in %rdi aka "c0"
        .macro getutag
          testb $3, %dil
          je L1\@
          shrq $16, %rdi
          jmp L2\@
        L1\@:
          movzwl (%rdi), %edi
          addq $-40, %rdi
          shrq $2, %rdi
        L2\@:
        .endm

	.macro tlen dst, t
        movq (\t), \dst
        shrq $16, \dst
        .endm

        .macro vlen dst, v
          cmp $18, \v
          je L1\@
          tlen \dst, \v
          tag \dst
          jmp L2\@
        L1\@:
          movq $1, \dst
        L2\@:
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
