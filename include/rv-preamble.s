# -*- Mode: assembler -*-

	.global prof_dump
prof_dump:
	ret

# macro definitions for rv64 output

	.macro kreturn
	ld t0, 24(x27)
	jr t0
	.endm

        .macro lld_addr reg, label
	L1\@:
        auipc \reg, %pcrel_hi(\label)
        addi  \reg, \reg, %pcrel_lo(L1\@)
        .endm

        .macro ldr_addr reg, label
	L1\@:
        auipc \reg, %pcrel_hi(\label)
        addi  \reg, \reg, %pcrel_lo(L1\@)
        ld    \reg, 0(\reg)
        .endm

	.macro str_addr val, label
	L1\@:
        auipc t0, %pcrel_hi(\label)
        addi  t0, t0, %pcrel_lo(L1\@)
        sd    \val, 0(t0)
        .endm

        .macro untag reg
        srai \reg, \reg, 1
        .endm

        .macro tag reg
        slli \reg, \reg, 1
        ori  \reg, \reg, 1
        .endm

        .macro alloc x, tc, size
        lui t0, (\size << 4)
        ori t0, t0, \tc
        sd  t0, 0(x25)
        mv  \x, x25
        addi x25, x25, (\size + 1) * 8
        .endm

        .macro bigalloc x, tc, size
        lui t0, (\size << 4)
        ori t0, t0, \tc
        sd  t0, (x25)
        mv  \x, x25
        li  t0, (\size + 1)
        slli t0, t0, 3
        add  x25, x25, t0
        .endm

        .macro topref dst, index
        ldr_addr \dst, top
        ld \dst, (\index+2)*8(\dst)
	.endm

	.macro bigtopref dst, index
        ldr_addr \dst, top
        li t0, \index+2
        slli t0, t0, 3
        add \dst, \dst, t0
        ld \dst, (\dst)
	.endm

        .macro topset val, index
        ldr_addr t0, top
        sd \val, (\index+2)*8(t0)
        .endm

	.macro bigtopset val, index
        ldr_addr t0, top
        li t1, \index+2
        slli t1, t1, 3
        add t0, t0, t1
        sd \val, (t0)
        .endm

        .macro heapcheck x
        ldr_addr \x, limit
        blt x25, \x, L1\@
        jal irk_gc_flip
        L1\@:
        .endm

	.macro getltag src
        andi \src, \src, 3
        slti \src, \src, 1
        .endm

        .macro getutag src
        andi t0, \src, 3
        beqz t0, L1\@
        srli \src, \src, 16
        j L2\@
        L1\@:
        lhu \src, 0(\src)
        addi \src, \src, -40
        srli \src, \src, 2
        L2\@:
        .endm

        .macro unbool reg
        srli \reg, \reg, 16
        .endm

        .macro unstring reg
        add \reg, \reg, 12
        .endm

	.macro tlen dst, tup
        ld \dst, (\tup)
        srli \dst, \dst, 16
        .endm

	# length of a vector (possibly empty)
	.macro vlen dst, v
        li	t0, 18
        beq	\v, t0, L1\@
        tlen	\dst, \v
        tag	\dst
        j	L2\@
        L1\@:
        li	\dst, 1
        L2\@:
        .endm

	# same as vlen, but untagged
	.macro vlen_ut dst, v
        li	t0, 18
        beq	\v, t0, L1\@
        tlen	\dst, \v
        j	L2\@
        L1\@:
        li	\dst, 0
        L2\@:
        .endm

	.macro rangecheck vec, index
        vlen_ut	t1, \vec
        mv	t0, \index
        blt	t0, t1, L1\@
        j	out_of_range_exception
        L1\@:
        .endm

	.macro make_foreign dst, val
        alloc	\dst, 0x24, 1
        sd	\val, 8(\dst)
        .endm

	.macro unforeign src
        mv	t0, \src
        ld	t1, 0(\src)
        ld	\src, 8(\src)
        srli	t1, t1, 16
        li	t2, 1
        beq	t1, t2, L1\@
        ld	t0, 16(t0)
        srai	t0, t0, 1
        add	\src, \src, t0
        addi	\src, \src, 8
        L1\@:
        .endm

	.text

# this has been done both ways - as a function, and as
#   an inline - need to benchmark it.
# return UITAG((a < b) ? 0 : ((b < a) ? 2 : 1));
irk_int_cmp:
        bltu    a1, a0, .Liic2
        lui     a2, 16
        addiw   a3, a2, 22
        li      a2, 22
        bgeu    a0, a1, .Liic3
        j       .Liic4
.Liic2:
        lui     a2, 32
        addiw   a3, a2, 22
        li      a2, 22
        bltu    a0, a1, .Liic4
.Liic3:
        mv      a2, a3
.Liic4:
        mv      a0, a2
        ret

irk_gc_flip:
        addi sp, sp, -16
        sd ra, 8(sp)
        str_addr x27, k
        str_addr x26, lenv
        str_addr x25, freep
        li a0, 0
        jal gc_flip
        ldr_addr x27, k
        ldr_addr x26, lenv
        ldr_addr x25, freep
        ld ra, 8(sp)
        addi sp, sp, 16
        ret

# only difference: a0 is set by the caller.
irk_gc_for_ensure_heap:
        addi sp, sp, -16
        sd ra, 8(sp)
        str_addr x27, k
        str_addr x26, lenv
        str_addr x25, freep
        jal gc_flip
        ldr_addr x27, k
        ldr_addr x26, lenv
        ldr_addr x25, freep
        ld ra, 8(sp)
        addi sp, sp, 16
        ret

rv_make_string:
        addi    sp, sp, -16
        sd      ra, 8(sp)
        str_addr x25, freep
        jal	irk_make_string
        ldr_addr x25, freep
        ld	ra, 8(sp)
        addi	sp, sp, 16
        ret
