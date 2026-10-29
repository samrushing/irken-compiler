//	 -*- Mode: assembler -*-


// [temporary] all dummies to make clang happy

	.global prof_dump
prof_dump:
	ret

// macro definitions for arm64 output

	// load address of label
	.macro lld_addr xreg, label
	adrp \xreg, \label
	add \xreg, \xreg, :lo12:\label
	.endm

	// load contents of label
	.macro ldr_addr xreg, label
	adrp \xreg, \label
	ldr \xreg, [\xreg, :lo12:\label]
	.endm

        // store into label
	.macro str_addr x, val, label
	adrp \x, \label
	str \val, [\x, :lo12:\label]
	.endm

	.macro alloc, x, tc, size
	movz \x, \tc
	movk \x, \size, lsl 16
	str \x, [x26]		// write tag
	mov \x, x26
	// add's immediate is 12 bits (optionally shifted 12): split big ones,
	//   e.g. the environment record of a function with hundreds of locals.
	.if ((\size + 1) * 8) < 4096
	add x26, x26, (\size + 1) * 8
	.else
	add x26, x26, #(((\size + 1) * 8) & 4095)
	add x26, x26, #(((\size + 1) * 8) >> 12), lsl #12
	.endif
	.endm

	.macro topref, dst, index
        ldr_addr \dst, top
	ldr \dst, [\dst, (\index + 2) * 8]
	.endm

	.macro topset, scratch, val, index
        ldr_addr \scratch, top
        str \val, [\scratch, (\index + 2) * 8]
        .endm

	.macro return, scratch
	ldr \scratch, [x28, 24]
	br \scratch
	.endm

	.macro tag, x
	lsl \x, \x, 1
	eor \x, \x, 1
	.endm

	.macro tagw dst, src
        sxtw \dst, \src
        lsl \dst, \dst, 1
        eor \dst, \dst, 1
        .endm

	.macro tagh dst, src
        sxth \dst, \src
        lsl \dst, \dst, 1
        eor \dst, \dst, 1
        .endm

	.macro untag, x
	asr \x, \x, 1
	.endm

	.macro unbool, x
        cmp \x, 6
        cset \x, ne
        .endm

	// header, len32, data
	.macro unstring, x
        add \x, \x, 12
        .endm

	// length of any tuple
        .macro tlen, dst, t
        ldr \dst, [\t]
        lsr \dst, \dst, 16
	.endm

       	// length of a vector (possibly empty)
	.macro vlen, dst, x
        cmp \x, 18
        b.eq L1\@
        tlen \dst, \x
        tag \dst
        b L2\@
	L1\@:
        mov \dst, 1
        L2\@:
        .endm

	// make_foreign
        .macro make_foreign dst, val
        alloc \dst, 0x24, 1
        str \val, [\dst, 8]
        .endm

	// uses x2/x3 as scratch.  (it used x6/x7, but those are also
	// irken registers 18/17 and C argument registers 6/7.)  x2/x3
	// are runtime scratch; emit-c-call converts arguments in x0
	// before it loads x1..x7, so they're free here.

	.macro unforeign src
          mov x3, \src
          ldp x2, \src, [\src]
          asr x2, x2, 16
          cmp x2, #1
          beq L1\@
          ldr x3, [x3, 16]
          add x3, \src, x3, asr 1
          add \src, x3, 8
        L1\@:
        .endm

        .macro heapcheck x
          ldr_addr \x, limit
          cmp x26, \x
          blt L1\@
          bl irk_gc_flip
        L1\@:
        .endm

      	// vlen, but with untagged result
	.macro vlen_ut, dst, x
          cmp \x, #18
          b.eq L1\@
          tlen \dst, \x
          b L2\@
	L1\@:
          mov \dst, #0
        L2\@:
        .endm

        .macro rangecheck vec, index
          vlen_ut x1, \vec
          mov x0, \index
          cmp x0, x1
          blt L1\@
          b out_of_range_exception
        L1\@:
        .endm

        .macro getutag src
          tst \src, 3
          beq L1\@
          lsr \src, \src, 16
          b L2\@
        L1\@:
          ldr \src, [\src]
          and \src, \src, #0xfffc
          sub \src, \src, 40
          lsr \src, \src, 2
        L2\@:
        .endm

	.macro getltag src
        tst \src, 3
        cset \src, eq
        .endm

irk_gc_flip:
        stp x29, x30, [sp, -16]!
        str_addr x1, x28, k
        str_addr x1, x27, lenv
        str_addr x1, x26, freep
        mov x0, 0
        bl gc_flip
        ldr_addr x28, k
        ldr_addr x27, lenv
        ldr_addr x26, freep
	ldp x29, x30, [sp], 16
        ret

irk_gc_for_ensure_heap:
	stp x29, x30, [sp, -16]!
        str_addr x1, x28, k
        str_addr x1, x27, lenv
        str_addr x1, x26, freep
        bl gc_flip
        ldr_addr x28, k
        ldr_addr x27, lenv
        ldr_addr x26, freep
	ldp x29, x30, [sp], 16
        ret


	// assumes TC_USERIMM/TC_USEROBJ (i.e. a standard datatype)
	// Now available in MACRO FORM!!
irk_get_user_tag:
        tst x0, 3
        beq igut0
        lsr x0, x0, 16
        ret
igut0:
        ldr x0, [x0]
        and x0, x0, #0xfffc
        sub x0, x0, 40
        lsr x0, x0, 2
        ret

	// x0 := 0 (for list:nil)
	// x0 := 1 (for list:cons)
	// Now available in MACRO FORM!!
irk_get_list_tag:
	tst x0, 3
        cset x0, eq
        ret

	// x26 = freep
	// x0 = scratch (loop counter)
	// x1 = src, result
	// x2 = scratch for this fun
	// x3 = scratch for this fun
	// assumes: length > 0

	.text
arm_copy_tuple_to:
        mov x3, x26             // save freep
        ldr x2, [x1], #8        // copy header
        str x2, [x26], #8
        asr x0, x2, #16         // tuple length, loop counter
Lictt1:
        ldr x2, [x1], #8        // copy element
        str x2, [x26], #8
        subs x0, x0, #1
        cmp x0, #0
        b.gt Lictt1
        mov x0, x3              // result
        ret

	//// we do this manually because this function allocates.
	//// later - we should look into adding a flag to arm-c-call
	//// that indicates a function touches the free pointer, and
	//// thus saves/restores it to x26.
arm_make_string:
        add x1, x0, 11
        lsr x1, x1, 3           // (n+11)>>3 = string-tuple-length
        lsl x2, x1, 16          // << TAG_MASK
        eor x2, x2, 16          // |= TC_STRING
        mov x3, x26             // save to return
        add x1, x1, 1           // +1 for header
        str x2, [x26]           // store header
        str w0, [x26, 8]        // store slen
        add x26, x26, x1, lsl 3 // bump freep
        mov x0, x3              // result
        ret
