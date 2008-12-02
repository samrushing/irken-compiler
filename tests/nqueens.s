	.file	"nqueens.c"
.globl gc_ticks
	.section	.bss
	.p2align 3
	.type	gc_ticks, @object
	.size	gc_ticks, 8
gc_ticks:
	.zero	8
.globl clear_tospace
	.p2align 3
	.type	clear_tospace, @object
	.size	clear_tospace, 8
clear_tospace:
	.zero	8
.globl clear_fromspace
	.p2align 3
	.type	clear_fromspace, @object
	.size	clear_fromspace, 8
clear_fromspace:
	.zero	8
.globl verbose_gc
	.p2align 3
	.type	verbose_gc, @object
	.size	verbose_gc, 8
verbose_gc:
	.zero	8
.globl heap1
	.p2align 3
	.type	heap1, @object
	.size	heap1, 8
heap1:
	.zero	8
.globl heap0
	.p2align 3
	.type	heap0, @object
	.size	heap0, 8
heap0:
	.zero	8
.globl heap_size
	.section	.rodata
	.p2align 3
	.type	heap_size, @object
	.size	heap_size, 8
heap_size:
	.quad	100000
	.text
	.p2align 4,,15
.globl unbox
	.type	unbox, @function
unbox:
.LFB3:
	sarq	%rdi
	movq	%rdi, %rax
	ret
.LFE3:
	.size	unbox, .-unbox
	.p2align 4,,15
.globl box
	.type	box, @function
box:
.LFB4:
	leaq	(%rdi,%rdi), %rax
	orq	$1, %rax
	ret
.LFE4:
	.size	box, .-box
	.p2align 4,,15
.globl is_int
	.type	is_int, @function
is_int:
.LFB5:
	movl	%edi, %eax
	andl	$1, %eax
	ret
.LFE5:
	.size	is_int, .-is_int
	.p2align 4,,15
.globl is_immediate
	.type	is_immediate, @function
is_immediate:
.LFB6:
	movzbq	%dil,%rdx
	xorl	%eax, %eax
	andl	$3, %edi
	cmovne	%rdx, %rax
	ret
.LFE6:
	.size	is_immediate, .-is_immediate
	.p2align 4,,15
.globl string_tuple_length
	.type	string_tuple_length, @function
string_tuple_length:
.LFB7:
	leaq	11(%rdi), %rax
	movl	$8, %edx
	movq	%rdx, %rcx
	cqto
	idivq	%rcx
	ret
.LFE7:
	.size	string_tuple_length, .-string_tuple_length
	.p2align 4,,15
	.type	get_tuple_size, @function
get_tuple_size:
.LFB10:
	movq	(%rdi), %rax
	shrq	$8, %rax
	ret
.LFE10:
	.size	get_tuple_size, .-get_tuple_size
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"  "
	.text
	.p2align 4,,15
.globl stack_depth_indent
	.type	stack_depth_indent, @function
stack_depth_indent:
.LFB14:
	pushq	%rbx
.LCFI0:
	movq	%rdi, %rbx
	jmp	.L15
	.p2align 4,,7
.L17:
	movq	__stderrp(%rip), %rcx
	movl	$2, %edx
	movl	$1, %esi
	movl	$.LC0, %edi
	movq	8(%rbx), %rbx
	call	fwrite
.L15:
	cmpq	$10, %rbx
	jne	.L17
	popq	%rbx
	ret
.LFE14:
	.size	stack_depth_indent, .-stack_depth_indent
	.section	.rodata.str1.1
.LC1:
	.string	"\\0x%02x"
.LC2:
	.string	"..."
	.text
	.p2align 4,,15
.globl print_string
	.type	print_string, @function
print_string:
.LFB15:
	pushq	%r13
.LCFI1:
	movl	%esi, %r13d
	pushq	%r12
.LCFI2:
	movq	%rdi, %r12
	pushq	%rbp
.LCFI3:
	pushq	%rbx
.LCFI4:
	leaq	12(%rdi), %rbx
	subq	$8, %rsp
.LCFI5:
	testl	%esi, %esi
	jne	.L34
.L19:
	xorl	%ebp, %ebp
	cmpl	8(%r12), %ebp
	jae	.L21
	.p2align 4,,7
.L36:
	movzbl	(%rbx), %eax
	cmpb	$34, %al
	je	.L35
	movsbl	%al,%edi
	xorl	%eax, %eax
	call	isprint
	testl	%eax, %eax
	je	.L25
	movsbl	(%rbx),%edi
	movq	__stdoutp(%rip), %rsi
.L33:
	call	fputc
.L24:
	cmpl	$50, %ebp
	jg	.L31
	incl	%ebp
	incq	%rbx
	cmpl	8(%r12), %ebp
	jb	.L36
.L21:
	testl	%r13d, %r13d
	.p2align 4,,2
	jne	.L37
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,7
.L25:
	movsbl	(%rbx),%edx
	movq	__stdoutp(%rip), %rdi
	movl	$.LC1, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L24
.L34:
	movq	__stdoutp(%rip), %rsi
	movl	$34, %edi
	call	fputc
	jmp	.L19
	.p2align 4,,7
.L35:
	movq	__stdoutp(%rip), %rsi
	movl	$92, %edi
	call	fputc
	movq	__stdoutp(%rip), %rsi
	movl	$34, %edi
	jmp	.L33
.L37:
	movq	__stdoutp(%rip), %rsi
	addq	$8, %rsp
	movl	$34, %edi
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	jmp	fputc
.L31:
	movq	__stdoutp(%rip), %rcx
	movl	$3, %edx
	movl	$1, %esi
	movl	$.LC2, %edi
	call	fwrite
	jmp	.L21
.LFE15:
	.size	print_string, .-print_string
	.section	.rodata.str1.1
.LC4:
	.string	"%zd"
.LC9:
	.string	"#u"
.LC8:
	.string	"()"
.LC6:
	.string	"#t"
.LC7:
	.string	"#f"
.LC5:
	.string	"#\\%c"
.LC3:
	.string	"<null>"
.LC14:
	.string	"<unknown object>"
.LC13:
	.string	"#("
.LC12:
	.string	"<tuple\n"
.LC11:
	.string	"<closure pc=%p lenv=%p>"
.LC10:
	.string	"<save pc=%p\n"
	.text
	.p2align 4,,15
	.type	dump_object, @function
dump_object:
.LFB12:
	movq	%r12, -24(%rsp)
.LCFI6:
	movl	%esi, %r12d
	movq	%rbx, -40(%rsp)
.LCFI7:
	leal	-1(%r12), %ebx
	movq	%rbp, -32(%rsp)
.LCFI8:
	movq	%r13, -16(%rsp)
.LCFI9:
	movq	%r14, -8(%rsp)
.LCFI10:
	movq	%rdi, %rbp
	subq	$40, %rsp
.LCFI11:
	jmp	.L89
	.p2align 4,,7
.L94:
	movq	__stdoutp(%rip), %rcx
	movl	$2, %edx
	movl	$1, %esi
	movl	$.LC0, %edi
	decl	%ebx
	call	fwrite
.L89:
	cmpl	$-1, %ebx
	jne	.L94
	testq	%rbp, %rbp
	je	.L95
	testb	$1, %bpl
	.p2align 4,,5
	jne	.L96
	movzbq	%bpl,%rdx
	xorl	%eax, %eax
	testb	$3, %bpl
	cmovne	%rdx, %rax
	cmpl	$14, %eax
	ja	.L44
	mov	%eax, %eax
	jmp	*.L82(,%rax,8)
	.section	.rodata
	.p2align 3
	.p2align 2
.L82:
	.quad	.L61
	.quad	.L44
	.quad	.L53
	.quad	.L44
	.quad	.L44
	.quad	.L44
	.quad	.L54
	.quad	.L44
	.quad	.L44
	.quad	.L44
	.quad	.L59
	.quad	.L44
	.quad	.L44
	.quad	.L44
	.quad	.L60
	.text
.L78:
	movl	$1, %esi
	movq	%rbp, %rdi
	call	print_string
	.p2align 4,,7
.L44:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movl	$262, %eax
	movq	16(%rsp), %r12
	movq	24(%rsp), %r13
	movq	32(%rsp), %r14
	addq	$40, %rsp
	ret
.L96:
	movq	__stdoutp(%rip), %rdi
	sarq	%rbp
	movl	$.LC4, %esi
	movq	%rbp, %rdx
	xorl	%eax, %eax
	call	fprintf
	jmp	.L44
.L60:
	movq	__stdoutp(%rip), %rcx
	movl	$2, %edx
	movl	$1, %esi
	movl	$.LC9, %edi
	call	fwrite
	jmp	.L44
.L59:
	movq	__stdoutp(%rip), %rcx
	movl	$2, %edx
	movl	$1, %esi
	movl	$.LC8, %edi
	call	fwrite
	jmp	.L44
.L54:
	movq	%rbp, %rax
	movq	__stdoutp(%rip), %rdi
	movl	$.LC6, %esi
	shrq	$8, %rax
	testb	%al, %al
	movl	$.LC7, %eax
	cmove	%rax, %rsi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L44
.L53:
	movq	__stdoutp(%rip), %rdi
	sarq	$8, %rbp
	movl	$.LC5, %esi
	movsbl	%bpl,%edx
	xorl	%eax, %eax
	call	fprintf
	jmp	.L44
.L61:
	movzbl	(%rbp), %eax
	subl	$4, %eax
	cmpl	$24, %eax
	ja	.L80
	mov	%eax, %eax
	.p2align 4,,3
	jmp	*.L81(,%rax,8)
	.section	.rodata
	.p2align 3
	.p2align 2
.L81:
	.quad	.L63
	.quad	.L80
	.quad	.L80
	.quad	.L80
	.quad	.L65
	.quad	.L80
	.quad	.L80
	.quad	.L80
	.quad	.L66
	.quad	.L80
	.quad	.L80
	.quad	.L80
	.quad	.L78
	.quad	.L80
	.quad	.L80
	.quad	.L80
	.quad	.L71
	.quad	.L80
	.quad	.L80
	.quad	.L80
	.quad	.L77
	.quad	.L80
	.quad	.L80
	.quad	.L80
	.quad	.L79
	.text
.L95:
	movq	__stdoutp(%rip), %rcx
	movl	$6, %edx
	movl	$1, %esi
	movl	$.LC3, %edi
	call	fwrite
	jmp	.L44
.L80:
	movq	__stdoutp(%rip), %rcx
	movl	$16, %edx
	movl	$1, %esi
	movl	$.LC14, %edi
	call	fwrite
	call	abort
.L79:
	movq	8(%rbp), %rdi
	xorl	%esi, %esi
	call	print_string
	jmp	.L44
.L77:
	movq	%rbp, %rdi
	call	print_list
	.p2align 4,,6
	jmp	.L44
.L71:
	movq	%rbp, %rdi
	xorl	%r12d, %r12d
	call	get_tuple_size
	movq	__stdoutp(%rip), %rcx
	movq	%rax, %r13
	movl	$2, %edx
	movl	$1, %esi
	movl	$.LC13, %edi
	call	fwrite
	testq	%r13, %r13
	jle	.L88
	leaq	-1(%r13), %r14
	xorl	%ebx, %ebx
	jmp	.L76
.L74:
	incl	%r12d
	movslq	%r12d,%rbx
	cmpq	%r13, %rbx
	jge	.L88
.L76:
	movq	8(%rbp,%rbx,8), %rdi
	xorl	%esi, %esi
	call	dump_object
	cmpq	%r14, %rbx
	jge	.L74
	movq	__stdoutp(%rip), %rsi
	movl	$32, %edi
	call	fputc
	jmp	.L74
.L66:
	movq	%rbp, %rdi
	xorl	%r13d, %r13d
	call	get_tuple_size
	movq	__stdoutp(%rip), %rcx
	movq	%rax, %rbx
	movl	$7, %edx
	leaq	-1(%rbx), %r14
	movl	$1, %esi
	movl	$.LC12, %edi
	call	fwrite
	testq	%r14, %r14
	jle	.L86
	leal	1(%r12), %ebx
	xorl	%eax, %eax
.L70:
	movq	16(%rbp,%rax,8), %rdi
	movl	%ebx, %esi
	incl	%r13d
	call	dump_object
	movq	__stdoutp(%rip), %rsi
	movl	$10, %edi
	call	fputc
	movslq	%r13d,%rax
	cmpq	%r14, %rax
	jl	.L70
.L86:
	movq	8(%rbp), %rdi
	leal	1(%r12), %esi
	jmp	.L93
.L65:
	movq	16(%rbp), %rcx
	movq	8(%rbp), %rdx
	movl	$.LC11, %esi
	movq	__stdoutp(%rip), %rdi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L44
.L63:
	movq	24(%rbp), %rdx
	movq	__stdoutp(%rip), %rdi
	movl	$.LC10, %esi
	xorl	%eax, %eax
	leal	1(%r12), %ebx
	call	fprintf
	movq	16(%rbp), %rdi
	movl	%ebx, %esi
	call	dump_object
	movq	__stdoutp(%rip), %rsi
	movl	$10, %edi
	call	fputc
	movq	8(%rbp), %rdi
	movl	%ebx, %esi
.L93:
	call	dump_object
	movq	__stdoutp(%rip), %rsi
	movl	$62, %edi
	call	fputc
	jmp	.L44
.L88:
	movq	__stdoutp(%rip), %rsi
	movl	$41, %edi
	call	fputc
	jmp	.L44
.LFE12:
	.size	dump_object, .-dump_object
	.section	.rodata.str1.1
.LC15:
	.string	" . "
	.text
	.p2align 4,,15
.globl print_list
	.type	print_list, @function
print_list:
.LFB16:
	movq	__stdoutp(%rip), %rsi
	pushq	%rbx
.LCFI12:
	movq	%rdi, %rbx
	movl	$40, %edi
	jmp	.L109
	.p2align 4,,7
.L111:
	movzbq	%bl,%rax
	testb	$3, %bl
	je	.L107
	testq	%rax, %rax
	jne	.L102
.L107:
	cmpb	$24, (%rbx)
	.p2align 4,,3
	jne	.L102
	movq	__stdoutp(%rip), %rsi
	movl	$32, %edi
.L109:
	call	fputc
	movq	8(%rbx), %rdi
	movq	16(%rbx), %rbx
	xorl	%esi, %esi
	call	dump_object
	cmpq	$10, %rbx
	jne	.L111
	popq	%rbx
	movq	__stdoutp(%rip), %rsi
	movl	$41, %edi
	jmp	fputc
.L102:
	movq	__stdoutp(%rip), %rcx
	movl	$3, %edx
	movl	$1, %esi
	movl	$.LC15, %edi
	call	fwrite
	movq	%rbx, %rdi
	xorl	%esi, %esi
	call	dump_object
	popq	%rbx
	movq	__stdoutp(%rip), %rsi
	movl	$41, %edi
	jmp	fputc
.LFE16:
	.size	print_list, .-print_list
	.p2align 4,,15
.globl DO
	.type	DO, @function
DO:
.LFB13:
	subq	$8, %rsp
.LCFI13:
	xorl	%esi, %esi
	call	dump_object
	movq	__stdoutp(%rip), %rsi
	movl	$10, %edi
	call	fputc
	movq	__stdoutp(%rip), %rdi
	addq	$8, %rsp
	jmp	fflush
.LFE13:
	.size	DO, .-DO
	.section	.rodata.str1.8,"aMS",@progbits,1
	.p2align 3
.LC16:
	.string	"panic: Unknown Type Code in GC copy loop: *pp=0x%08x tc=0x%x\n"
	.text
	.p2align 4,,15
	.type	copy.2, @function
copy.2:
.LFB29:
	subq	$8, %rsp
.LCFI14:
	movq	%r10, (%rsp)
	movq	(%rdi), %rsi
	movq	-8(%r10), %r11
	movzbq	%sil,%rdx
	testb	$3, %sil
	movq	%rsi, %r8
	je	.L118
	testq	%rdx, %rdx
	movq	%rsi, %rax
	jne	.L117
.L118:
	movq	heap0(%rip), %rax
	cmpq	%rax, %rsi
	jb	.L123
	addq	$800000, %rax
	cmpq	%rsi, %rax
	jbe	.L123
	movq	(%rsi), %rdx
	cmpq	$-4, %rdx
	je	.L142
	cmpb	$28, %dl
	ja	.L128
	movq	%rdx, %rax
	movq	-24(%r11), %r10
	xorl	%r9d, %r9d
	sarq	$8, %rax
	incq	%rax
	cmpq	$0, %rax
	leaq	8(%r10), %rcx
	jg	.L132
	jmp	.L136
	.p2align 4,,7
.L137:
	movq	(%r8), %rdx
.L132:
	incq	%r9
	movq	%rdx, -8(%rcx)
	addq	$8, %r8
	movq	%rcx, %rdx
	addq	$8, %rcx
	cmpq	%r9, %rax
	jg	.L137
	movq	(%rdi), %rsi
	movq	%rdx, -24(%r11)
.L136:
	movq	$-4, (%rsi)
	movq	(%rdi), %rax
	movq	%r10, 8(%rax)
	movq	%r10, %rax
.L117:
	addq	$8, %rsp
	ret
	.p2align 4,,7
.L123:
	movq	%rsi, %rax
	addq	$8, %rsp
	ret
.L142:
	movq	8(%rsi), %rax
	addq	$8, %rsp
	ret
.L128:
	movq	__stderrp(%rip), %rdi
	movzbl	%dl, %ecx
	movl	$.LC16, %esi
	xorl	%eax, %eax
	call	fprintf
	call	abort
.LFE29:
	.size	copy.2, .-copy.2
	.section	.rodata.str1.1
.LC17:
	.string	"[gc..."
.LC19:
	.string	"collected %d words]\n"
	.section	.rodata.str1.8
	.p2align 3
.LC18:
	.string	"panic: Unknown Type Code in GC scan loop: scan=0x%08x tc=0x%02x\n"
	.text
	.p2align 4,,15
	.type	gc_flip.0, @function
gc_flip.0:
.LFB27:
	pushq	%r15
.LCFI15:
	movq	%rdi, %r15
	pushq	%r14
.LCFI16:
	movq	%r10, %r14
	pushq	%r13
.LCFI17:
	pushq	%r12
.LCFI18:
	xorl	%r12d, %r12d
	pushq	%rbp
.LCFI19:
	pushq	%rbx
.LCFI20:
	subq	$8, %rsp
.LCFI21:
	cmpq	$0, verbose_gc(%rip)
	movq	%r10, (%rsp)
	jne	.L188
.L143:
	cmpq	$0, clear_tospace(%rip)
	jne	.L189
.L144:
	movq	heap1(%rip), %rbp
	movq	-8(%r14), %rax
	movq	%rax, (%rbp)
	movq	-16(%r14), %rax
	leaq	24(%rbp), %r13
	movq	%rbp, %rbx
	movq	%r13, -24(%r14)
	movq	%rax, 8(%rbp)
	movq	-32(%r14), %rax
	movq	%rax, 16(%rbp)
	.p2align 4,,7
.L148:
	leaq	8(%rsp), %r10
	movq	%rbp, %rdi
	incl	%r12d
	addq	$8, %rbp
	call	copy.2
	movq	%rax, (%rbx)
	addq	$8, %rbx
	cmpl	$2, %r12d
	jle	.L148
	movq	%r13, %rbp
.L186:
	movq	-24(%r14), %rcx
.L187:
	cmpq	%rcx, %rbp
	jae	.L178
.L190:
	movq	(%rbp), %rax
	testb	$3, %al
	je	.L151
	addq	$8, %rbp
	cmpq	%rcx, %rbp
	jb	.L190
.L178:
	movq	heap1(%rip), %rax
	movq	heap0(%rip), %rdx
	movq	%rdx, heap1(%rip)
	movq	(%rax), %rdx
	movq	%rax, heap0(%rip)
	movq	%rdx, -8(%r14)
	movq	8(%rax), %rdx
	movq	%rdx, -16(%r14)
	movq	16(%rax), %rdx
	addq	$791808, %rax
	cmpq	$0, clear_fromspace(%rip)
	movq	%rdx, -32(%r14)
	movq	%rax, (%r15)
	jne	.L191
.L172:
	cmpq	$0, verbose_gc(%rip)
	jne	.L192
	movq	-24(%r14), %rax
	subq	heap0(%rip), %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	sarq	$2, %rax
	orq	$1, %rax
	ret
	.p2align 4,,7
.L151:
	movq	%rax, %r13
	movl	%eax, %edx
	movzbl	%al, %eax
	subl	$4, %eax
	sarq	$8, %r13
	leaq	8(%rbp), %rbx
	cmpl	$24, %eax
	ja	.L169
	mov	%eax, %eax
	jmp	*.L170(,%rax,8)
	.section	.rodata
	.p2align 3
	.p2align 2
.L170:
	.quad	.L163
	.quad	.L169
	.quad	.L169
	.quad	.L169
	.quad	.L162
	.quad	.L169
	.quad	.L169
	.quad	.L169
	.quad	.L157
	.quad	.L169
	.quad	.L169
	.quad	.L169
	.quad	.L168
	.quad	.L169
	.quad	.L169
	.quad	.L169
	.quad	.L157
	.quad	.L169
	.quad	.L169
	.quad	.L169
	.quad	.L157
	.quad	.L169
	.quad	.L169
	.quad	.L169
	.quad	.L157
	.text
.L189:
	movq	heap1(%rip), %rdi
	movl	$100000, %edx
	movl	$170, %esi
	call	memset
	jmp	.L144
.L188:
	movq	__stderrp(%rip), %rcx
	movl	$6, %edx
	movl	$1, %esi
	movl	$.LC17, %edi
	call	fwrite
	jmp	.L143
.L192:
	movq	-24(%r14), %rdx
	subq	heap0(%rip), %rdx
	movl	$.LC19, %esi
	movq	__stderrp(%rip), %rdi
	xorl	%eax, %eax
	sarq	$3, %rdx
	call	fprintf
	movq	-24(%r14), %rax
	subq	heap0(%rip), %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	sarq	$2, %rax
	orq	$1, %rax
	ret
.L191:
	movq	heap1(%rip), %rdi
	movl	$100000, %edx
	xorl	%esi, %esi
	call	memset
	jmp	.L172
.L157:
	xorl	%r12d, %r12d
	cmpq	%r13, %r12
	jge	.L168
	.p2align 4,,7
.L161:
	leaq	8(%rsp), %r10
	movq	%rbx, %rdi
	incq	%r12
	call	copy.2
	movq	%rax, (%rbx)
	addq	$8, %rbx
	cmpq	%r13, %r12
	jl	.L161
	movq	-24(%r14), %rcx
.L168:
	leaq	8(%rbp,%r13,8), %rbp
	jmp	.L187
.L169:
	movq	__stderrp(%rip), %rdi
	movzbl	%dl, %ecx
	movl	$.LC18, %esi
	movq	%rbp, %rdx
	xorl	%eax, %eax
	call	fprintf
	call	abort
.L162:
	addq	$8, %rbx
	leaq	8(%rsp), %r10
	addq	$24, %rbp
	movq	%rbx, %rdi
	call	copy.2
	movq	%rax, (%rbx)
	jmp	.L186
.L163:
	leaq	8(%rsp), %r10
	movq	%rbx, %rdi
	movl	$3, %r12d
	call	copy.2
	leaq	8(%rsp), %r10
	movq	%rax, (%rbx)
	addq	$8, %rbx
	movq	%rbx, %rdi
	call	copy.2
	movq	%rax, (%rbx)
	addq	$16, %rbx
	cmpq	%r13, %r12
	jge	.L182
	.p2align 4,,7
.L193:
	leaq	8(%rsp), %r10
	movq	%rbx, %rdi
	incq	%r12
	call	copy.2
	movq	%rax, (%rbx)
	addq	$8, %rbx
	cmpq	%r13, %r12
	jl	.L193
.L182:
	leaq	8(%rbp,%r13,8), %rbp
	jmp	.L186
.LFE27:
	.size	gc_flip.0, .-gc_flip.0
	.p2align 4,,15
	.type	allocate.3, @function
allocate.3:
.LFB30:
	movq	%r10, -8(%rsp)
	movq	-24(%r10), %rcx
	movq	%rsi, %rax
	salq	$8, %rax
	andl	$255, %edi
	movq	%r10, %r8
	orq	%rdi, %rax
	movq	%rcx, %rdx
	movq	%rax, (%rcx)
	jmp	.L200
	.p2align 4,,7
.L201:
	movq	%rdx, %rax
	addq	$8, %rdx
	movq	$10, 8(%rax)
.L200:
	decq	%rsi
	cmpq	$-1, %rsi
	jne	.L201
	leaq	8(%rdx), %rax
	movq	%rax, -24(%r8)
	movq	%rcx, %rax
	ret
.LFE30:
	.size	allocate.3, .-allocate.3
	.p2align 4,,15
	.type	check_heap.6, @function
check_heap.6:
.LFB31:
	pushq	%rbx
.LCFI22:
	subq	$16, %rsp
.LCFI23:
	movq	%r10, 8(%rsp)
	movq	-40(%r10), %rax
	cmpq	%rax, -24(%r10)
	jae	.L206
	addq	$16, %rsp
	movl	$262, %eax
	popq	%rbx
	ret
	.p2align 4,,7
.L206:
#APP
	rdtsc
#NO_APP
	leaq	-40(%r10), %rdi
	salq	$32, %rdx
	mov	%eax, %ebx
	orq	%rdx, %rbx
	call	gc_flip.0
#APP
	rdtsc
#NO_APP
	mov	%eax, %eax
	salq	$32, %rdx
	orq	%rdx, %rax
	subq	%rbx, %rax
	addq	%rax, gc_ticks(%rip)
	addq	$16, %rsp
	popq	%rbx
	movl	$262, %eax
	ret
.LFE31:
	.size	check_heap.6, .-check_heap.6
	.p2align 4,,15
.globl vm
	.type	vm, @function
vm:
.LFB19:
.L208:
.L240:
.L260:
	pushq	%r13
.LCFI24:
	movl	$3, %esi
	movl	$4, %edi
	pushq	%r12
.LCFI25:
	pushq	%rbp
.LCFI26:
	pushq	%rbx
.LCFI27:
	subq	$40, %rsp
.LCFI28:
	movq	heap0(%rip), %rax
	leaq	40(%rsp), %r10
	movq	$10, 32(%rsp)
	movq	$10, 24(%rsp)
	movq	$10, 8(%rsp)
	movq	%rax, 16(%rsp)
	addq	$791808, %rax
	movq	%rax, (%rsp)
	call	allocate.3
	leaq	40(%rsp), %r10
	movl	$3, %esi
	movl	$12, %edi
	movq	$10, 8(%rax)
	movq	$10, 16(%rax)
	movq	$.L207, 24(%rax)
	movq	%rax, 24(%rsp)
	call	allocate.3
	movq	%rax, %r12
	movq	%rax, 8(%rsp)
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$8, %edi
	movq	%r12, 32(%rsp)
	movq	%rax, 8(%r12)
	call	allocate.3
	movq	%rax, %rbp
	movq	$.L209, 8(%rax)
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$8, %edi
	movq	%rax, 16(%rbp)
	movq	%rbp, 16(%r12)
	call	allocate.3
	movq	%rax, %rbp
	movq	$.L216, 8(%rax)
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$4, %esi
	movl	$12, %edi
	movq	%rax, 16(%rbp)
	movq	%rbp, 24(%r12)
	call	allocate.3
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$12, %edi
	movq	%rax, %r12
	call	allocate.3
	movq	%rax, %rbp
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$8, %edi
	movq	%rbp, 32(%rsp)
	movq	%rax, 8(%rbp)
	call	allocate.3
	movq	%rax, %rbx
	movq	$.L261, 8(%rax)
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$3, %esi
	movl	$12, %edi
	movq	%rax, 16(%rbx)
	movq	%rbx, 16(%rbp)
	call	allocate.3
	movq	%rax, %rbp
	movq	$25, 16(%rax)
	movq	$10, 24(%rax)
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$4, %esi
	movl	$4, %edi
	movq	16(%rax), %rbx
	call	allocate.3
	movq	%rax, %rcx
	movq	24(%rsp), %rax
	movq	$.L269, 24(%rcx)
	movq	%r12, 32(%rcx)
	movq	%rcx, 24(%rsp)
	movq	%rax, 8(%rcx)
	movq	32(%rsp), %rax
	movq	%rbp, 32(%rsp)
	movq	%rax, 16(%rcx)
	movq	16(%rbx), %rax
	movq	%rax, 8(%rbp)
.L261:
	leaq	40(%rsp), %r10
	xorl	%eax, %eax
	call	check_heap.6
	movq	32(%rsp), %rbx
	movq	16(%rbx), %r12
	cmpq	$1, %r12
	jne	.L262
	movq	24(%rbx), %r13
	.p2align 4,,7
.L276:
	movq	24(%rsp), %rax
	movq	24(%rax), %rax
.L273:
	movq	32(%rsp), %rbx
	jmp	*%rax
.L232:
	movq	24(%rsp), %rax
	movq	%r13, %rbp
	movq	16(%rax), %rdx
	movq	32(%rax), %r12
	movq	8(%rax), %rax
	movq	%rdx, 32(%rsp)
	movq	%rax, 24(%rsp)
.L233:
	leaq	40(%rsp), %r10
	movq	%rbp, 16(%r12)
	movl	$4, %esi
	movl	$12, %edi
	call	allocate.3
	movq	%rax, %rbp
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$24, %edi
	movq	8(%rax), %rax
	movq	16(%rax), %rbx
	movq	16(%rbx), %rbx
	movq	%rbx, 16(%rbp)
	movq	16(%rax), %rbx
	movq	8(%rbx), %rbx
	call	allocate.3
	movq	32(%rsp), %rdx
	movq	%rax, %r13
	leaq	40(%rsp), %r10
	movl	$4, %esi
	movl	$4, %edi
	movq	8(%rdx), %rax
	movq	%rbx, 8(%r13)
	movq	24(%rax), %rax
	movq	%r13, 24(%rbp)
	movq	%rax, 16(%r13)
	movq	8(%rdx), %rax
	movq	32(%rax), %rbx
	movq	8(%rax), %rax
	movq	%rbx, 32(%rbp)
	movq	16(%rax), %rbx
	call	allocate.3
	movq	%rax, %rcx
	movq	24(%rsp), %rax
	movq	$.L236, 24(%rcx)
	movq	%rax, 8(%rcx)
	movq	32(%rsp), %rax
	movq	%rax, 16(%rcx)
.L274:
	movq	%r12, 32(%rcx)
	movq	16(%rbx), %rax
	movq	%rcx, 24(%rsp)
	movq	%rbp, 32(%rsp)
	movq	%rax, 8(%rbp)
.L209:
	leaq	40(%rsp), %r10
	xorl	%eax, %eax
	call	check_heap.6
	movq	32(%rsp), %rbx
	cmpq	$10, 16(%rbx)
	jne	.L210
.L278:
	cmpq	$10, 24(%rbx)
	movl	$1, %r13d
	jne	.L276
	movl	$3, %r13d
	jmp	.L276
.L277:
	movq	16(%rbx), %r12
	movq	24(%rbx), %rbp
	sarq	%r12
	sarq	%rbp
	subq	%rbp, %r12
	leaq	(%r12,%r12), %rax
	orq	$1, %rax
	cmpq	%rax, 8(%rdx)
	cmovne	%rcx, %rsi
	cmpq	$6, %rsi
	jne	.L248
	movq	24(%rbx), %rax
	movq	16(%rdx), %rbp
	andq	$-2, %rax
	movq	%rbp, 32(%rbx)
	addq	$2, %rax
	orq	$1, %rax
	movq	%rax, 24(%rbx)
	.p2align 4,,7
.L216:
	movq	32(%rbx), %r12
	movl	$262, %r13d
	cmpq	$10, %r12
	movq	%r12, %rdx
	je	.L276
	movq	16(%rbx), %r12
	movq	24(%rbx), %rbp
	movl	$262, %esi
	sarq	%r12
	sarq	%rbp
	leaq	(%r12,%rbp), %rax
	movq	%rsi, %r12
	addq	%rax, %rax
	orq	$1, %rax
	cmpq	%rax, 8(%rdx)
	movl	$6, %eax
	cmovne	%rax, %r12
	cmpq	$6, %r12
	movq	%r12, %rcx
	je	.L277
.L248:
	movl	$6, %r13d
	jmp	.L276
.L222:
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$12, %edi
	call	allocate.3
	movq	%rax, %r12
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$3, %esi
	movl	$4, %edi
	movq	16(%rax), %rbp
	movq	8(%rax), %rax
	movq	16(%rbp), %rbp
	movq	%rbp, 16(%r12)
	movq	16(%rax), %rbp
	call	allocate.3
	movq	%rax, %rcx
	movq	24(%rsp), %rax
	movq	$.L226, 24(%rcx)
	movq	%rcx, 24(%rsp)
	movq	%rax, 8(%rcx)
	movq	32(%rsp), %rax
	movq	%r12, 32(%rsp)
	movq	%rax, 16(%rcx)
	movq	16(%rbp), %rax
	movq	%rax, 8(%r12)
	.p2align 4,,7
.L221:
	leaq	40(%rsp), %r10
	xorl	%eax, %eax
	call	check_heap.6
	movq	32(%rsp), %rax
	movq	16(%rax), %r12
	cmpq	$10, 16(%r12)
	jne	.L222
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$24, %edi
	movq	8(%r12), %r12
	call	allocate.3
	movq	%rax, %rbp
	movq	32(%rsp), %rax
	movq	%rbp, %r13
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	%r12, 8(%rbp)
	movq	24(%rax), %rbx
	movq	%rbx, 16(%rbp)
	jmp	.L276
.L270:
	movq	24(%rsp), %rax
	movq	%r13, %rdi
	xorl	%esi, %esi
	movq	16(%rax), %rdx
	movq	8(%rax), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rdx), %rax
	movq	%rax, 32(%rsp)
	call	dump_object
	movq	__stdoutp(%rip), %rsi
	movq	%rax, %r12
	movl	$10, %edi
	movq	%r12, %r13
	call	fputc
	jmp	.L276
.L207:
	addq	$40, %rsp
	movq	%r13, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.L215:
	movq	24(%rsp), %rax
	cmpq	$6, %r13
	movq	16(%rax), %rdx
	movq	32(%rax), %r12
	movq	8(%rax), %rax
	movq	%rdx, 32(%rsp)
	movq	%rax, 24(%rsp)
	je	.L217
	leaq	40(%rsp), %r10
	movl	$4, %esi
	movl	$12, %edi
	call	allocate.3
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$12, %edi
	movq	%rax, %rbp
	call	allocate.3
	movq	%rax, %rbx
	movq	32(%rsp), %rax
	movq	%rbx, 32(%rsp)
	movq	%rax, 8(%rbx)
	movq	8(%rax), %rax
	movq	16(%rax), %r13
	movq	16(%r13), %r13
	cmpq	$10, %r13
	movq	%r13, 16(%rbx)
	jne	.L218
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rbx
	jmp	.L219
.L269:
	movq	24(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$3, %esi
	movl	$4, %edi
	movq	32(%rax), %r12
	movq	16(%rax), %rdx
	movq	8(%rax), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rdx), %rax
	movq	%r13, 16(%r12)
	movq	$10, 24(%r12)
	movq	$10, 32(%r12)
	movq	%rax, 32(%rsp)
	movq	16(%rax), %rbp
	call	allocate.3
	movq	%rax, %rcx
	movq	24(%rsp), %rax
	leaq	40(%rsp), %r10
	movq	$.L270, 24(%rcx)
	movq	%rcx, 24(%rsp)
	movq	%rax, 8(%rcx)
	movq	32(%rsp), %rax
	movq	%r12, 32(%rsp)
	movq	%rax, 16(%rcx)
	movq	16(%rbp), %rax
	movq	%rax, 8(%r12)
	xorl	%eax, %eax
	call	check_heap.6
	movq	32(%rsp), %rbx
	cmpq	$10, 16(%rbx)
	je	.L278
	.p2align 4,,7
.L210:
	leaq	40(%rsp), %r10
	movl	$3, %esi
	movl	$12, %edi
	call	allocate.3
	movq	%rax, %r12
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$4, %esi
	movl	$12, %edi
	movq	%r12, 32(%rsp)
	movq	%rax, 8(%r12)
	call	allocate.3
	movq	%rax, %rbp
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$4, %esi
	movl	$4, %edi
	movq	8(%rax), %rax
	movq	16(%rax), %rbx
	movq	$3, 24(%rbp)
	movq	8(%rbx), %rbx
	movq	%rbx, 16(%rbp)
	movq	32(%rax), %rbx
	movq	8(%rax), %rax
	movq	%rbx, 32(%rbp)
	movq	24(%rax), %rbx
	call	allocate.3
	movq	%rax, %rcx
	movq	24(%rsp), %rax
	movq	$.L215, 24(%rcx)
	movq	%r12, 32(%rcx)
	movq	%rcx, 24(%rsp)
	movq	%rax, 8(%rcx)
	movq	32(%rsp), %rax
	movq	%rbp, 32(%rsp)
	movq	%rax, 16(%rcx)
	movq	16(%rbx), %rax
	movq	%rbp, %rbx
	movq	%rax, 8(%rbp)
	jmp	.L216
.L236:
	movq	24(%rsp), %rax
	movq	16(%rax), %rdx
	movq	32(%rax), %r12
	movq	8(%rax), %rcx
	movq	%r13, 24(%r12)
	movq	16(%rdx), %r12
	movq	24(%rdx), %rbp
	movq	%rdx, 32(%rsp)
	movq	%rcx, 24(%rsp)
	sarq	%r12
	sarq	%rbp
	leaq	(%r12,%rbp), %rax
	leaq	(%rax,%rax), %r13
	movq	24(%rcx), %rax
	orq	$1, %r13
	jmp	.L273
.L226:
	movq	24(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$24, %edi
	movq	16(%rax), %rdx
	movq	8(%rax), %rax
	movq	16(%rdx), %rbp
	movq	%rdx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	8(%rbp), %rbp
	call	allocate.3
	movq	%r13, 16(%rax)
	movq	%rax, %r13
	movq	%rbp, 8(%rax)
	jmp	.L276
.L229:
	movq	24(%rsp), %rax
	movq	%r13, %rbx
	movq	16(%rax), %rdx
	movq	32(%rax), %rbp
	movq	40(%rax), %r12
	movq	8(%rax), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rdx), %rax
	movq	%rax, 32(%rsp)
.L219:
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$24, %edi
	movq	8(%rax), %rax
	movq	%rbx, 16(%rbp)
	movq	$10, 24(%rbp)
	movq	%rax, 32(%rsp)
	movq	8(%rax), %rax
	movq	16(%rax), %rbx
	movq	8(%rbx), %rbx
	call	allocate.3
	movq	32(%rsp), %rdx
	movq	%rax, %r13
	leaq	40(%rsp), %r10
	movl	$4, %esi
	movl	$4, %edi
	movq	8(%rdx), %rax
	movq	%rbx, 8(%r13)
	movq	32(%rax), %rax
	movq	%r13, 32(%rbp)
	movq	%rax, 16(%r13)
	movq	8(%rdx), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rbx
	call	allocate.3
	movq	%rax, %rcx
	movq	24(%rsp), %rax
	movq	$.L232, 24(%rcx)
	movq	%rax, 8(%rcx)
	movq	32(%rsp), %rax
	movq	%rax, 16(%rcx)
	jmp	.L274
	.p2align 4,,7
.L262:
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$24, %edi
	sarq	%r12
	call	allocate.3
	movq	%rax, %rbp
	movq	32(%rsp), %rax
	leaq	-2(%r12,%r12), %r12
	orq	$1, %r12
	movq	16(%rax), %rbx
	movq	24(%rax), %r13
	movq	%r12, 16(%rax)
	movq	%rbp, 24(%rax)
	movq	%rbx, 8(%rbp)
	movq	%r13, 16(%rbp)
	jmp	.L261
.L217:
	movl	$1, %ebp
	jmp	.L233
.L218:
.L220:
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$12, %edi
	call	allocate.3
	movq	%rax, %rbx
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$8, %edi
	movq	%rbx, 32(%rsp)
	movq	%rax, 8(%rbx)
	call	allocate.3
	movq	%rax, %r13
	movq	$.L221, 8(%rax)
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$12, %edi
	movq	%rax, 16(%r13)
	movq	%r13, 16(%rbx)
	call	allocate.3
	movq	32(%rsp), %rdx
	movq	%rax, %rbx
	leaq	40(%rsp), %r10
	movl	$5, %esi
	movl	$4, %edi
	movq	8(%rdx), %rax
	movq	16(%rax), %r13
	movq	%r13, 16(%rbx)
	movq	16(%rdx), %r13
	call	allocate.3
	movq	%rax, %rcx
	movq	24(%rsp), %rax
	movq	$.L229, 24(%rcx)
	movq	%rbp, 32(%rcx)
	movq	%r12, 40(%rcx)
	movq	%rcx, 24(%rsp)
	movq	%rax, 8(%rcx)
	movq	32(%rsp), %rax
	movq	%rbx, 32(%rsp)
	movq	%rax, 16(%rcx)
	movq	16(%r13), %rax
	movq	%rax, 8(%rbx)
	jmp	.L221
.LFE19:
	.size	vm, .-vm
	.section	.rodata.str1.8
	.p2align 3
.LC21:
	.string	"{total ticks: %lld gc ticks: %lld}\n"
	.section	.rodata.str1.1
.LC20:
	.string	"unable to allocate heap\n"
	.text
	.p2align 4,,15
.globl main
	.type	main, @function
main:
.LFB18:
	movq	%rbx, -24(%rsp)
.LCFI29:
	movq	%rbp, -16(%rsp)
.LCFI30:
	movl	$800000, %edi
	movq	%r12, -8(%rsp)
.LCFI31:
	subq	$24, %rsp
.LCFI32:
	call	malloc
	movl	$800000, %edi
	movq	%rax, heap0(%rip)
	call	malloc
	cmpq	$0, heap0(%rip)
	movq	%rax, heap1(%rip)
	je	.L281
	testq	%rax, %rax
	je	.L281
#APP
	rdtsc
#NO_APP
	salq	$32, %rdx
	mov	%eax, %ebp
	orq	%rdx, %rbp
	call	vm
	movq	%rax, %r12
#APP
	rdtsc
#NO_APP
	xorl	%esi, %esi
	salq	$32, %rdx
	movq	%r12, %rdi
	mov	%eax, %ebx
	orq	%rdx, %rbx
	call	dump_object
	movq	__stdoutp(%rip), %rsi
	movl	$10, %edi
	subq	%rbp, %rbx
	call	fputc
	movq	gc_ticks(%rip), %rcx
	movq	__stderrp(%rip), %rdi
	movq	%rbx, %rdx
	movl	$.LC21, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	%r12d, %eax
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
	.p2align 4,,7
.L281:
	movq	__stderrp(%rip), %rcx
	movl	$24, %edx
	movl	$1, %esi
	movl	$.LC20, %edi
	call	fwrite
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movl	$-1, %eax
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
.LFE18:
	.size	main, .-main
	.section	.eh_frame,"a",@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1
.LSCIE1:
	.long	0x0
	.byte	0x1
	.string	""
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.p2align 3
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1
.LASFDE1:
	.long	.LASFDE1-.Lframe1
	.quad	.LFB3
	.quad	.LFE3-.LFB3
	.p2align 3
.LEFDE1:
.LSFDE3:
	.long	.LEFDE3-.LASFDE3
.LASFDE3:
	.long	.LASFDE3-.Lframe1
	.quad	.LFB4
	.quad	.LFE4-.LFB4
	.p2align 3
.LEFDE3:
.LSFDE5:
	.long	.LEFDE5-.LASFDE5
.LASFDE5:
	.long	.LASFDE5-.Lframe1
	.quad	.LFB5
	.quad	.LFE5-.LFB5
	.p2align 3
.LEFDE5:
.LSFDE7:
	.long	.LEFDE7-.LASFDE7
.LASFDE7:
	.long	.LASFDE7-.Lframe1
	.quad	.LFB6
	.quad	.LFE6-.LFB6
	.p2align 3
.LEFDE7:
.LSFDE9:
	.long	.LEFDE9-.LASFDE9
.LASFDE9:
	.long	.LASFDE9-.Lframe1
	.quad	.LFB7
	.quad	.LFE7-.LFB7
	.p2align 3
.LEFDE9:
.LSFDE11:
	.long	.LEFDE11-.LASFDE11
.LASFDE11:
	.long	.LASFDE11-.Lframe1
	.quad	.LFB10
	.quad	.LFE10-.LFB10
	.p2align 3
.LEFDE11:
.LSFDE13:
	.long	.LEFDE13-.LASFDE13
.LASFDE13:
	.long	.LASFDE13-.Lframe1
	.quad	.LFB14
	.quad	.LFE14-.LFB14
	.byte	0x4
	.long	.LCFI0-.LFB14
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.p2align 3
.LEFDE13:
.LSFDE15:
	.long	.LEFDE15-.LASFDE15
.LASFDE15:
	.long	.LASFDE15-.Lframe1
	.quad	.LFB15
	.quad	.LFE15-.LFB15
	.byte	0x4
	.long	.LCFI1-.LFB15
	.byte	0xe
	.uleb128 0x10
	.byte	0x8d
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI2-.LCFI1
	.byte	0xe
	.uleb128 0x18
	.byte	0x8c
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI3-.LCFI2
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI4-.LCFI3
	.byte	0xe
	.uleb128 0x28
	.byte	0x83
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI5-.LCFI4
	.byte	0xe
	.uleb128 0x30
	.p2align 3
.LEFDE15:
.LSFDE17:
	.long	.LEFDE17-.LASFDE17
.LASFDE17:
	.long	.LASFDE17-.Lframe1
	.quad	.LFB12
	.quad	.LFE12-.LFB12
	.byte	0x4
	.long	.LCFI6-.LFB12
	.byte	0x8c
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI7-.LCFI6
	.byte	0x83
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI10-.LCFI7
	.byte	0x8e
	.uleb128 0x2
	.byte	0x8d
	.uleb128 0x3
	.byte	0x86
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI11-.LCFI10
	.byte	0xe
	.uleb128 0x30
	.p2align 3
.LEFDE17:
.LSFDE19:
	.long	.LEFDE19-.LASFDE19
.LASFDE19:
	.long	.LASFDE19-.Lframe1
	.quad	.LFB16
	.quad	.LFE16-.LFB16
	.byte	0x4
	.long	.LCFI12-.LFB16
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.p2align 3
.LEFDE19:
.LSFDE21:
	.long	.LEFDE21-.LASFDE21
.LASFDE21:
	.long	.LASFDE21-.Lframe1
	.quad	.LFB13
	.quad	.LFE13-.LFB13
	.byte	0x4
	.long	.LCFI13-.LFB13
	.byte	0xe
	.uleb128 0x10
	.p2align 3
.LEFDE21:
.LSFDE23:
	.long	.LEFDE23-.LASFDE23
.LASFDE23:
	.long	.LASFDE23-.Lframe1
	.quad	.LFB29
	.quad	.LFE29-.LFB29
	.byte	0x4
	.long	.LCFI14-.LFB29
	.byte	0xe
	.uleb128 0x10
	.p2align 3
.LEFDE23:
.LSFDE25:
	.long	.LEFDE25-.LASFDE25
.LASFDE25:
	.long	.LASFDE25-.Lframe1
	.quad	.LFB27
	.quad	.LFE27-.LFB27
	.byte	0x4
	.long	.LCFI15-.LFB27
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI16-.LCFI15
	.byte	0xe
	.uleb128 0x18
	.byte	0x8e
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI17-.LCFI16
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI18-.LCFI17
	.byte	0xe
	.uleb128 0x28
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI19-.LCFI18
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI20-.LCFI19
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI21-.LCFI20
	.byte	0xe
	.uleb128 0x40
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.p2align 3
.LEFDE25:
.LSFDE27:
	.long	.LEFDE27-.LASFDE27
.LASFDE27:
	.long	.LASFDE27-.Lframe1
	.quad	.LFB30
	.quad	.LFE30-.LFB30
	.p2align 3
.LEFDE27:
.LSFDE29:
	.long	.LEFDE29-.LASFDE29
.LASFDE29:
	.long	.LASFDE29-.Lframe1
	.quad	.LFB31
	.quad	.LFE31-.LFB31
	.byte	0x4
	.long	.LCFI22-.LFB31
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI23-.LCFI22
	.byte	0xe
	.uleb128 0x20
	.byte	0x83
	.uleb128 0x2
	.p2align 3
.LEFDE29:
.LSFDE31:
	.long	.LEFDE31-.LASFDE31
.LASFDE31:
	.long	.LASFDE31-.Lframe1
	.quad	.LFB19
	.quad	.LFE19-.LFB19
	.byte	0x4
	.long	.LCFI24-.LFB19
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI25-.LCFI24
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI26-.LCFI25
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI27-.LCFI26
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI28-.LCFI27
	.byte	0xe
	.uleb128 0x50
	.byte	0x83
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x2
	.p2align 3
.LEFDE31:
.LSFDE33:
	.long	.LEFDE33-.LASFDE33
.LASFDE33:
	.long	.LASFDE33-.Lframe1
	.quad	.LFB18
	.quad	.LFE18-.LFB18
	.byte	0x4
	.long	.LCFI32-.LFB18
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x2
	.byte	0x86
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.p2align 3
.LEFDE33:
	.ident	"GCC: (GNU) 3.4.6 [FreeBSD] 20060305"
