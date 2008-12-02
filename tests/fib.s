	.file	"fib.c"
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
	.p2align 4,,15
	.type	allocate.3, @function
allocate.3:
.LFB27:
	movq	%r10, -8(%rsp)
	movq	-24(%r10), %rcx
	movq	%rsi, %rax
	salq	$8, %rax
	andl	$255, %edi
	movq	%r10, %r8
	orq	%rdi, %rax
	movq	%rcx, %rdx
	movq	%rax, (%rcx)
	jmp	.L120
	.p2align 4,,7
.L121:
	movq	%rdx, %rax
	addq	$8, %rdx
	movq	$10, 8(%rax)
.L120:
	decq	%rsi
	cmpq	$-1, %rsi
	jne	.L121
	leaq	8(%rdx), %rax
	movq	%rax, -24(%r8)
	movq	%rcx, %rax
	ret
.LFE27:
	.size	allocate.3, .-allocate.3
	.p2align 4,,15
.globl vm
	.type	vm, @function
vm:
.LFB19:
.L123:
	pushq	%r12
.LCFI14:
	movl	$3, %esi
	movl	$4, %edi
	pushq	%rbp
.LCFI15:
	pushq	%rbx
.LCFI16:
	subq	$40, %rsp
.LCFI17:
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
	movl	$2, %esi
	movl	$12, %edi
	movq	$10, 8(%rax)
	movq	$10, 16(%rax)
	movq	$.L122, 24(%rax)
	movq	%rax, 24(%rsp)
	call	allocate.3
	movq	%rax, %rbx
	movq	%rax, 8(%rsp)
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$2, %esi
	movl	$8, %edi
	movq	%rbx, 32(%rsp)
	movq	%rax, 8(%rbx)
	call	allocate.3
	movq	%rax, %rsi
	movq	$.L124, 8(%rax)
	movq	32(%rsp), %rax
	leaq	40(%rsp), %r10
	movl	$12, %edi
	movq	%rax, 16(%rsi)
	movq	%rsi, 16(%rbx)
	movl	$4, %esi
	call	allocate.3
	movq	%rax, %rbx
	movq	$101, 16(%rax)
	movq	$1, 24(%rax)
	movq	$3, 32(%rax)
	movq	8(%rsp), %rax
	movq	%rbx, %rbp
	movq	24(%rsp), %r12
	movq	%rbx, 32(%rsp)
	movq	16(%rax), %rsi
	movq	16(%rsi), %rax
	movq	%rax, 8(%rbx)
.L124:
	movq	16(%rbp), %rbx
	cmpq	$1, %rbx
	je	.L135
.L125:
	movq	32(%rbp), %rcx
	sarq	%rbx
	movq	24(%rbp), %rsi
	leaq	-2(%rbx,%rbx), %rax
	movq	%rcx, %rdx
	orq	$1, %rax
	sarq	%rsi
	sarq	%rdx
	movq	%rax, 16(%rbp)
	movq	16(%rbp), %rbx
	leaq	(%rsi,%rdx), %rdx
	movq	%rcx, 24(%rbp)
	addq	%rdx, %rdx
	orq	$1, %rdx
	cmpq	$1, %rbx
	movq	%rdx, 32(%rbp)
	jne	.L125
.L135:
	movq	24(%rbp), %rbx
	jmp	*24(%r12)
	.p2align 4,,7
.L122:
	addq	$40, %rsp
	movq	%rbx, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
.LFE19:
	.size	vm, .-vm
	.section	.rodata.str1.8,"aMS",@progbits,1
	.p2align 3
.LC17:
	.string	"{total ticks: %lld gc ticks: %lld}\n"
	.section	.rodata.str1.1
.LC16:
	.string	"unable to allocate heap\n"
	.text
	.p2align 4,,15
.globl main
	.type	main, @function
main:
.LFB18:
	movq	%rbx, -24(%rsp)
.LCFI18:
	movq	%rbp, -16(%rsp)
.LCFI19:
	movl	$800000, %edi
	movq	%r12, -8(%rsp)
.LCFI20:
	subq	$24, %rsp
.LCFI21:
	call	malloc
	movl	$800000, %edi
	movq	%rax, heap0(%rip)
	call	malloc
	cmpq	$0, heap0(%rip)
	movq	%rax, heap1(%rip)
	je	.L138
	testq	%rax, %rax
	je	.L138
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
	movl	$.LC17, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	%r12d, %eax
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
	.p2align 4,,7
.L138:
	movq	__stderrp(%rip), %rcx
	movl	$24, %edx
	movl	$1, %esi
	movl	$.LC16, %edi
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
	.quad	.LFB27
	.quad	.LFE27-.LFB27
	.p2align 3
.LEFDE23:
.LSFDE25:
	.long	.LEFDE25-.LASFDE25
.LASFDE25:
	.long	.LASFDE25-.Lframe1
	.quad	.LFB19
	.quad	.LFE19-.LFB19
	.byte	0x4
	.long	.LCFI14-.LFB19
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI15-.LCFI14
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI16-.LCFI15
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI17-.LCFI16
	.byte	0xe
	.uleb128 0x48
	.byte	0x83
	.uleb128 0x4
	.byte	0x86
	.uleb128 0x3
	.byte	0x8c
	.uleb128 0x2
	.p2align 3
.LEFDE25:
.LSFDE27:
	.long	.LEFDE27-.LASFDE27
.LASFDE27:
	.long	.LASFDE27-.Lframe1
	.quad	.LFB18
	.quad	.LFE18-.LFB18
	.byte	0x4
	.long	.LCFI21-.LFB18
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x2
	.byte	0x86
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.p2align 3
.LEFDE27:
	.ident	"GCC: (GNU) 3.4.6 [FreeBSD] 20060305"
