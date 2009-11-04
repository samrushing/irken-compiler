(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

;; test the raw speed of an integer loop

(let loop ((n 1000000))
  (if (= n 0)
      "done"
      (loop (- n 1))))

;; irken compiles <loop> to this:
;;
;;  FUN_146loop_7:
;;    r0 = varref (0,0);
;;    r1 = (object *) 1;
;;    if (unbox(r0)==unbox(r1)) {
;;      r0 = top[2];
;;      PXLL_RETURN(0);
;;    } else {
;;      r0 = varref (0,0);
;;      r1 = (object *) 3;
;;      r0 = box(unbox(r0)-unbox(r1));
;;      lenv[2] = r0;
;;      goto FUN_146loop_7;
;;    }
;;    PXLL_RETURN(0);

;; gcc -O3 compiles *that* to:
;; L130:
;; 	movq	(%r12), %rax
;; 	sarq	%rax
;; 	jne	L131
;; 	movq	(%r13), %rbx
;; 	jmp	*(%r14)
;; 	.align 4,0x90
;; L131:
;; 	leaq	-2(%rax,%rax), %rax
;; 	orq	$1, %rax
;; 	movq	%rax, (%r12)
;; 	jmp	L130

;; gcc-llvm does even better:
;;LBB6_1:
;;	cmpq	$1, %rdx
;;	ja	LBB6_4
;;	cmpq	$2, 24(%rcx)
;;	je	LBB6_1
;;Llabel47:
;;Llabel24:
;;	popq	%rbp
;;	ret
;;LBB6_4:
;;	addq	$-2, %rdx
;;	orq	$1, %rdx
;;	movq	%rdx, 120(%rcx)
;;	jmp	LBB6_1

;; dunno why, but gcc gives about 8 cycles per loop,
;   and the llvm gives around *2* cycles per loop.
