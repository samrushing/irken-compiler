;; -*- Mode: llvm -*-

;; --- preamble ---

;; these functions are meant to (roughly) implement the
;;  cps insns, along with some helpers.  The expectation
;;  is that these will all be inlined, resulting in relatively
;;  compact output for compiled functions.

;; XXX consider using llvm.read_register/write_register to keep
;;  lenv or k in %ebp. [especially since llvm doesn't seem to use
;;  it for anything when you -fomit-frame-pointer.

@k	= external global i8**
@lenv	= external global i8**
@top	= external global i8**
@freep	= external global i8**

%struct._string = type { i64, i32, [0 x i8] }

;; intrinsics
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* %dst, i8* %src, i64 %len, i32 %align, i1 %isvolatile)
declare i64 @llvm.ctpop.i64 (i64 %n)
declare i64 @llvm.readcyclecounter ()

;; with LTO, many/most of these functions could be written in C.

;; these functions are written in C, mostly in include/header1.c
declare void @check_heap()
declare i8** @make_vector (i64 %size, i8** %val)
declare void @vector_range_check (i8** %vec, i64 %index)
declare i8** @record_fetch (i8** %rec, i64 %label)
declare void @record_store (i8** %rec, i64 %label, i8** %val)
declare void @exit_continuation(i8**)
declare void @DO (i8** %ob)
declare void @DENV()
declare void @TRACE(i8* %name)
declare i8** @irk_get_errno()
declare i64  @magic_cmp (i8**, i8**)
declare i8** @irk_write_stdout (i8**, i8**)
declare i8** @irk_dump_object (i8**)
declare i8** @irk_putc (i8**)
declare i8** @irk_flush()
declare i8** @irk_make_argv()
declare i8** @irk_set_verbose_gc(i8**)
declare i8** @irk_string_cmp (i8** %a, i8** %b)
declare i8** @irk_make_string (i8** %len)
declare void @relocate_llvm_literals (i8**, i32*)
declare i8** @irk_copy_tuple (i8**)
declare i8** @irk_mul2 (i64, i64, i8**)
declare i8** @irk_div2b1 (i64, i64, i64, i8**)
declare i8** @irk_object2int (i8**)
declare i8** @irk_objectptr2int (i8**)

;; FFI
declare i8** @make_malloc (i64 %size, i64 %count)
declare i8** @make_halloc (i64 %size, i64 %count)
declare i8** @make_foreign (i8* %p)
declare i8*  @get_foreign (i8** %ob)
declare i8** @offset_foreign (i8** %foreign, i64 %offset)
declare i8** @free_foreign (i8** %foreign)
declare i8** @irk_cref_2_string (i8** %src, i8** %len)
declare i8** @irk_string_2_cref (i8** %src)
declare i8** @irk_cref_2_int (i8** %src)
declare i8** @irk_int_2_cref (i8** %src)

;; the following functions are written in llvm, often based on (or
;; compiled from) a prototype written in C.  Having them here, and
;; declared 'internal fastcc', allows llvm to inline them.

define internal fastcc i8** @insn_varref(i64 %depth, i64 %index) {
  %1 = load i8**, i8*** @lenv
  %2 = icmp eq i64 %depth, 0
  br i1 %2, label %._crit_edge, label %.lr.ph

.lr.ph:
  %lenv0.02 = phi i8** [ %6, %.lr.ph ], [ %1, %0 ]
  %.01 = phi i64 [ %3, %.lr.ph ], [ %depth, %0 ]
  %3 = add nsw i64 %.01, -1
  %4 = getelementptr inbounds i8*, i8** %lenv0.02, i64 1
  %5 = load i8*, i8** %4
  %6 = bitcast i8* %5 to i8**
  %7 = icmp eq i64 %3, 0
  br i1 %7, label %._crit_edge, label %.lr.ph

._crit_edge:
  %lenv0.0.lcssa = phi i8** [ %1, %0 ], [ %6, %.lr.ph ]
  %8 = add nsw i64 %index, 2
  %9 = getelementptr inbounds i8*, i8** %lenv0.0.lcssa, i64 %8
  %10 = load i8*, i8** %9
  %11 = bitcast i8* %10 to i8**
  ret i8** %11
}

; Function Attrs: nounwind ssp uwtable
define internal fastcc void @insn_varset(i64 %depth, i64 %index, i8** %val) {
  %1 = load i8**, i8*** @lenv
  %2 = icmp eq i64 %depth, 0
  br i1 %2, label %._crit_edge, label %.lr.ph

.lr.ph:
  %lenv0.02 = phi i8** [ %6, %.lr.ph ], [ %1, %0 ]
  %.01 = phi i64 [ %3, %.lr.ph ], [ %depth, %0 ]
  %3 = add nsw i64 %.01, -1
  %4 = getelementptr inbounds i8*, i8** %lenv0.02, i64 1
  %5 = load i8*, i8** %4
  %6 = bitcast i8* %5 to i8**
  %7 = icmp eq i64 %3, 0
  br i1 %7, label %._crit_edge, label %.lr.ph

._crit_edge:
  %lenv0.0.lcssa = phi i8** [ %1, %0 ], [ %6, %.lr.ph ]
  %8 = bitcast i8** %val to i8*
  %9 = add nsw i64 %index, 2
  %10 = getelementptr inbounds i8*, i8** %lenv0.0.lcssa, i64 %9
  store i8* %8, i8** %10
  ret void
}

;; consider using llvm.memset to clear allocated space
;;  [and thus remove the need to clear tospace after gc]

define internal fastcc i8** @allocate(i64 %tc, i64 %size) {
  %1 = load i8**, i8*** @freep
  %2 = shl i64 %size, 8
  %3 = and i64 %tc, 255
  %4 = or i64 %2, %3
  %5 = inttoptr i64 %4 to i8*
  store i8* %5, i8** %1
  %6 = add nsw i64 %size, 1
  %7 = load i8**, i8*** @freep
  %8 = getelementptr inbounds i8*, i8** %7, i64 %6
  store i8** %8, i8*** @freep
  ret i8** %1
}

define internal fastcc i64 @get_case(i8** %ob) {
  %1 = ptrtoint i8** %ob to i64
  %2 = and i64 %1, 3
  %3 = icmp eq i64 %2, 0
  br i1 %3, label %7, label %4

; <label>:4
  %5 = and i64 %1, 1
  %6 = icmp eq i64 %5, 0
  %. = select i1 %6, i64 %1, i64 0
  ret i64 %.

; <label>:7
  %8 = bitcast i8** %ob to i64*
  %9 = load i64, i64* %8
  %10 = and i64 %9, 255
  ret i64 %10
}

define internal fastcc void @link_env_with_args (i8** %env, i8** %fun) {
  %1 = getelementptr i8*, i8** %fun, i64 2
  %2 = load i8*, i8** %1
  %3 = getelementptr i8*, i8** %env, i64 1
  store i8* %2, i8** %3
  store i8** %env, i8*** @lenv
  ret void
}

define internal fastcc void @link_env_noargs (i8** readonly %fun) {
  %1 = getelementptr inbounds i8*, i8** %fun, i64 2
  %2 = load i8*, i8** %1
  %3 = bitcast i8* %2 to i8**
  store i8** %3, i8*** @lenv
  ret void
}

define internal fastcc i8** @fetch (i8*** %src, i64 %off) {
  %1 = load i8**, i8*** %src
  %2 = getelementptr i8*, i8** %1, i64 %off
  %3 = load i8*, i8** %2
  %4 = bitcast i8* %3 to i8**
  ret i8** %4
}

define internal fastcc void @push_env (i8** %env)
{
  %1 = load i8**, i8*** @lenv
  %2 = bitcast i8** %1 to i8*
  %3 = getelementptr i8*, i8** %env, i64 1
  store i8* %2, i8** %3
  store i8** %env, i8*** @lenv
  ret void
}

define internal fastcc void @pop_env() {
  %1 = load i8**, i8*** @lenv
  %2 = getelementptr inbounds i8*, i8** %1, i64 1
  %3 = load i8*, i8** %2
  %4 = bitcast i8* %3 to i8**
  store i8** %4, i8*** @lenv
  ret void
}

; void
; push_k (object * t, object * fp)
; {
;   t[1] = k;
;   t[2] = lenv;
;   t[3] = fp;
;   k = t;
; }

define internal fastcc void @push_k(i8** %t, i8** %fp) {
  %1 = load i8**, i8*** @k
  %2 = bitcast i8** %1 to i8*
  %3 = getelementptr i8*, i8** %t, i64 1
  store i8* %2, i8** %3
  %4 = load i8**, i8*** @lenv
  %5 = bitcast i8** %4 to i8*
  %6 = getelementptr i8*, i8** %t, i64 2
  store i8* %5, i8** %6
  %7 = bitcast i8** %fp to i8*
  %8 = getelementptr i8*, i8** %t, i64 3
  store i8* %7, i8** %8
  store i8** %t, i8*** @k
  ret void
}

; void
; pop_k (void)
; {
;   lenv = k[2];
;   k = k[1];
; }

define internal fastcc void @pop_k() {
  %1 = load i8**, i8*** @k
  %2 = getelementptr i8*, i8** %1, i64 2
  %3 = load i8*, i8** %2
  %4 = bitcast i8* %3 to i8**
  store i8** %4, i8*** @lenv
  %5 = getelementptr i8*, i8** %1, i64 1
  %6 = load i8*, i8** %5
  %7 = bitcast i8* %6 to i8**
  store i8** %7, i8*** @k
  ret void
}

define internal fastcc i8** @insn_fetch (i8** %ob, i64 %i)
{
  %1 = getelementptr i8*, i8** %ob, i64 %i
  %2 = load i8*, i8** %1
  %3 = bitcast i8* %2 to i8**
  ret i8** %3
}

define internal fastcc i8** @insn_topref (i64 %index) {
  %1 = load i8**, i8*** @top
  %2 = add i64 %index, 2 ;; skip lenv:header,next
  %3 = getelementptr i8*, i8** %1, i64 %2
  %4 = load i8*, i8** %3
  %5 = bitcast i8* %4 to i8**
  ret i8** %5
}

define internal fastcc void @insn_topset(i64, i8**) {
  %3 = load i8**, i8*** @top
  %4 = add nsw i64 %0, 2
  %5 = getelementptr inbounds i8*, i8** %3, i64 %4
  %6 = bitcast i8** %5 to i8***
  store i8** %1, i8*** %6
  ret void
}

define internal fastcc i64 @insn_unbox (i8** %val) {
  %1 = ptrtoint i8** %val to i64
  %2 = ashr i64 %1, 1
  ret i64 %2
}

define internal fastcc i8** @insn_box (i64 %val) {
  %1 = shl i64 %val, 1
  %2 = or i64 %1, 1
  %3 = inttoptr i64 %2 to i8**
  ret i8** %3
}

define internal fastcc i8** @insn_add (i8** %a, i8** %b) {
  %1 = call fastcc i64 @insn_unbox (i8** %a)
  %2 = call fastcc i64 @insn_unbox (i8** %b)
  %3 = add i64 %1, %2
  %4 = call fastcc i8** @insn_box (i64 %3)
  ret i8** %4
}

define internal fastcc void @insn_return (i8** %val) {
  %1 = load i8**, i8*** @k
  %2 = getelementptr i8*, i8** %1, i64 3
  %3 = bitcast i8** %2 to void (i8**)**
  %4 = load void (i8**)*, void (i8**)** %3
  tail call fastcc void %4(i8** %val)
  ret void
}

define internal fastcc void @tail_call (i8** %fun) {
  %1 = getelementptr i8*, i8** %fun, i64 1
  %2 = bitcast i8** %1 to void ()**
  %3 = load void ()*, void ()** %2
  tail call fastcc void %3()
  ret void
}

define internal fastcc void @insn_store (i8** %dst, i64 %off, i8** %src) {
  %1 = getelementptr i8*, i8** %dst, i64 %off
  %2 = bitcast i8** %src to i8*
  store i8* %2, i8** %1
  ret void
}

define internal fastcc i8** @insn_close (void()* %fun) {
  %1 = call fastcc i8** @allocate (i64 8, i64 2)
  %2 = getelementptr i8*, i8** %1, i64 1
  %3 = bitcast void()* %fun to i8*
  store i8* %3, i8** %2
  %4 = getelementptr i8*, i8** %1, i64 2
  %5 = load i8**, i8*** @lenv
  %6 = bitcast i8** %5 to i8*
  store i8* %6, i8** %4
  ret i8** %1
}

define internal fastcc i8** @irk_makei (i8** %tag, i8** %val) {
  %tag0 = call fastcc i64 @insn_unbox (i8** %tag)
  %val0 = call fastcc i64 @insn_unbox (i8** %val)
  %1 = shl i64 %val0, 8
  %2 = and i64 %tag0, 255
  %3 = or i64 %1, %2
  %4 = inttoptr i64 %3 to i8**
  ret i8** %4
}

define internal fastcc i8** @irk_get_char (i8** %char) {
  %1 = ptrtoint i8** %char to i64
  %2 = ashr i64 %1, 8
  %3 = call fastcc i8** @insn_box (i64 %2)
  ret i8** %3
}

; object *
; irk_string_ref (object * src, object * idx)
; {
;   pxll_int index = UNBOX_INTEGER (idx);
;   pxll_string * src0 = (pxll_string *) src;
;   return TO_CHAR ((uint8_t)(src0->data[index]));
; }

define internal fastcc i8** @irk_string_ref(i8**, i8**) {
  %3 = ptrtoint i8** %1 to i64
  %4 = ashr i64 %3, 1
  %5 = bitcast i8** %0 to %struct._string*
  %6 = getelementptr inbounds %struct._string, %struct._string* %5, i64 0, i32 2, i64 %4
  %7 = load i8, i8* %6
  %8 = zext i8 %7 to i64
  %9 = shl nuw nsw i64 %8, 8
  %10 = or i64 %9, 2
  %11 = inttoptr i64 %10 to i8**
  ret i8** %11
}

; object *
; irk_string_set (object * src, object * idx, object *val)
; {
;   pxll_int index = UNBOX_INTEGER (idx);
;   pxll_string * src0 = (pxll_string *) src;
;   src0->data[index] = GET_CHAR(val);
;   return (object *) PXLL_UNDEFINED;
; }

define internal fastcc i8** @irk_string_set(i8**, i8**, i8**) {
  %4 = ptrtoint i8** %1 to i64
  %5 = ashr i64 %4, 1
  %6 = bitcast i8** %0 to %struct._string*
  %7 = ptrtoint i8** %2 to i64
  %8 = lshr i64 %7, 8
  %9 = trunc i64 %8 to i8
  %10 = getelementptr inbounds %struct._string, %struct._string* %6, i64 0, i32 2, i64 %5
  store i8 %9, i8* %10
  ret i8** inttoptr (i64 14 to i8**)
}

; object *
; irk_magic_cmp (object * a, object * b)
; {
;   return (object *) UITAG (1 + magic_cmp (a, b));
; }

define internal fastcc i8** @irk_magic_cmp(i8**, i8**) {
  %3 = tail call i64 @magic_cmp(i8** %0, i8** %1)
  %4 = shl i64 %3, 8
  %5 = add i64 %4, 278
  %6 = inttoptr i64 %5 to i8**
  ret i8** %6
}

; object *
; irk_int_cmp (object *a, object * b)
; {
;   pxll_int a0 = UNBOX_INTEGER (a);
;   pxll_int b0 = UNBOX_INTEGER (b);
;   pxll_int r = (a0 < b0) ? 0 : ((b0 < a0) ? 2 : 1);
;   return (object *) UITAG(r);
; }

define internal fastcc i8** @irk_int_cmp(i8**, i8**) {
  %3 = ptrtoint i8** %0 to i64
  %4 = ashr i64 %3, 1
  %5 = ptrtoint i8** %1 to i64
  %6 = ashr i64 %5, 1
  %7 = icmp slt i64 %4, %6
  %8 = icmp slt i64 %6, %4
  %9 = select i1 %8, i8** inttoptr (i64 534 to i8**), i8** inttoptr (i64 278 to i8**)
  %10 = select i1 %7, i8** inttoptr (i64 22 to i8**), i8** %9
  ret i8** %10
}

define internal fastcc i8** @insn_callocate(i64, i64) {
  %3 = mul nsw i64 %1, %0
  %4 = add i64 %3, 7
  %5 = lshr i64 %4, 3
  %6 = load i8**, i8*** @freep
  %7 = shl i64 %5, 8
  %8 = or i64 %7, 32
  %9 = inttoptr i64 %8 to i8*
  store i8* %9, i8** %6
  %10 = add nuw nsw i64 %5, 1
  %11 = load i8**, i8*** @freep
  %12 = getelementptr inbounds i8*, i8** %11, i64 %10
  store i8** %12, i8*** @freep
  ret i8** %6
}

define internal fastcc i8** @irk_string_len(i8**) {
  %2 = getelementptr inbounds i8*, i8** %0, i64 1
  %3 = bitcast i8** %2 to i32*
  %4 = load i32, i32* %3
  %5 = zext i32 %4 to i64
  %6 = shl nuw nsw i64 %5, 1
  %7 = or i64 %6, 1
  %8 = inttoptr i64 %7 to i8**
  ret i8** %8
}

define internal fastcc i8** @irk_tuple_len(i8**) {
  %2 = bitcast i8** %0 to i64*
  %3 = load i64, i64* %2
  %4 = ashr i64 %3, 7
  %5 = or i64 %4, 1
  %6 = inttoptr i64 %5 to i8**
  ret i8** %6
}

; object *
; irk_buffer_copy (object * src, object * sstart, object * n, object * dst, object * dstart)
; {
;   void * src0 = GET_STRING_POINTER (src);
;   pxll_int sstart0 = unbox (sstart);
;   pxll_int n0 = unbox (n);
;   void * dst0 = GET_STRING_POINTER (dst);
;   pxll_int dstart0 = unbox (dstart);
;   memcpy (dst0 + dstart0, src0 + sstart0, n0);
;   return (object *) PXLL_UNDEFINED;
; }

;; note: @llvm.objectsize + @__memcpy_chk rewritten to use @llvm.memcpy
define internal fastcc i8** @irk_buffer_copy(i8**, i8**, i8**, i8**, i8**) {
  %6 = bitcast i8** %0 to %struct._string*
  %7 = ptrtoint i8** %1 to i64
  %8 = ashr i64 %7, 1
  %9 = ptrtoint i8** %2 to i64
  %10 = ashr i64 %9, 1
  %11 = bitcast i8** %3 to %struct._string*
  %12 = ptrtoint i8** %4 to i64
  %13 = ashr i64 %12, 1
  %14 = getelementptr %struct._string, %struct._string* %11, i64 0, i32 2, i64 %13
  %15 = getelementptr %struct._string, %struct._string* %6, i64 0, i32 2, i64 %8
  tail call void @llvm.memcpy.p0i8.p0i8.i64 (i8* %14, i8* %15, i64 %10, i32 0, i1 0)
  ret i8** inttoptr (i64 14 to i8**)
}

;; this is just to make the linker happy.
;; currently there is no support for profiling the LLVM backend.
define void @prof_dump() { ret void }

;; ---- bignum helpers ----

;; note: there doesn't seem to be any speed advantage to copying the
;;  compiled llvm for mul2/div2b1 here.

;; wrapper for header1.c/irk_mul2
define internal fastcc i8** @irk_ll_mul2 (i8** %a, i8** %b, i8** %r) {
  %a0 = call fastcc i64 @insn_unbox (i8** %a)
  %b0 = call fastcc i64 @insn_unbox (i8** %b)
  %r0 = call i8** @irk_mul2 (i64 %a0, i64 %b0, i8** %r)
  ret i8** %r0
}

;; wrapper for header1.c/irk_div2b1
define internal fastcc i8** @irk_ll_div2b1 (i8** %ah, i8** %al, i8** %b, i8** %r) {
  %ah0 = call fastcc i64 @insn_unbox (i8** %ah)
  %al0 = call fastcc i64 @insn_unbox (i8** %al)
  %b0 = call fastcc i64 @insn_unbox (i8** %b)
  %u = call i8** @irk_div2b1 (i64 %ah0, i64 %al0, i64 %b0, i8** %r)
  ret i8** %u
}

define internal fastcc i8** @irk_popcount (i8** %n) {
  %n0 = call fastcc i64 @insn_unbox (i8** %n)
  %r0 = call i64 @llvm.ctpop.i64 (i64 %n0)
  %r1 = call fastcc i8** @insn_box (i64 %r0)
  ret i8** %r1
}

define internal fastcc i8** @irk_readcyclecounter () {
  %r0 = call i64 @llvm.readcyclecounter()
  %r1 = call fastcc i8** @insn_box (i64 %r0)
  ret i8** %r1
}

;; --- generated code follows ---
