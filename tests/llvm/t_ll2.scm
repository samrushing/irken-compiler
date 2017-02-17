;; -*- Mode: Irken -*-

(datatype bool
  (:true)
  (:false)
)

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (= a b)
  (%%cexp (int int -> bool) "%0==%1" a b))

(define boolvar #f)

(define (out0 a)
  (define (b)
    (define (c)
      (define (^llvm-thing) a)
      (^llvm-thing))
    (c))
  (b)
  )
(printn (out0 77))

;; ; Function Attrs: nounwind ssp uwtable
;; define internal void @FUN_lvm_thing_15_0() #0 {
;;   %r24 = alloca i8**, align 8
;;   %1 = load i8*** @lenv, align 8
;;   %2 = bitcast i8** %1 to i8*****
;;   %3 = getelementptr inbounds i8***** %2, i64 1
;;   %4 = load i8***** %3, align 8
;;   %5 = getelementptr inbounds i8**** %4, i64 1
;;   %6 = load i8**** %5, align 8
;;   %7 = getelementptr inbounds i8*** %6, i64 1
;;   %8 = load i8*** %7, align 8
;;   %9 = getelementptr inbounds i8** %8, i64 2
;;   %10 = load i8** %9, align 8
;;   %11 = bitcast i8* %10 to i8**
;;   store i8** %11, i8*** %r24, align 8
;;   %12 = load i8*** %r24, align 8
;;   store i8** %12, i8*** @result, align 8
;;   %13 = load i8*** @k, align 8
;;   %14 = getelementptr inbounds i8** %13, i64 3
;;   %15 = load i8** %14, align 8
;;   %16 = bitcast i8* %15 to void ()*
;;   call void %16()
;;   ret void
;; }

