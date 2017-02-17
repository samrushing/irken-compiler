
(define (llvm-thing a b)
  (%%cexp (int int -> int) "%0+%1" a b)
  ;;(%plus #f a b)
  )

(llvm-thing 3 4)

;; plus:
;; %7 = and i64 %6, -2
;; %8 = ptrtoint i8* %3 to i64
;; %9 = add i64 %7, %8
;; %10 = or i64 %9, 1
;; %11 = inttoptr i64 %10 to i8**
