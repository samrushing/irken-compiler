;; -*- Mode: Irken -*-

(define (print x)
  (%backend c (%%cexp ('a -> undefined) "dump_object (%0, 0)" x))
  (%backend llvm (%llvm-call ("@irk_dump_object" ('a -> undefined) ccc) x))
  (%backend bytecode (%%vminsn ('a -> undefined) 'printo x))
  )

(define (newline)
  (%backend c (%%cexp (char -> int) "fputc (GET_CHAR(%0), stdout)" #\newline))
  (%backend llvm (%llvm-call ("@irk_putc" (char -> undefined) ccc) #\newline))
  (%backend bytecode (print-string "\n")))

(define (printn x)
  (print x)
  (newline)
  )

(define (eq? a b)
  (%backend c (%%cexp ('a 'a -> bool) "%0==%1" a b))
  (%backend llvm (%lleq #f a b))
  (%backend bytecode (%%vminsn ('a 'a -> bool) 'eq a b))
  )

(define (not x)
  (eq? x #f))

(datatype cmp
  (:<)
  (:=)
  (:>)
  )

(define cmp-repr
  (cmp:<) -> "<"
  (cmp:=) -> "="
  (cmp:>) -> ">"
  )

(define (magic-cmp a b)
  (%backend c
    ;; note: magic_cmp returns -1|0|+1, we adjust that to UITAG 0|1|2
    ;;  to match the 'cmp' datatype.
    (%%cexp ('a 'a -> cmp) "(object*)UITAG(1+magic_cmp(%0, %1))" a b))
  (%backend llvm
    (%llvm-call ("@irk_magic_cmp" ('a 'a -> cmp)) a b))
  (%backend bytecode
    (%%vminsn ('a 'a -> cmp) 'cmp a b))
  )

(define (magic<? a b)
  (eq? (cmp:<) (magic-cmp a b)))

(define (magic=? a b)
  (eq? (cmp:=) (magic-cmp a b)))

(define (char=? a b)
  (eq? a b))


(printn (eq? 19 34))
(printn (eq? #\A #\B))
