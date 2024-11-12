;; -*- Mode: Irken -*-

(define (print x)
  (%backend c (%%cexp ('a -> undefined) "dump_object (%0, 0)" x))
  (%backend llvm (%llvm-call ("@irk_dump_object" ('a -> undefined) ccc) x))
  (%backend arm (%arm-c-call ("irk_dump_object" ('a -> undefined)) x))
  (%backend bytecode (%%vminsn ('a -> undefined) 'printo x))
  )

(define (newline)
  (%backend c (%%cexp (char -> int) "fputc (GET_CHAR(%0), stdout)" #\newline))
  (%backend llvm (%llvm-call ("@irk_putc" (char -> undefined) ccc) #\newline))
  (%backend arm (%arm-c-call ("irk_putc" (char -> undefined)) #\newline))
  (%backend bytecode (print-string "\n")))

(define (printn x)
  (print x)
  (newline))

(define (string-length s)
  (%backend c (%%cexp ((raw string) -> int) "%0->len" s))
  (%backend llvm (%llvm-call ("@irk_string_len" (string -> int)) s))
  (%backend bytecode (%%vminsn (string -> int) 'slen s))
  (%backend arm
    (%arm
     ((string -> int)
      (ldrsw trg (ind (a 0) (i 8))) ;; trg = irk_string.len
      (lsl trg trg (i 1))           ;; trg <<= 1
      (orr trg trg (i 1)))          ;; trg |= 1
     s))
  )

(define (string-compare a b) : (string string -> cmp)
  ;; incorrect because needs raw strings.
  (%arm-c-call ("irk_string_cmp" (string string -> cmp) #f) a b))

(let ((s "hello, world!\n"))
  (%arm-c-call
   ("write" (int string int -> int))
   0 s (string-length s)
   )
  (printn (string-compare "no" "yes"))
  (printn (string-compare "yes" "yes"))
  (printn (string-compare "yes" "no"))
  (%exit #f 107)
  )
