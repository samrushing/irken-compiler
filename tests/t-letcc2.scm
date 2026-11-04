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

(define (= a b)
  (%llicmp eq a b)
  )

(define (+ a b)
  (%llarith add a b))

(defmacro getcc
  (getcc) -> (%getcc #f))

(defmacro putcc
  (putcc k r) -> (%putcc #f k r))

(define (callcc p) : (((continuation 'a) -> 'a) -> 'a)
  (p (getcc)))

(define (throw k v)
  (putcc k v))

(defmacro let/cc
  (let/cc name body ...)
  -> (callcc
      (lambda ($k)
	(let ((name (lambda ($val)
		      (throw $k $val))))
	  body ...))))

(define (thing)
  (let/cc exit
    (printn 1)
    (printn 2)
    (printn 3)
    (exit 99)
    (printn 1)
    (printn 2)
    (printn 3)
    (printn 1)
    (printn 3)
    101
    ))

(printn (thing))

