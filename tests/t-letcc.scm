;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))
(define (= a b) (%llicmp eq a b))
(define (+ a b) (%llarith add a b))

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

(define (thing n)
  (let/cc exit
    (for-range
        i 1000
      (printn i)
      (if (= i n)
          (exit i))
      )
    1001))

(define (test)
  (printn "before")
  (printn (thing 20))
  (printn "after"))

(test)
