;; -*- Mode: Irken -*-

(define (+ a b)
  (%backend c (%%cexp (int int -> int) "%0+%1" a b))
  (%backend (llvm arm) (%llarith add a b))
  (%backend bytecode (%%vminsn (int int -> int) 'add a b))
  )
(define (- a b)
  (%backend c (%%cexp (int int -> int) "%0-%1" a b))
  (%backend (llvm arm) (%llarith sub a b))
  (%backend bytecode (%%vminsn (int int -> int) 'add a b))
  )

(let ((z 100)
      (ev #())
      (thing1 {x=123 y="testing"})
      (thing2 {yabba=#() gabba=#(1 2 3)})
      )
  (- thing1.x z))

