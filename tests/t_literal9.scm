
(include "lib/core.scm")
(include "lib/pair.scm")

(define (thing1 a b) a)
(define (thing2 a b) b)

;(define v0 #(thing1 thing2))
;(v0[0] 3 4)

(datatype X
  (:t string int))

(define opcode-info
  #((X:t "lit" 2)
    (X:t "ret" 1)
    (X:t "add" 3)
    (X:t "sub" 3)
    (X:t "eq" 3)
    (X:t "tst" 2)
    (X:t "jmp" 1)
    (X:t "fun" 2)
    (X:t "tail" 2)
    (X:t "tail0" 1)
    (X:t "env" 2)
    (X:t "arg" 3)
    (X:t "ref" 3)
    (X:t "mov" 2)
    (X:t "push" 1)
    (X:t "trcall" 3)
    (X:t "ref0" 2)
    (X:t "call" 3)
    (X:t "pop" 1)
    (X:t "ge" 3)
    (X:t "print" 1)
    ))

(printn opcode-info)
