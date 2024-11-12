;; -*- Mode: Irken -*-

(define (eq? a b)
  (%backend c (%%cexp ('a 'a -> bool) "%0==%1" a b))
  (%backend (llvm arm) (%lleq #f a b))
  (%backend bytecode (%%vminsn ('a 'a -> bool) 'eq a b))
  )

(define thing
  #\A -> 1
  #\B -> 2
  #\C -> 3
  _   -> 4
  )

(thing #\C)




