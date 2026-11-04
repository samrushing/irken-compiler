;; -*- Mode: Irken -*-

(define thing1 (%%platform Darwin 1 else 2))
(define thing2 (%%platform (Darwin Linux) 2 else 3))
(define thing3 (%%platform (Linux Darwin) 2 else 3))
(define thing4 (%%platform Nope1 1 Nope2 2 Nope3 3 else 4))

(:tuple thing1 thing2 thing3 thing4)
  
