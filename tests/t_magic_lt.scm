;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(defmacro test
  (test val exp) -> (assert (eq? exp val))
  )

(test #f (magic<? '(1 2) '(1 1)))
(test #t (magic<? 19 34))
(test #f (magic<? 34 19))
(test #t (magic<? #\a #\b))
(test #f (magic<? #\b #\a))
(test #t (magic<? '(88) '(99)))
(test #f (magic<? '(99) '(88)))
(test #t (magic<? '(99) '(99 21)))
(test #f (magic<? '(99 21) '(99)))
(test #f (magic<? '(1 2 3) '(1 2 3)))
(test #f (magic<? '(1 2 3) '(0 2 3)))
(test #t (magic<? '(0 2 3) '(1 2 3)))
(test #t (magic<? '(1 2 3 4 5) '(1 2 3 4 6)))
(test #t (magic<? '(0 2 3 4 6) '(1 2 3 4 5)))
(test #t (magic<? '(1 2 3 4 5) '(1 2 4 9 8)))
(test #f (magic<? '(1 2 4 9 8) '(1 2 3 4 5)))

(datatype thing
  (:one)
  (:two)
  (:three int)
  )

(printn (sort magic<? (list (thing:two) (thing:three 7) (thing:one) (thing:two) (thing:three 12))))

    
