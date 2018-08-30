;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/alist.scm")

;; ok, here's how you would have to build a table without a macro facility:

(define numbers
  (alist:entry
   0 'zero
   (alist:entry
    1 'one
    (alist:entry
     2 'two
     (alist:entry
      3 'three
      (alist:entry
       4 'four
       (alist:entry
        5 'five
        (alist:entry
         6 'six
         (alist:entry
          7 'seven
          (alist:entry
           8 'eight
           (alist:entry
            9 'nine
            (alist:nil))))))))))))

(printn (alist/lookup numbers 6))

;; using the macro

(define numbers2
  (alist/make
   (0 'zero)
   (1 'one)
   (2 'two)
   (3 'three)
   (4 'four)
   (5 'five)
   (6 'six)
   (7 'seven)
   (8 'eight)
   (9 'nine)
   ))

(printn (alist/lookup numbers2 7))
(printn (alist/lookup numbers2 100))

(alist/push numbers2 100 'one-hundred)

(printn numbers2)

(define thingies
  (alist/make
   ('a "a")
   ('b "b")
   ('c "c")
   ))

(printn thingies)
