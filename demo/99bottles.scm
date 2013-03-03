;; -*- Mode: Irken -*-

;; http://www.99-bottles-of-beer.net/language-irken-2823.html

(include "lib/basis.scm")

(define B
  0 -> "no more bottles"
  1 -> "1 bottle"
  x -> (format (int x) " bottles")
  )

(for-range
    i 99
    (let ((n (- 99 i)))
      (printf (B n) " of beer on the wall, " (B n) " of beer.\n"
	      "Take one down and pass it around, "
	      (B (- n 1)) " of beer on the wall.\n\n")))

(printf "No more bottles of beer on the wall, no more bottles of beer.\n")
(printf "Go to the store and buy some more, 99 bottles of beer on the wall.\n")
