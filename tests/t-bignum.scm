;; -*- Mode: Irken -*-

(require "lib/core.scm")
(require "lib/pair.scm")
(require "lib/string.scm")
(require "lib/format.scm")
(require "lib/alist.scm")
(require "lib/sexp.scm")
(require "lib/queue.scm")
(require "lib/set.scm")
(require "lib/symbol.scm")
(require "lib/stdio.scm")
(require "lib/ctype.scm")
(require "lib/read.scm")
(require "lib/os.scm")
(require "lib/io.scm")
(require "lib/frb.scm")
(require "lib/enum.scm")
(require "lib/map.scm")
(require "lib/bignum.scm")

(define (big-fact n)
  (define recur
    1 acc -> acc
    n acc -> (recur (- n 1) (big-mul acc (int->big n)))
    )
  (recur n (int->big 1))
  )

(printf (big-repr (big-mul big/2 big/3)) "\n")
(printf (big-repr (big-fact 2)) "\n")
(printf (big-repr (big-fact 3)) "\n")
(printf (big-repr (big-fact 4)) "\n")
(printf (big-repr (big-fact 5)) "\n")

(printf (big-repr (big (dec "5617843187844953170308463622230283376298685"))) "\n")
(printn (big-repr (b256->big "rubber ducky")))
(list->vector '(30724728358 392411733372922745))


