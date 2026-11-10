;; -*- Mode: Irken -*-

(require "lib/core.scm")

(defmacro rtest
  (rtest x)
  -> (%%record (x 0) (y 0))
  )

(defmacro rtest2
  (rtest2 x) -> {x=0 y=1}
  )

(printn (rtest z))
(printn (rtest2 yowsa))

