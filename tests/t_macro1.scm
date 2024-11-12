;; -*- Mode: Irken -*-

(defmacro rtest
  (rtest x)
  -> (%%record (x 0) (y 0))
  )

(rtest z)
