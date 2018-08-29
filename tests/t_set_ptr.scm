;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(let ((int5* (halloc ushort 5))
      (int5** (halloc (* ushort) 5))
      (p0 (c-aref int5** 0))
      (p1 (c-aref int5* 0))
      )
  (c-set-int p1 3141)
  (c-set-ptr p0 p1)
  (assert (= 3141 (c-get-int (c-get-ptr p0))))
  (for-range i 5
    (c-set-int (c-aref int5* i) (* i i))
    (c-set-ptr (c-aref int5** i) (c-aref int5* i))
    )
  (let ((r '()))
    (for-range i 5
      (push! r (c-get-int (c-aref int5* i))))
    (assert (eq? (cmp:=) (list-cmp int-cmp r '(16 9 4 1 0))))
    )
  )
