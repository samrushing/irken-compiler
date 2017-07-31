;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(let ((buf (%halloc int 20)))
  (for-range j 1000000
    (let ((thing (make-string 1024)))
      (for-range i 20
        (let ((int* (%c-aref int buf i)))
          ;; (printn int*)
          (%c-set-int int (+ i 1234) int*)))))
  (for-range i 20
    (let ((int* (%c-aref int buf i)))
      (printf (lpad 3 (int i)) " " (lpad 8 (int (%c-get-int int int*))) "\n")
      ))
  (let ((thing (halloc int 1)))
    (free thing))
  )

