;; -*- Mode: Irken -*-

(require "lib/basis.scm")
;(require-ffi 'posix)

(defmacro parray
  (parray a n w) ;; array N width
  -> (for-range i n
       (printf (repeat w " ")
               (zpad (* w 2)
                     (hex (c-get-int (c-aref a i)))))))

(let ((v (halloc u8 40))           ;; u8[40]
      (w (%c-cast (array u16) v))  ;; u16[20]
      (x (%c-cast (array u32) v))) ;; u32[10]

  ;; 00 01 02 ...
  (for-range i 40
    (c-set-int (c-aref v i) i))


  ;; (printn (%cref->int #f v))

  ;; dump the array in all three interpretations...
  (parray v 40 1)
  (printf "\n")
  (parray w 20 2)
  (printf "\n")
  (parray x 10 4)
  (printf "\n")

  ;;(%arm ((-> undefined) (com "HERE")))

  ;; set as u32 values...
  (for-range i 10
    (c-set-int (c-aref x i) (>> #xdeadbeef (* 4 i))))

  ;; dump again
  (parray v 40 1)
  (printf "\n")
  (parray w 20 2)
  (printf "\n")
  (parray x 10 4)
  (printf "\n")

  (printf (hex (>> #xdeadbeef 4)) "\n")
  (printf (hex (>> #xdeadbeef 8)) "\n")

  )

