;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(define big-endian?
  (let ((val0* (halloc u32 1))
        (val1* (%c-cast (array u8) val0*)))
    (define (get n)
      (c-get-int (c-aref val1* n)))
    (c-set-int (c-aref val0* 0) #xf00fda)
    (match (get 0) (get 1) (get 2) with
      #xda #x0f #xf0 -> #f
      #xf0 #x0f #xda -> #t
      x y z
      -> (begin
           (printf "vals " (int x) " " (int y) " " (int z) "\n")
           (raise (:I_Am_Confused)))
      )))

(printf
 (match big-endian? with
   #t -> "big-endian"
   #f -> "little-endian")
 "\n")


