;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(defmacro map-range-rev
  (map-range-rev vname num body ...)
  -> (let (($n num))
       (let $loop ((vname 0)
                   ($acc (list:nil)))
         (if (= vname $n)
             $acc
             ($loop (+ vname 1) (list:cons (begin body ...) $acc))))))

(printn (map-range     i 10 i))
(printn (map-range-rev i 10 i))
