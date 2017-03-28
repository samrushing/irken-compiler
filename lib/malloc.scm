;; -*- Mode: Irken -*-

(datatype malloc
  (:t int)
  )

;; Features to consider:
;; * use a record to track whether a pointer is valid or not.
;; * track all valid pointers in a map.
;; * a macro that automatically frees all memory allocated
;;   in that scope upon exit (i.e., a c++-style destructor)

;; Note: we treat addresses as integers here: this will fail if 
;;  the highest bit is set on the address. (which should be rare).
(define (malloc size)
  (let ((addr (%%cexp (int -> int) "malloc((size_t)%0)" size)))
    (if (= addr 0)
        (raise (:MallocFailed))
        (malloc:t addr))))

(define free 
  (malloc:t r)
  -> (%%cexp (int -> undefined) "(free((void*)%0), PXLL_UNDEFINED)" r)
  )

(define malloc/addr
  (malloc:t addr) -> addr
  )
