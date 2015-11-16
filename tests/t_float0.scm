
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (make-float) 
  (%%cexp (-> float) 
    "alloc_no_clear (TC_FLOAT, 1+(sizeof(double)/sizeof(pxll_int)))"))

(define (f+ a b)
  (let ((r (make-float)))
    (%%cexp (float float float -> float)
       "%0 = %1 + %2"
       r a b)
    r))
    
(define (f- a b)
  (let ((r (make-float)))
    (%%cexp (float float float -> float)
       "%0 = %1 - %2"
       r a b)
    r))

(define (f* a b)
  (let ((r (make-float)))
    (%%cexp (float float float -> float)
       "%0 = %1 * %2"
       r a b)
    r))

(define (f/ a b)
  (let ((r (make-float)))
    (%%cexp (float float float -> float)
       "%0 = %1 / %2"
       r a b)
    r))

(printn (f+ 3.14159 1.0000))
(printn (f- 3.14159 1.0000))
(printn (f* 3.14159 9.1234))
(printn (f/ 3.14159 4.0000))
