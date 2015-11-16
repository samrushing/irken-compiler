;; -*- Mode: Irken -*-

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
