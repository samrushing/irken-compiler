
;; tests cexp type decls

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (random)
  (%%cexp (-> int) "random()"))

(define (fun p)
  ;; takes and returns an identity function
  (%%cexp (('a -> 'a) -> ('a -> 'a)) "%s" p))

(let ((x {a=1 b="two" c=#f}))
  ;; pass our identity function through <fun> then apply it
  (printn ((fun (lambda (x) x)) 19))
  (printn (:thing x (random))))
