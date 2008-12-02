(define resume #f)

(define make-coroutine
  (lambda (body)
    (let ((lcs #f))

      (define (newcoroutine value)
        (lcs value))

      (define (localresume cont value)
        (let ((receivedvalue
               (call/cc
                (lambda (localcont)
                  (set! lcs localcont)
                  (cont value)))))
          (set! resume localresume)
          receivedvalue
          ))
      
      (call/cc
       (lambda (exit)
         (body (localresume exit newcoroutine))
         (error))))))
                   
(define make-samefringe-coroutine
  (lambda (driver tree)
    (make-coroutine
     (lambda (initvalue)
       (define (traverse tree)
         (cond ((pair? tree)
                (traverse (car tree))
                (if (pair? (cdr tree))
                    (traverse (cdr tree))
                    0))
               (else (resume driver tree))))
       (traverse tree)
       (resume driver 0)))))
                                    
(define samefringe
  (lambda (tree1 tree2)
    (call/cc
     (lambda (return-cont)
       (let ((co1 #f)
             (co2 #f)
             (driver #f))
         (set! driver
               (make-coroutine
                (lambda (initvalue)
                  (let loop ()
                    (let ((leaf1 (resume co1 #f))
                          (leaf2 (resume co2 #f)))
                      (if (eq? leaf1 leaf2)
                          (if (zero? leaf1)
                              (return-cont 1)
                              (loop))
                          (return-cont 0)))
                    (loop)))))
         (set! co1 (make-samefringe-coroutine driver tree1))
         (set! co2 (make-samefringe-coroutine driver tree2))
         (driver #f)
         )))))

