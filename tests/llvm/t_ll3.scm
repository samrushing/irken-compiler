
(define (^llvm-thing)
  (let ((x 19))
    (set! x 34)
    x))

(^llvm-thing)
