;; -*- Mode: Irken -*-

(define (urandom-make)
  (let ((fd (open "/dev/random" O_RDONLY 0)))
    (define (get n)
      (let ((bytes (read fd n)))
        (when (not (= (string-length bytes) n))
          (raise (:Urandom/Underflow n)))
        bytes))
    get
    ))
