;; -*- Mode: Irken -*-

(define (make-counter init)
  (let ((value init))

    (define (inc)
      (let ((r value))
	(set! value (+ 1 value))
	r))

    (define (get)
      value)

    (define (dec)
      (let ((r value))
	(set! value (- value 1))
	r))

    {inc=inc get=get dec=dec}
    ))
