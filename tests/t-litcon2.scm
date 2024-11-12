;; -*- Mode: Irken -*-

(define thing {x=#(0 1 2) y=#(#\a #\b #\c)})

(set! thing.y[1] #\A)
thing

