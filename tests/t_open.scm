;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/io.scm")

(define tagsize 16)

(let ((f (file/open-write "thing.txt" #t #o644)))
  (file/write f "testing\n")
  (file/flush f)
  (file/close f)
  )
