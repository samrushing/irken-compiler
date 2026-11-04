;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(let ((fd (open
           "thing.txt"
           (logior* O_TRUNC O_WRONLY O_CREAT)
           #o644))
      (s "testing\n")
      (slen (string-length s)))
  (printn fd)
;;  (posix/write fd (%c-cast void (%string->cref #f s)) slen)
  (printn (posix/write fd (%c-cast void (%string->cref #f s)) slen))
  (printn (posix/close fd))
  )
