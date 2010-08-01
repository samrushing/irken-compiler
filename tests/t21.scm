;; file i/o

;; XXX needs to catch errors

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/io.scm")

;; copy file to stdout
(let ((f (file/open-read "gc.c")))
  (let loop ((buffer (file/read-buffer f)))
    (cond ((> (string-length buffer) 0)
	   (write 1 buffer)
	   (loop (file/read-buffer f)))))
  (file/close f))

;; read a few characters...
(let ((f (file/open-read "gc.c")))
  (let loop ((n 10))
    (cond ((= n 0) #t)
	  (else 
	   (printn (file/read-char f))
	   (loop (- n 1)))))
  (file/close f))

;; write a file by chars...
(let ((f (file/open-write "thing.txt" #t #o644)))
  ;;(print-string "fd=") (printn f.fd)
  (file/write-char f #\H)
  (file/write-char f #\o)
  (file/write-char f #\w)
  (file/write-char f #\d)
  (file/write-char f #\y)
  (file/write-char f #\newline)
  (file/flush f)
  (file/close f)
  )
  
