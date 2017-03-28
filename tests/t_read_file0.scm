;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/os.scm")

(define (read-file path)
  (let ((ifile (file/open-read path)))
    (let loop ((buf (file/read-buffer ifile))
	       (l '()))
      (cond ((= (string-length buf) 0) (string-concat (reverse l)))
	    (else (loop (file/read-buffer ifile)
			(list:cons buf l)))))))

(let ((s (read-file (if (> sys.argc 1) sys.argv[1] "tests/t_read_file0.scm"))))
  (print-string s)
  (print-string (format "file length=" (int (string-length s)) "\n")))
