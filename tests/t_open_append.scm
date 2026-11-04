;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/io.scm")
(require "lib/os.scm")

(try
 (unlink "test-file.txt")
 except (:OSError _)
 -> 0
 )

(let ((f (file/open-append "test-file.txt" #o644)))
  (file/write f "first line\n")
  (file/flush f)
  (file/close f)
  (printf "first write done.\n")
  (let ((f0 (file/open-append "test-file.txt" #o644)))
    (file/write f "second line\n")
    (file/flush f)
    (file/close f))
  (printf "second write done.\n")
  (let ((f1 (file/open-read "test-file.txt")))
    (printf "contents: " (string (file/read-buffer f1)) "\n")
    ))


