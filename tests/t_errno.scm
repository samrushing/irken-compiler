;; -*- Mode: Irken -*-
;; a failed syscall should report the real errno, and %c-sizeof a tagged int.
(require "lib/basis.scm")
(require "lib/posix.scm")
(try
 (begin (syscall (posix/close 9999)) #u)
 except
 (:OSError e) -> (begin (printn e) (assert (= e 9)))) ;; EBADF
(printn (%c-sizeof int))
(assert (= 4 (%c-sizeof int)))
(printf "ok\n")
