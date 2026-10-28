;; -*- Mode: Irken -*-
;; c-get-int must sign-extend signed C types (e.g. kevent's `short filter`).
;; (`char` is left out: its signedness differs between platforms.)
(require "lib/basis.scm")

(let ((s* (c-aref (halloc short 1) 0))
      (i* (c-aref (halloc int 1) 0))
      (us* (c-aref (halloc ushort 1) 0))
      (ui* (c-aref (halloc uint 1) 0)))
  (c-set-int s* -1)
  (c-set-int i* -7)
  (c-set-int us* -1)
  (c-set-int ui* -1)
  (printn (list (c-get-int s*) (c-get-int i*) (c-get-int us*) (c-get-int ui*)))
  (assert (= -1 (c-get-int s*)))
  (assert (= -7 (c-get-int i*)))
  (assert (= 65535 (c-get-int us*)))
  (assert (= 4294967295 (c-get-int ui*)))
  (printf "ok\n"))
