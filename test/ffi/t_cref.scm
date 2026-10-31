;; -*- Mode: Irken -*-

;; NULL pointers and %cref->int.  (riscv64's %cref->int once clobbered its
;;  own result, so every cref looked non-NULL and getenv of an unset
;;  variable crashed in strlen.)

(include "lib/basis.scm")

(printn (cref-null? NULL))
(printn (cref-null? (cstring "hello")))
;; => #t
;; => #f

(printn (getenv "IRKEN_TEST_SURELY_NOT_SET"))
(printn (getenv-or "IRKEN_TEST_SURELY_NOT_SET" "default"))
;; => ""
;; => "default"

(printn (= 0 (%cref->int #f (%int->cref #f 0))))
(printn (%cref->int #f (%int->cref #f 12345)))
;; => #t
;; => 12345
