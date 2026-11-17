;; -*- Mode: Irken -*-

;; a datatype every one of whose alternatives needs one already (here
;;  through another datatype) can never be built (#40).

;; expect-error: f_datatype_no_values.scm:10:11: error: datatype `yy` has no values: every alternative needs a `yy` or `zz` to make one

(include "lib/basis.scm")
(datatype ok (:nil) (:cons int ok))
(datatype yy (:y zz) (:w int yy))
(datatype zz (:z {p=yy}))
(printn (ok:nil))
