;; -*- Mode: Irken -*-


(require "lib/basis.scm")
(require "lib/mimetypes.scm")

(printn (guess-mime-type "heic"))
(printn (guess-mime-type "nobody"))
