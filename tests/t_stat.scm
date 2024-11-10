;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/posix.scm")

(printn (stat sys.argv[1]))
(printn (isdir? sys.argv[1]))

