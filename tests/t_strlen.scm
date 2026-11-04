;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(libc/strlen (cstring "howdythere\x00"))
