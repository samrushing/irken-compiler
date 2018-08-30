;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(libc/strlen (cstring "howdythere\x00"))
