;; -*- Mode: Irken -*-

;; a type alias is expanded where it's used: it can't refer to itself (#40).

;; expect-error: f_alias_recursive.scm:8:37: error: type alias `tree` refers to itself

(include "lib/basis.scm")
(typealias tree {val=int kids=(list tree)})
(printn 1)
