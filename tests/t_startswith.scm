;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(defmacro verify
  (verify) -> #t
  (verify truth test rest ...)
  -> (if (not (eq? truth test))
	 (error "failed assertion")
	 (verify rest ...))
  )

(verify
 #t (starts-with "moose" "moo")
 #f (starts-with "noose" "moo")
 #f (starts-with "moo" "moose")
 #t (starts-with "abcd" "")
 #f (starts-with "" "abcd")
 #f (starts-with "foo" "bar")
 #f (starts-with "foo" "fox")
 #f (starts-with "foo" "fox")
 )
