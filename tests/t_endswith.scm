;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(defmacro verify
  (verify) -> #t
  (verify truth test rest ...)
  -> (if (not (eq? truth test))
	 (error "failed assertion")
	 (verify rest ...))
  )

(verify
 #t (ends-with "moose" "ose")
 #f (ends-with "noose" "osf")
 #f (ends-with "moo" "moose")
 #t (ends-with "abcd" "")
 #f (ends-with "" "abcd")
 #f (ends-with "foo" "bar")
 #f (ends-with "foo" "fox")
 #f (ends-with "foo" "fox")
 )
