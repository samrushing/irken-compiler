;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(datatype thing
  (:int int)
  (:str string)
  (:sym symbol)
  )

(LIST (thing:int 1) (thing:str "two") (thing:sym 'three))
  