;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(datatype thing
  (:int int)
  (:str string)
  (:sym symbol)
  )

(list (thing:int 1) (thing:str "two") (thing:sym 'three))
  
