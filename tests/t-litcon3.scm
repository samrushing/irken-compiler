;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

'("testing" "fnord" "one" "two")
