;; -*- Mode: Irken -*-

(include "lib/core.scm")

;; test macros that output record literals and attribute references.

(defmacro thing
  (thing a b)
  -> {x=a y=b}
  )

(defmacro yattr
  (yattr ob)
  -> ob.y
  )

(defmacro attr
  (attr ob z)
  -> ob.z
  )

(defmacro mkrec
  (mkrec field val)
  -> {field=val}
  )

(let ((o (thing 1 2)))
  (printn (+ (yattr o) (attr o x)))
  (printn (mkrec z 34))
  )



  
