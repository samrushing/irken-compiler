;; -*- Mode: Irken -*-

(%arm
 (( -> int)
  (adrp (s 0) "_lenv@GOTPAGE")
  ;; not yet: ldr x0, [x0, _lenv@GOTPAGEOFF]
  (mov (s 0) (s 1))
  (mov trg (s 0)))
 )


