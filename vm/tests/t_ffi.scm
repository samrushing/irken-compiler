;; -*- Mode: Irken -*-

(include "lib/core.scm")

;; test ffi from irken-vm

(define write 
  (let ((p (%%cexp (string -> int) "dlsym" "write\x00")))
    (lambda (fd s)
      (%%cexp (int int int string int -> int)
              "ffi"
              p 3 fd s (string-length s))
      )))

(write 2 "yabba dabba dooooooo\n")

  
