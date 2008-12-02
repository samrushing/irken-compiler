(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define (dump filename thunk)
  (%%verify "TC_STRING" 1 filename)
  (%%cexp "dump_image (GET_STRING_POINTER(%s), %s)" filename thunk)
  #f)

(define (load filename)
  (%printn "calling load()...")
  (%%cexp "load_image (GET_STRING_POINTER(%s))" filename))

(define (thingy)
  (%printn "howdy there!\n"))

(%printn (sys.argv 0))
(if (and (> (sys.argc) 1) (string-=? (sys.argv 1) "-d"))
    (dump "test.img" thingy)
    ((load "test.img")))
