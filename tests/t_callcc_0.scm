(include "lib/core.scm")
(%printn 1)
(%printn 2)
(%printn 3)
(%printn 4)
(^call/cc (lambda (exit) (%printn 5) (%printn 6) (exit 3) (%printn 7) (%printn 8)))
(%printn 9)
88
