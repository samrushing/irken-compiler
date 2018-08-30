
(include "lib/core.scm")
(include "lib/pair.scm")

(define v1 #(#(0 1 2) #(3 4 5) #(6 7 8)))
(printn v1[0][0])
(set! v1[2][2] 99)
(printn v1)
(define v2 #(#(#(0 1) #(2 3) #(4 5))
             #(#(6 7) #(8 9) #(10 11))
             #(#(12 13) #(14 15) #(16 17))))
(set! v2[1][1][0] 99)
(printn v2)
