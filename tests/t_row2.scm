
;;(include "lib/core.scm")

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(datatype glork
  (:x 'a)
  (:y 'a 'a))

(datatype thing
  (:one {x=int ...})
  )

(let ((x (thing:one {x=3}))
      (y (thing:one {x=1 y=3}))
      )
  (printn x)
  (printn y)
  )
