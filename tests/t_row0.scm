
(include "lib/core.scm")

(datatype thing
  (:one {x=int y=char ...})
  (:two int)
  )

(let ((x (thing:one {x=3 y=#\A}))
      (y (thing:one {x=1 y=#\Z z=3}))
      (z (thing:two 9))
      ;;(y (thing:one {x=1 y=#\Z}))
      )
  (printn x)
  (printn y)
  (printn z)
  )
