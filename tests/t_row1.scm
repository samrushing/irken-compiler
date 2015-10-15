
(include "lib/core.scm")

(datatype thing
  (:one {x=int y=thing ...})
  (:two int)
  )

(define frob
  (thing:two x) -> (thing:two (* x 2))
  (thing:one r) -> (thing:one {x=(* r.x 2) y=(frob r.y)})
  )

(let ((x (thing:one {x=3 y=(thing:two 9)})))
  (printn x)
  (printn (frob x))
  )
