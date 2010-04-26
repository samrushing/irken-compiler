
(datatype uimm
  (:INT int)
  (:CHAR char)
  (:pair int int)
;  (:THING int)
  )

(let ((x (uimm:INT 3)))
  (vcase uimm x
    ((:INT n) "gotcha")
    ((:CHAR ch) "not so much")
    ((:pair a b) "fuggedaboudit")
    ))
  
