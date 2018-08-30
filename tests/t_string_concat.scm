(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(let ((sl
       (list:cons
	"not "
	(list:cons
	 "today "
	 (list:cons
	  "zurg!"
	  (list:nil))))))
  (print (string-concat sl)))
