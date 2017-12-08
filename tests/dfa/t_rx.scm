;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/counter.scm")
(include "lib/set3.scm")
(include "lib/dfa/charset.scm")
(include "lib/dfa/rx.scm")

(printf (rx-repr (parse-rx "[A-Z]+")) "\n")
(printf (rx-repr (parse-rx "(ab+)?")) "\n")
(printf (rx-repr (parse-rx "a*bca*")) "\n")
(printf (rx-repr (parse-rx "([abc]~)?[de]+")) "\n")
(printf (rx-repr (parse-rx "[a-z]\\[0")) "\n")

(let ((r0 (parse-rx "a+b*a+"))
      (r1 (parse-rx "a+b*a+")))
  (printf "r0 < r1 = " (bool (rx< r0 r1)) "\n")
  (printf "r1 < r0 = " (bool (rx< r1 r0)) "\n")
  )
(let ((r0 (parse-rx "a+"))
      (r1 (parse-rx "a+")))
  (printf "r0 < r1 = " (bool (rx< r0 r1)) "\n")
  (printf "r1 < r0 = " (bool (rx< r1 r0)) "\n")
  )

(printf (pp-rx (parse-rx "((ab)~c)~d*")) "\n")
(printf (pp-rx (parse-rx "{(ab)~c}~d*")) "\n")
