;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(printn (string-find "xyz" "-------xyz----------"))
(printn (string-find "xyz" "--------------------"))
(printn (string-find "xyz" "------------------xy"))
(printn (string-find "xyz" "xyz-----------------"))
(printn (string-find "xyz" "-----------------xyz"))
(printn (string-find "xyz" "xy---------------xyz"))
(printn (string-find "xyz" "xy------------------"))
(printn (string-find "xyz" "-----xyxyxyz--------"))
