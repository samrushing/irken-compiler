;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/format.scm")

(printn (string-split "quick brown fox" #\space))
(printn (string-split "%%vcon/list/cons" #\/))
(printn (string-split "thing" #\/))
(printn (string-split "" #\a))
(printn (string-split "..." #\.))
(printn (string-split "%0 %% %1" #\%))

