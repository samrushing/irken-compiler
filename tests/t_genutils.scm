;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(for (i num) (counting-gen (list-generator (reverse (range 10))))
  (printf "i " (int i) " : " (int num) "\n"))

(for (last? num) (notify-last-gen (list-generator (range 10)))
  (printf (bool last?) " " (int num) "\n"))

