;; -*- Mode: Irken -*-

(require "lib/core.scm")

(let loop ((n 0))
  (if (= n 20)
      (%exit #f 42)
      (if (< n 200000)
          (begin
            (printn n)
            (loop (+ n 1))))))

