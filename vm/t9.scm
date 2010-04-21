(let loop ((n 1000000))
  (if (%= 0 n)
      42
      (loop (%- n 1))))

