;; -*- Mode: Irken -*-

(typealias thing int)

(datatype blob
  (:one thing thing)
  (:two thing thing)
  )

(blob:one 42 0)
;43
