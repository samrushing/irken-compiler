;; -*- Mode: Irken -*-

(define (thing)
  (%arm
   (( -> undefined)
    (adrp trg "_top@PAGE")
    (ldr trg (ind trg "_top@PAGEOFF"))
    (ldr trg (ind trg (i 16))))))

(thing)



