;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (popcount n)
  (%backend arm
    (%arm
     ((int -> int)
      (untag (a 0))
      (fmov (r d0) (a 0))
      (cnt.8b (r v0) (r v0))
      (uaddlv.8b (r h0) (r v0))
      (fmov (r w0) (r s0))
      (mov trg (r x0))
      (tag trg))
     n))
  (%backend amd
    (%amd
     ((int -> int)
      (untag (a 0))
      (popcnt (a 0) trg)
      (tag trg))
     n))
  ;; requires Zbb extension
  (%backend rv
    (%rv
     ((int -> int)
      (untag (a 0))
      (cpop trg (a 0))
      (tag trg))
     n))
  )

(printn (popcount 3141))



