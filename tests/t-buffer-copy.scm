;; -*- Mode: Irken -*-

(require "lib/core.scm")
;; (require "lib/alist.scm")
;; (require "lib/pair.scm")
;; (require "lib/format.scm")
;; (require "lib/string.scm")

(define A "the quick brown fox jumped over the lazy dog")
(define B "                                            ")

(define (buffer-copy src src-start n dst dst-start)
  (%asm-c-call
   ("irk_buffer_copy" (string int int string int -> undefined) #f)
   src src-start n dst dst-start))

(buffer-copy A 4 5 B 0)
(printn B)
