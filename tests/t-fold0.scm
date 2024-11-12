;; -*- Mode: Irken -*-

(require "lib/core.scm")
(require "lib/alist.scm")
(require "lib/pair.scm")
(require "lib/string.scm")
(require "lib/format.scm")

;; merge a list of strings into one string
(define zing-concat
  ;; handle some edge cases
  ()    -> ""
  (one) -> one
  l     -> (let ((tsize (fold (lambda (s acc) (+ (string-length s) acc)) 0 l))
	         (buffer (make-string tsize)))
             (let loop ((l0 l) (pos 0))
               (match l0 with
                 ()        -> buffer
	         (hd . tl) -> (begin
		                (buffer-copy hd 0 (string-length hd) buffer pos)
		                (loop tl (+ pos (string-length hd))))))))


(printn (fold binary+ 0 '(1 2 3 4 5)))
(printn (foldr list:cons '() '(a b c d e fg)))
(define strings '("one" "of" "these" "days"))
(printn (fold (lambda (s acc) (+ (string-length s) acc)) 0 strings))

(zing-concat strings)

