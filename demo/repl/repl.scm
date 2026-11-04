;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "demo/repl/eval.scm")

;; --- s-expression input ---

(define (ask prompt ifile ofile)
  (file/write ofile prompt)
  (file/flush ofile)
  (match (file/read-line ifile) with
    (maybe:yes line) -> line
    (maybe:no)       -> ""
    ))

(define (repl-error what)
  (printf "error: " what "\n")
  (univ:undef)
  )

(define (eval-string s namespace)
  (eval (car (read-string s)) namespace))

(define (setup-initial-environment)
  (varset 'x (univ:int 34) namespace)
  (varset '+ (eval-string "(lambda (a b) (%+ a b))" namespace) namespace)
  (varset '- (eval-string "(lambda (a b) (%- a b))" namespace) namespace)
  (varset '* (eval-string "(lambda (a b) (%* a b))" namespace) namespace)
  (varset '/ (eval-string "(lambda (a b) (%/ a b))" namespace) namespace)
  )

(define (read-eval-print-loop ifile ofile)
  (setup-initial-environment)
  (let loop ((line (ask "> " ifile ofile)))
    (match (string-length line) with
      0 -> #u
      _ -> (begin
             (for-list exp (read-string line)
               (let ((val (eval exp namespace)))
                 ;; store last value in `_`.
                 (varset '_ val namespace)
                 (printf (univ-repr val) "\n")))
             (loop (ask "> " ifile ofile)))
      )))

(read-eval-print-loop (file/open-stdin) (file/open-stdout))
