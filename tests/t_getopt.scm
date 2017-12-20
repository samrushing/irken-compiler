;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/getopt.scm")

;; -v -x 23 -name "test" config.scm file0.txt file1.txt ...

(define options0
  (LIST
   (option:flag 'v (optval:bool #f))             ;; -v
   (option:arg 'x (optval:int 0))                ;; -x 23
   (option:arg 'name (optval:string "test"))     ;; -name "test"
   (option:pos 'config #f #t (optval:string "")) ;; config.scm
   (option:pos 'inputs #t #t (optval:string "")) ;; file0.txt file1.txt ...
   ))

;; same thing, but using the makeopt DSL
(define options1
  (makeopt
   (flag 'v)
   (arg 'x (int 0))
   (arg 'name (string "test"))
   (pos 'config (string ""))
   (pos* 'inputs (string ""))
   ))

(define sample-args (LIST "-v" "-x" "23" "-name" "test" "config.scm" "file0.txt" "file1.txt"))
(define flags-at-end (LIST "config.scm" "file0.txt" "file1.txt" "-v" "-x" "23" "-name" "test"))
(define extra-args (LIST "-v" "-x" "23" "-z" "zzzz" "config.scm"))
(define missing-args (LIST "-v" "-x" "23" "-name" "test"))
(define default-args (LIST "-v" "-name" "test" "config.scm"))

(printn (process-options options0 sample-args))
(printn (process-options options1 sample-args))
(printn (process-options options1 flags-at-end))
(try
 (begin
   (process-options options1 extra-args)
   (error "test should have failed"))
 except (:Getopt/UnknownArg _ _)
 -> #u
 )

(try
 (begin
   (process-options options1 missing-args)
   (error "test should have failed"))
 except (:Getopt/MissingArg _ _)
 -> #u
 )

(let ((opts (process-options options1 sample-args)))
  (match (getopt opts 'config) with
    (maybe:yes (optval:string s))
    -> (printf "config value is " (string s) "\n")
    _ -> (impossible)))

(let ((opts (process-options options1 default-args)))
  (match (getopt opts 'x) with
    (maybe:yes (optval:int n))
    -> (printf "x default value is " (int n) "\n")
    _ -> (impossible)))
