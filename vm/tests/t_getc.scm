;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define vm-get-object
  (let ((object-getter (%%cexp (-> (string -> 'a)) "getc")))
    (lambda (name)
      (%%cexp ((string -> 'a) int string -> 'a) "irk" object-getter 1 name))))

(define STDOUT_FILENO : (int) (vm-get-object "STDOUT_FILENO"))
(define O_CREAT : (int) (vm-get-object "O_CREAT"))

(print STDOUT_FILENO)

(define open : (string int int -> int)
  (let ((open-closure (vm-get-object "open")))
    (lambda (name oflag mode)
      (%%cexp ((string int int -> int) int string int int -> int)
              "irk" open-closure 3 name oflag mode))))

(define write : (int string -> int)
  (let ((write-closure (vm-get-object "write")))
    (lambda (fd s)
      (%%cexp ((int string -> int) int int string -> int)
              "irk" write-closure 2 fd s))))

(define close : (int -> int)
  (let ((cl (vm-get-object "close")))
    (lambda (fd)
      (%%cexp ((int -> int) int int -> int)
              "irk" cl 1 fd))))

;; ;; (define (%typed (thing x) (int -> int)) (+ x 1)))
;; ;(define (thing x) : (int -> int)
;; ;  (+ x 1))

(let ((fd (open "/tmp/thing.tst" O_CREAT #o777)))
  (print fd)
  (write fd "testing, testing!\n")
  (close fd))


