;; -*- Mode: Irken -*-

;; the HTTP/2 server (make-h2-conn, doom/http/h2.scm) and client
;;  (h2/client, doom/http/h2client.scm) in one program.  both raise
;;  :H2/BadFrame, and exceptions are typed program-wide, so this only
;;  compiles if they give it the same type: a string saying what was
;;  wrong.  (they once didn't: the server raised the frame itself.)
;;
;; unused code isn't type-checked, so both are called, in a branch
;;  that never runs.

;; skip: b -- the VM can't start a program that has make-h2-conn in it (before this change too)

(require "lib/basis.scm")
(require "lib/rope.scm")
(require "doom/doom.scm")
(require "doom/http/h2.scm")
(require "doom/http/h2client.scm")

(when (= sys.argc 12345)
  ;; stand-ins for the connections: just the fields each side uses.
  ((make-h2-conn "127.0.0.1"
                 {recv-exact=(lambda (n) "") send-rope=(lambda (r npad) #u)}
                 (lambda (request) (request.send-headers "404" (list:nil) #f))))
  (let ((c (h2/client {recv-exact=(lambda (n) "") send=(lambda (s) #u)})))
    (c.request "GET" "localhost" "/" (list:nil) "")
    #u))

(define (bad-frame why)
  (try
   (raise (:H2/BadFrame why))
   except
   (:H2/BadFrame what) -> (format "bad frame: " what)))

(printf (bad-frame "SETTINGS payload of 5 bytes, not a multiple of 6") "\n")
(printf (bad-frame "padding") "\n")
;; => bad frame: SETTINGS payload of 5 bytes, not a multiple of 6
;; => bad frame: padding
