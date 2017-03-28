;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/os.scm")

(local-include "vm.c")

(define (read-bytecode-file path)
  (%%cexp (string -> int) "read_bytecode_file (%0)" path))

;; this is an (admittedly kludgy) way of bootstrapping the vm->irken
;;  call interface: we provide a single "object-getting closure"...
(define (set-closure closure)
  (%%cexp ('a -> int) "vm_set_closure (%0)" closure))

;; ... and these are the exposed objects.
(define boot-get-object
  "O_RDONLY"      -> (magic O_RDONLY)
  "O_RDWR"        -> (magic O_RDWR)
  "O_CREAT"       -> (magic O_CREAT)
  "STDOUT_FILENO" -> (magic STDOUT_FILENO)
  "open"          -> (magic open)
  "close"         -> (magic close)
  "write"         -> (magic write)
  "argv"          -> (magic sys.argv)
  x               -> (error1 "boot-get-object" x)
  )

(define (go)
  (cond ((not (= -1 (read-bytecode-file (zero-terminate sys.argv[1]))))
         (set-closure boot-get-object)
         (%%cexp (-> 'a) "vm_go()"))
        (else
         (printf "failed to read bytecode file\n"))
        ))

(printn (boot-get-object "STDOUT_FILENO"))
(go)
