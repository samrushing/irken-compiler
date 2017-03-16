;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/os.scm")

(local-include "vm.c")

(define (read-bytecode-file path stsize)
  (%%cexp (string int -> int) "read_bytecode_file (%0, %1)" path symbol-table-size)
  )

(define (set-closure closure)
  (%%cexp ('a -> int) "vm_set_closure (%0)" closure))

(define (writex fd s)
  (printf "write fd=" (int fd) " s= " (string s) "\n")
  (write fd s))

(define boot-get-object
  "O_RDONLY"      -> (magic O_RDONLY)
  "O_RDWR"        -> (magic O_RDWR)
  "O_CREAT"       -> (magic O_CREAT)
  "STDOUT_FILENO" -> (magic STDOUT_FILENO)
  "open"          -> (magic open)
  "close"         -> (magic close)
  "write"         -> (magic writex)
  x               -> (error1 "boot-get-object" x)
  )

(define (go)
  (if (not (= -1 (read-bytecode-file 
                  (zero-terminate sys.argv[1])
                  symbol-table-size)))
      (begin
        (set-closure boot-get-object)
        (%%cexp (-> 'a) "vm_go()")
        )
      (printf "failed to read bytecode file\n"))
  )

(printn (boot-get-object "STDOUT_FILENO"))
(go)
