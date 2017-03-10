;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/os.scm")

;; Why include this tiny, useless wrapper?  Because we need vm.c to have access to
;;  everything in the Irken runtime.  This is an easy way to do that.

(local-include "vm.c")

(define (read-bytecode-file path)
  (%%cexp (string -> int) "read_bytecode_file(%0)" path)
  )

(define (go)
  (if (not (= -1 (read-bytecode-file (zero-terminate sys.argv[1]))))
      (%%cexp (-> 'a) "vm_go()")
      (printf "failed to read bytecode file\n"))
  )

(go)
