;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/os.scm")

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


