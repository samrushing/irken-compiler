;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(require-ffi 'posix)
(require-ffi 'stdio)

;; NOTE: stdin/stdout/stderr cannot be portably accessed via
;;  the LLVM backend, because they are macros, not globals.
;;  The problem can be sorta worked around with `fdopen`.

(defmacro fwrite
  (fwrite s stream)
  -> (stdio/fwrite s 1 (posix/strlen s) stream)
  )

(let ((path (cstring "/tmp/thing.txt"))
      (wb (cstring "wb"))
      (rb (cstring "rb"))
      (data (cstring "testing, testing\n"))
      (file (stdio/fopen path wb))
      (buffer (%c-aref char (halloc char 100) 0))
      ;;(stdout (%c-pref #f stdio/__stdoutp)) ;; freebsd/macos
      (stdout (stdio/fdopen 1 wb))
      (stderr (stdio/fdopen 2 wb)))
  ;; write some data & close the file
  (fwrite data file)
  (stdio/fclose file)
  ;; open the file in read mode...
  (set! file (stdio/fopen path rb))
  ;; and read some data
  (let ((nbytes (stdio/fread buffer 1 100 file))
        (data0 (%cref->string #f buffer nbytes)))
    (printf (int nbytes) " bytes of data read from file: " (string data0) "\n"))
  (fwrite data stdout)
  (stdio/fflush stdout)
  (fwrite (cstring "Hey, this is stderr!\n") stderr)
  )
