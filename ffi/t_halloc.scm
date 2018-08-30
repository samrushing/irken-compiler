;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(require-ffi 'posix)

(define (get-cstring p) (%cref->string #f p (posix/strlen p)))

(let ((utsname0 (halloc (struct utsname)))
      (utsname1 (malloc (struct utsname))))
  (posix/uname utsname0)
  (posix/uname utsname1)
  (printf (string (get-cstring (%c-sref utsname.nodename utsname0))) "\n")
  (printf (string (get-cstring (%c-sref utsname.nodename utsname1))) "\n")
  (printf (string (get-cstring (%c-sref utsname.sysname utsname0))) "\n")
  (printf (string (get-cstring (%c-sref utsname.sysname utsname1))) "\n")
  (free utsname1)
  )
