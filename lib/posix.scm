;; -*- Mode: Irken -*-

;; This should really go into lib/os.scm or somewhere like that.
;; *or* we put posix-only stuff here.

(require-ffi 'posix)

(define (uname)
  (let ((utsname* (malloc (struct utsname)))
        (_ (syscall (posix/uname utsname*)))
        (result
         {sysname  = (get-cstring (%c-sref utsname.sysname utsname*))
          nodename = (get-cstring (%c-sref utsname.nodename utsname*))
          release  = (get-cstring (%c-sref utsname.release utsname*))
          version  = (get-cstring (%c-sref utsname.version utsname*))
          machine  = (get-cstring (%c-sref utsname.machine utsname*))}))
    (free utsname*)
    result
    ))
