;; -*- Mode: Irken -*-

;; This should really go into lib/os.scm or somewhere like that.
;; *or* we put posix-only stuff here.

(require-ffi 'posix)

(define (uname)

  ;; probably useful outside this fun.
  (define (get-string s*)
    (let ((s0* (%c-aref char s* 0))
          (slen (posix/strlen s0*)))
      (%c-sfromc #f s0* slen)))

  (let ((utsname* (malloc (struct utsname)))
        (_ (syscall (posix/uname utsname*)))
        (result
         {sysname  = (get-string (%c-sref utsname.sysname utsname*))
          nodename = (get-string (%c-sref utsname.nodename utsname*))
          release  = (get-string (%c-sref utsname.release utsname*))
          version  = (get-string (%c-sref utsname.version utsname*))
          machine  = (get-string (%c-sref utsname.machine utsname*))}))
    (free utsname*)
    result
    ))
