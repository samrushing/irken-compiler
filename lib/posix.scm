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

(make-enum stat-mode
  (pipe    S_IFIFO)
  (char    S_IFCHR)
  (dir     S_IFDIR)
  (block   S_IFBLK)
  (regular S_IFREG)
  (symlink S_IFLNK)
  (socket  S_IFSOCK)
  )

(define (isdir? path)
  (try
   (let ((info (stat path)))
     (not (zero? (logand info.mode S_IFDIR))))
   except (:OSError _)
   -> #f
   ))

(define (isfile? path)
  (try
   (let ((info (stat path)))
     (not (zero? (logand info.mode S_IFREG))))
   except (:OSError _)
   -> #f
   ))


(define (stat path)

  ;; unfortunately some of these slots are not portable between systems.
  ;; i.e. on macos we have this lovely macro:
  ;; #define st_atime st_atimespec.tv_sec
  ;; and on freebsd we have:
  ;; #define st_atime st_atim.tv_sec
  ;; and glibc on linux uses the same st_atim names as freebsd.
  ;; maybe eventually I'll add '%platform to the compiler?
  
  (define (get-atime stat*)
    (c-get-int
     (%%platform
      Darwin
      (%c-sref stat.st_atimespec.tv_sec stat*)
      (FreeBSD Linux)
      (%c-sref stat.st_atim.tv_sec stat*)
      else
      (%c-sref stat.st_atime stat*))
     ))

  (define (get-mtime stat*)
    (c-get-int
     (%%platform
      Darwin
      (%c-sref stat.st_mtimespec.tv_sec stat*)
      (FreeBSD Linux)
      (%c-sref stat.st_mtim.tv_sec stat*)
      else
      (%c-sref stat.st_mtime stat*))
     ))

  (define (get-ctime stat*)
    (c-get-int
     (%%platform
      Darwin
      (%c-sref stat.st_ctimespec.tv_sec stat*)
      (FreeBSD Linux)
      (%c-sref stat.st_ctim.tv_sec stat*)
      else
      (%c-sref stat.st_ctime stat*))
     ))

  (let ((stat* (halloc (struct stat))))
    (syscall (posix/stat (cstring path) stat*))
    { mode	= (c-get-int (%c-sref stat.st_mode stat*))
      ;;ino	= (c-get-int (%c-sref stat.st_in stat*))
      ;;dev	= (c-get-int (%c-sref stat.st_dev stat*))
      nlink	= (c-get-int (%c-sref stat.st_nlink stat*))
      uid	= (c-get-int (%c-sref stat.st_uid stat*))
      gid	= (c-get-int (%c-sref stat.st_gid stat*))
      size	= (c-get-int (%c-sref stat.st_size stat*))
      atime	= (get-atime stat*)
      mtime	= (get-mtime stat*)
      ctime	= (get-ctime stat*)
      }
    ))

(define (signal/ignore signum)
  (%backend bytecode (posix/signal signum SIG_IGN))
  ;; there are horrible c type shenanigans with signal,
  ;;   this seems to mollify the c compiler.
  (%backend (c llvm arm amd rv) (%%ffi signal signum SIG_IGN)))


