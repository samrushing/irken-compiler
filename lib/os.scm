;; -*- Mode: Irken -*-

;; portable OS functions: these need only libc (whose interface is
;;  the same on every platform), so the compiler can use them.  see
;;  lib/dir.scm for listdir, which needs posix.

(require-ffi 'libc)

(define (system cmd)
  (syscall (libc/system (%string->cref #f (zero-terminate cmd)))))

(define (abort)
  (libc/abort))

(define (tmpnam)
  ;; (L_tmpnam varies by platform, but is never more than PATH_MAX)
  (let ((buf* (malloc char 4096))
        (r* (libc/tmpnam (%c-aref char buf* 0))))
    (if (not (cref-null? r*))
        (let ((r (get-cstring buf*)))
          (free buf*)
          r)
        (begin
          (free buf*)
          (raise-system-error)))))

;; the OS we're running on, e.g. "Linux", "Darwin", "FreeBSD".  The VM
;;  knows; a native program always runs on the platform it was compiled
;;  for, so it's decided at compile time.  (not posix uname(): see
;;  lib/base.scm.)
(define (platform-name)
  (%backend bytecode (%%vminsn (-> string) 'plat))
  (%backend (c llvm arm amd rv)
    (%%platform
     Darwin  "Darwin"
     Linux   "Linux"
     FreeBSD "FreeBSD"
     OpenBSD "OpenBSD"
     NetBSD  "NetBSD"
     else    "Unknown")))

(define (getenv name)
  (let ((val* (libc/getenv (%string->cref #f (zero-terminate name)))))
    (if (cref-null? val*)
        ""
        (%cref->string #f val* (libc/strlen val*))
        )))

(define (getenv-or var default)
  (let ((val (getenv var)))
    (if (= 0 (string-length val))
        default
        val)))

;; remove(3) is unlink(2) for files, and is in C's stdio.
(define (unlink name)
  (syscall (libc/remove (%string->cref #f (zero-terminate name)))))

(define path/join
  "" ""	-> ""
  "" x	-> x
  x  ""	-> x
  a  b
  -> (match (string-ref a (- (string-length a) 1)) (string-ref b 0) with
       #\/ #\/ -> (string-append a (substring b 1 (string-length b)))
       #\/  _  -> (string-append a b)
       _   #\/ -> (string-append a b)
       _    _  -> (format a "/" b)
       ))

(defmacro path/make
  (path/make a)         -> a
  (path/make a b)       -> (path/join a b)
  (path/make a b c ...) -> (path/join a (path/make b c ...))
  )
