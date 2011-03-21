;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(cinclude "sys/types.h")
(cinclude "sys/event.h")

;; better than trying to muck about with the variadic (and evil) fcntl.
(cverbatim "
void
set_nonblocking (int fd)
{
  int flag;
  flag = fcntl (fd, F_GETFL, 0);
  flag |= (O_NDELAY);
  fcntl (fd, F_SETFL, flag);
}
")

(define (kqueue)
  (%%cexp (-> int) "kqueue()"))

;; filters
(define EVFILT_READ     (%%cexp int "EVFILT_READ"))
(define EVFILT_WRITE    (%%cexp int "EVFILT_WRITE"))
(define EVFILT_AIO	(%%cexp int "EVFILT_AIO"))
(define EVFILT_VNODE	(%%cexp int "EVFILT_VNODE"))
(define EVFILT_PROC	(%%cexp int "EVFILT_PROC"))
(define EVFILT_SIGNAL	(%%cexp int "EVFILT_SIGNAL"))
(define EVFILT_MACHPORT (%%cexp int "EVFILT_MACHPORT"))
(define EVFILT_TIMER	(%%cexp int "EVFILT_TIMER"))
(define EVFILT_SESSION	(%%cexp int "EVFILT_SESSION"))

;; flags
(define EV_ADD		(%%cexp int "EV_ADD"))
(define EV_ENABLE	(%%cexp int "EV_ENABLE"))
(define EV_DISABLE	(%%cexp int "EV_DISABLE"))
(define EV_DELETE	(%%cexp int "EV_DELETE"))
(define EV_RECEIPT	(%%cexp int "EV_RECEIPT"))
(define EV_ONESHOT	(%%cexp int "EV_ONESHOT"))
(define EV_CLEAR	(%%cexp int "EV_CLEAR"))
(define EV_EOF		(%%cexp int "EV_EOF"))
(define EV_ERROR	(%%cexp int "EV_ERROR"))

(define (make-changelist n)
  {size=n
   index=0
   buffer = (%callocate (struct kevent) n)
   })

(define (add-kevent changes ident filter flags) ;; fflags data udata
  (if (< changes.index changes.size)
      (begin
	(%%cexp ((buffer (struct kevent)) int int int int -> undefined)
		"EV_SET (%0+%1, %2, %3, %4, 0, 0, 0)"
		changes.buffer changes.index ident filter flags)
	(set! changes.index (+ 1 changes.index)))
      (error1 "changes overflowed" changes.index)))

(define (kevent kqfd changes-in changes-out)
  (%%cexp (int (buffer (struct kevent)) int (buffer (struct kevent)) int -> int)
	  "kevent (%0, %1, %2, %3, %4, NULL)"
	  kqfd
	  changes-in.buffer changes-in.index
	  changes-out.buffer changes-out.size
	  ))

(cinclude "sys/types.h")
(cinclude "sys/socket.h")
(cinclude "netinet/in.h")
(cinclude "arpa/inet.h")

(define SOCK_STREAM	(%%cexp int "SOCK_STREAM"))
(define AF_INET         (%%cexp int "AF_INET"))

(define (socket family type protocol)
  (%%cexp (int int int -> int)
	  "socket (%0, %1, %2)"
	  family type protocol))

(define (set-nonblocking fd)
  (%%cexp (int -> undefined) "set_nonblocking (%0)" fd))

(define (inet_pton af ascii buf)
  (%%cexp (int string (buffer (struct sockaddr_in)) -> int)
	  "inet_pton (%0, %1, &(%2->sin_addr))"
	  af ascii buf))

(define (inet_ntop af buf)
  (let ((ascii (make-string 100))
	(r (%%cexp (int (buffer (struct sockaddr_in)) string int -> int)
		   "inet_ntop (%0, &(%1->sin_addr), %2, %3)"
		   af buf ascii (string-length ascii))))
    ;; should strip this to NUL
    ascii))

(define (make-in-addr ip port)
  (let ((ss (%callocate (struct sockaddr_in) 1)))
    (%%cexp ((buffer (struct sockaddr_in)) -> undefined) "(%0->sin_family = PF_INET, PXLL_UNDEFINED)" ss)
    (%%cexp ((buffer (struct sockaddr_in)) int -> undefined) "(%0->sin_port = htons(%1), PXLL_UNDEFINED)" ss port)
    (trysys (inet_pton AF_INET ip ss))
    ss))

(define (bind fd addr)
  (%%cexp (int (buffer (struct sockaddr_in)) -> int)
	  "bind (%0, (struct sockaddr *) %1, sizeof(struct sockaddr_in))"
	  fd addr))

(define (listen fd backlog)
  (%%cexp (int int -> int) "listen (%0, %1)" fd backlog))

(define (accept fd)
  (let ((sockaddr (%callocate (struct sockaddr_in) 1))
	(address-len (%callocate socklen_t 1)))
    (%%cexp ((buffer socklen_t) -> undefined) "*%0 = sizeof(struct sockaddr_in)" address-len)
    (%%cexp (int (buffer (struct sockaddr_in)) (buffer socklen_t) -> int)
	    "accept (%0, (struct sockaddr *) %1, %2)"
	    fd sockaddr address-len)))

(define (connect fd addr)
  (%%cexp (int (buffer (struct sockaddr_in)) -> int)
	  "connect (%0, (struct sockaddr *) %1, sizeof (struct sockaddr_in))"
	  fd addr))

(define EAGAIN (%%cexp int "EAGAIN"))

;; XXX this is in progress XXX
;; ok, to get further we'll need the basic coro stuff working:
;;  scheduler, suspend/resume, wait-on-kevent, etc...

;; (define (nb-read fd size)
;;   (let ((buffer (make-string size))
;; 	(r (%%cexp (int string int -> int) "read (%0, %1, %2)" fd buffer size)))
;;     (cond ((= r EAGAIN) do-nonblocking-stuff-here)
;; 	  ((< r 0) (error "read() failed"))
;; 	  ((= r size) buffer)
;; 	  (else (copy-string buffer r)))))

(cinclude "sys/errno.h")

(define (trysys retval)
  (if (< retval 0)
      (error1 "system error" (copy-cstring (%%cexp (-> cstring) "strerror(errno)" )))
      retval))

(let ((kqfd (kqueue))
      (cl-in (make-changelist 10))
      (cl-out (make-changelist 10))
      ;;(sfd (socket AF_INET SOCK_STREAM 0))
      ;;(addr (make-in-addr "72.52.84.226" 80))
      )
  ;;(printn (connect sfd addr))
  ;;(set-nonblocking sfd)
  ;;(printn (read sfd 256))
  ;;(printn (write sfd "Howdy, Dude!\r\n"))
  (print-string "hit return, sucka!\n")
  (add-kevent cl-in STDIN_FILENO EVFILT_READ EV_ADD)
  (kevent kqfd cl-in cl-out)
  )
