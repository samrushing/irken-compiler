;; -*- Mode: Irken -*-

(include "lib/basis.scm")

;; kqueue demo.
;; should work on modern BSD's, including OS X.
;; see the 'doom' subdirectory for followon.

(cinclude "sys/types.h")
(cinclude "sys/event.h")

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
(define EVFILT_SYSCOUNT (%%cexp int "EVFILT_SYSCOUNT"))

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

(define EV_ADDONE       (%%cexp int "EV_ADD|EV_ONESHOT"))

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

(define (get-kevent changes i)
  (:kev
   (%%cexp ((buffer (struct kevent)) int -> int) "%0[%1].ident"  changes.buffer i)
   (%%cexp ((buffer (struct kevent)) int -> int) "%0[%1].filter" changes.buffer i)
   ;; eventually fill in with flags/data/udata
   ))

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

(cinclude "sys/errno.h")

(define (trysys retval)
  (if (< retval 0)
      (error1 "system error" (copy-cstring (%%cexp (-> cstring) "strerror(errno)" )))
      retval))

(define run-queue (queue/make))

(define (enqueue k)
  (queue/add run-queue k))

(define (dispatch)
  (match (queue/pop run-queue) with
    (maybe:yes k) -> (putcc k #u)
    (maybe:no) -> #u))

(define (fork f)
  (enqueue (getcc))
  (f)
  (dispatch))

(define (yield)
  (enqueue (getcc))
  (dispatch))

(define (dispatch-kevent p)
  (match (queue/pop run-queue) with
    (maybe:yes k) -> (putcc k #u)
    (maybe:no)	  -> (poller/wait-and-schedule p)))

(define (make-poller)
  { kqfd    = (kqueue)
    nwait   = 0 ;; how many events are waiting?
    filters = (make-vector EVFILT_SYSCOUNT (tree/empty))
    ievents = (make-changelist 1000)
    oevents = (make-changelist 1000)
    })

;; these funs know that EVFILT values are consecutive small negative ints
(define (poller/lookup-event p ident filter)
  (tree/member p.filters[(- 0 filter)] int-cmp ident))

(define (poller/add-event p ident filter k)
  (set! p.nwait (+ 1 p.nwait))
  (set! p.filters[(- 0 filter)]
	(tree/insert p.filters[(- 0 filter)]
		     int-cmp ident k)))

(define (poller/delete-event p ident filter)
  (set! p.filters[(- 0 filter)]
	(tree/delete p.filters[(- 0 filter)] int-cmp ident))
  (set! p.nwait (- p.nwait 1)))

(define (poller/wait-for p ident filter)
  (let ((k (getcc)))
    (match (poller/lookup-event p ident filter) with
      (maybe:no)
      -> (begin
	   (print-string (format "adding kevent: ident=" (int ident) " filter=" (int filter) "\n"))
	   (add-kevent p.ievents ident filter EV_ADDONE)
	   (poller/add-event p ident filter k)
	   (dispatch-kevent p))
      (maybe:yes _) -> (error "poller/wait-for: event already present")
      )))

(define (poller/wait-for-read p fd)
  (poller/wait-for p fd EVFILT_READ))

(define (poller/wait-for-write p fd)
  (poller/wait-for p fd EVFILT_WRITE))

(define poller/enqueue-waiting-thread
  p (:kev ident filter)
  -> (match (poller/lookup-event p ident filter) with
	 (maybe:yes k) -> (begin
			    (poller/delete-event p ident filter)
			    (enqueue k))
	 (maybe:no)    -> (error "poller/get-waiting-thread: no thread")))

(define (poller/wait-and-schedule p)
  ;; all the runnable threads have done their bit, now
  ;; throw it to kevent().
  (if (= p.nwait 0)
      (print-string "no events, will wait forever!\n"))
  (let ((n (kevent p.kqfd p.ievents p.oevents)))
    (if (< n 0)
	(error "kevent() failed")
	(begin
	  (print-string (format "poller/wait-and-schedule: got " (int n) " events\n"))
	  (set! p.ievents.index 0)
	  (for-range
	      i n
	      (poller/enqueue-waiting-thread
	       p (get-kevent p.oevents i)))
	  (dispatch-kevent p)
	  ))))

(define (fetch-head p ip)
  (let ((sfd (socket AF_INET SOCK_STREAM 0))
	(addr (make-in-addr ip 80)))
    (set-nonblocking sfd)
    (connect sfd addr)
    (poller/wait-for-write p sfd)
    (printn (write sfd "HEAD / HTTP/1.0\r\n\r\n"))
    (print-string "sent request, waiting for read...\n")
    (poller/wait-for-read p sfd)
    (print-string (read sfd 1024))
    (print-string "done!\n")
    (close sfd)
    ))

(let ((p (make-poller))
      (ip "72.52.84.226"))
  (fork (lambda () (fetch-head p ip)))
  (fork (lambda () (fetch-head p ip)))
  (fork (lambda () (fetch-head p ip)))
  (fetch-head p ip)
  )
