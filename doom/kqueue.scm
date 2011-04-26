;; -*- Mode: Irken -*-

(include "lib/basis.scm")

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
