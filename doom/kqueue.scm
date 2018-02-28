;; -*- Mode: Irken -*-

(require-ffi 'kqueue)

(define EV_ADDONE (logior EV_ADD EV_ONESHOT))

(define (make-changelist n)
  {size=n
   index=0
   karray = (malloc (struct kevent) n)
   })

(define (add-kevent changes ident filter flags) ;; fflags data udata
  (when (>= changes.index changes.size)
    (raise (:Kqueue/ChangesOverflow)))
  (let ((kev* (c-aref changes.karray changes.index)))
    (c-set-int (%c-sref kevent.ident kev*) ident)
    (c-set-int (%c-sref kevent.filter kev*) filter)
    (c-set-int (%c-sref kevent.flags kev*) flags)
    (inc! changes.index)))

(define (get-kevent changes i)
  (let ((kev* (c-aref changes.karray i)))
    (:kev
     (c-get-int (%c-sref kevent.ident kev*))
     (c-get-int (%c-sref kevent.filter kev*))
     )))

(define (kevent kqfd changes-in changes-out)
  (kqueue/kevent
   kqfd
   (%c-cast (struct kevent) changes-in.karray)  changes-in.index
   (%c-cast (struct kevent) changes-out.karray) changes-out.size
   NULL ;; timespec, eventually will need a timeout here.
   ))
