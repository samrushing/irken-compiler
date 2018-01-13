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
  (let ((kev* (%c-aref (struct kevent) changes.karray changes.index)))
    (%c-set-int ulong ident (%c-sref kevent.ident kev*))
    (%c-set-int short filter (%c-sref kevent.filter kev*))
    (%c-set-int ushort flags (%c-sref kevent.flags kev*))
    (inc! changes.index)))

(define (get-kevent changes i)
  (let ((kev* (%c-aref (struct kevent) changes.karray i)))
    (:kev
     (%c-get-int ulong (%c-sref kevent.ident kev*))
     (%c-get-int short (%c-sref kevent.filter kev*))
     )))

(define (kevent kqfd changes-in changes-out)
  (kqueue/kevent
   kqfd
   (%c-cast (struct kevent) changes-in.karray)  changes-in.index
   (%c-cast (struct kevent) changes-out.karray) changes-out.size
   NULL ;; timespec, eventually will need a timeout here.
   ))
