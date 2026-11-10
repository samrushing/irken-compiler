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
     (c-get-int (%c-sref kevent.flags kev*))
     )))

;; drop any not-yet-submitted changes for fd <ident> (read and write
;;  filters only).  if a closed fd's change stayed in the list, the next
;;  kevent() would report it back as an EV_ERROR (EBADF) event - or, if
;;  the fd number had been reused, register it on someone else's socket.
(define (remove-kevents changes ident)
  (let ((j 0))
    (for-range i changes.index
      (let ((src* (c-aref changes.karray i))
            (id (c-get-int (%c-sref kevent.ident src*)))
            (filter (c-get-int (%c-sref kevent.filter src*)))
            (flags (c-get-int (%c-sref kevent.flags src*))))
        (when (not (and (= id ident)
                        (or (= filter EVFILT_READ) (= filter EVFILT_WRITE))))
          (when (< j i)
            (let ((dst* (c-aref changes.karray j)))
              (c-set-int (%c-sref kevent.ident dst*) id)
              (c-set-int (%c-sref kevent.filter dst*) filter)
              (c-set-int (%c-sref kevent.flags dst*) flags)))
          (inc! j))))
    (set! changes.index j)))

(define (kevent kqfd changes-in changes-out timeout)
  (let ((ts* (halloc (struct timespec)))
        ((sec nsec) (divmod timeout 1000000000)))
    (c-set-int (%c-sref timespec.tv_sec ts*) sec)
    (c-set-int (%c-sref timespec.tv_nsec ts*) nsec)
    (kqueue/kevent
     kqfd
     (%c-cast (struct kevent) changes-in.karray)  changes-in.index
     (%c-cast (struct kevent) changes-out.karray) changes-out.size
     ts*
     )))
