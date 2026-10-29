;; -*- Mode: Irken -*-

;; the Linux poller, on epoll(7).  doom/scheduler.scm uses this in place
;;  of the kqueue code on Linux.
;;
;; kqueue registers (fd, filter) pairs, so a read and a write wait on
;;  the same fd are independent.  epoll registers each fd once, with a
;;  mask of the events wanted, so we keep the readers and writers
;;  separately and compute the mask from them.  every registration is
;;  EPOLLONESHOT (like EV_ONESHOT): after an event fires the fd is
;;  disabled until we re-arm it, which we do if a thread is still
;;  waiting in the other direction.

(require-ffi 'epoll)

(define (make-poller)
  { epfd     = (syscall (epoll/epoll_create1 EPOLL_CLOEXEC))
    runnable = (queue/make)
    nwait    = 0
    readers  = (tree/empty) ;; fd -> save
    writers  = (tree/empty) ;; fd -> save
    added    = (tree/empty) ;; fds currently registered with epoll
    nevents  = 1000
    events   = (malloc (struct epoll_event) 1000)
    evbuf    = (malloc (struct epoll_event))
    })

(define the-poller (make-poller))

(define (epoll/waiting? table fd)
  (match (tree/member table int-cmp fd) with
    (maybe:yes _) -> #t
    (maybe:no)    -> #f))

;; the events we want for <fd>, given who's waiting on it.
(define (epoll/mask fd)
  (logior* EPOLLONESHOT
           (if (epoll/waiting? the-poller.readers fd) EPOLLIN 0)
           (if (epoll/waiting? the-poller.writers fd) EPOLLOUT 0)))

(define (epoll/ctl op fd mask)
  (let ((ev* the-poller.evbuf))
    (c-set-int (%c-sref epoll_event.events ev*) mask)
    (c-set-int (%c-sref epoll_event.data.fd ev*) fd)
    (epoll/epoll_ctl the-poller.epfd op fd ev*)))

;; (re-)register <fd> for whoever is waiting on it.
(define (epoll/arm fd)
  (let ((mask (epoll/mask fd)))
    (if (epoll/waiting? the-poller.added fd)
        (when (< (epoll/ctl EPOLL_CTL_MOD fd mask) 0)
          ;; ENOENT: the fd was closed (which removes it from the epoll
          ;;  set) and the number reused before we heard about it.
          (if (= (get-errno) ENOENT)
              (begin (syscall (epoll/ctl EPOLL_CTL_ADD fd mask)) #u)
              (raise-system-error)))
        (begin
          (when (< (epoll/ctl EPOLL_CTL_ADD fd mask) 0)
            (if (= (get-errno) EEXIST)
                (begin (syscall (epoll/ctl EPOLL_CTL_MOD fd mask)) #u)
                (raise-system-error)))
          (tree/insert! the-poller.added int-cmp fd #t)))))

;; put the current thread to sleep until <fd> is readable (read? = #t)
;;   or writable.
(define (epoll/wait-for fd read?)
  (let ((k (getcc)))
    (if read?
        (when (epoll/waiting? the-poller.readers fd)
          (raise (:PollerEventAlreadyPresent fd 'read)))
        (when (epoll/waiting? the-poller.writers fd)
          (raise (:PollerEventAlreadyPresent fd 'write))))
    (if read?
        (tree/insert! the-poller.readers int-cmp fd (poller/make-save k))
        (tree/insert! the-poller.writers int-cmp fd (poller/make-save k)))
    (inc! the-poller.nwait)
    (epoll/arm fd)
    (poller/dispatch)
    #u))

(define (poller/wait-for-read fd)
  (epoll/wait-for fd #t))

(define (poller/wait-for-write fd)
  (epoll/wait-for fd #f))

;; wake the thread waiting on <fd> in <table>, if any.
(defmacro epoll/wake!
  (epoll/wake! table fd)
  -> (match (tree/member table int-cmp fd) with
       (maybe:yes save)
       -> (begin
            (tree/delete! table int-cmp fd)
            (dec! the-poller.nwait)
            (poller/schedule save))
       (maybe:no) -> #u))

(define (poller/forget fd)
  (when (tree/delete!? the-poller.readers int-cmp fd)
    (dec! the-poller.nwait))
  (when (tree/delete!? the-poller.writers int-cmp fd)
    (dec! the-poller.nwait))
  (when (tree/delete!? the-poller.added int-cmp fd)
    ;; fails harmlessly (ENOENT/EBADF) if the fd is already closed.
    (epoll/ctl EPOLL_CTL_DEL fd 0)
    #u))

(define (epoll/dispatch-event i)
  (let ((ev* (c-aref the-poller.events i))
        (fd (c-get-int (%c-sref epoll_event.data.fd ev*)))
        (events (c-get-int (%c-sref epoll_event.events ev*)))
        ;; errors and hangups wake both sides: their read/write will
        ;;  see the error or EOF.
        (err? (> (logand events (logior EPOLLERR EPOLLHUP)) 0)))
    (when (or err? (> (logand events EPOLLIN) 0))
      (epoll/wake! the-poller.readers fd))
    (when (or err? (> (logand events EPOLLOUT) 0))
      (epoll/wake! the-poller.writers fd))
    ;; oneshot: the fd is now disabled.  re-arm it if a thread is still
    ;;  waiting in the other direction.
    (when (or (epoll/waiting? the-poller.readers fd)
              (epoll/waiting? the-poller.writers fd))
      (epoll/arm fd))))

(define (poller/wait-and-schedule)
  ;; all the runnable threads have done their bit, now throw it to epoll_wait().
  (when (and (eq? the-timeq (tree:empty)) (= the-poller.nwait 0))
    (debugf "no more events, exiting.\n")
    (%exit #f 1))
  (let ((timeout-ns (timeq/time-to-next))
        ;; round up, so a timer that's due in under a millisecond doesn't spin.
        (timeout-ms (/ (+ timeout-ns 999999) 1000000))
        (n (epoll/epoll_wait the-poller.epfd (%c-cast (struct epoll_event) the-poller.events)
                              the-poller.nevents timeout-ms)))
    (cond ((>= n 0)
           (for-range i n
             (epoll/dispatch-event i)))
          ((= (get-errno) EINTR) #u)
          (else (raise-system-error)))
    (timeq/schedule)
    (poller/dispatch)))

(define (poller/dump)
  (printf "poller {\n"
          "  fd: " (int the-poller.epfd) "\n"
          "  [runnable]: " (int the-poller.runnable.len) "\n"
          "  nwait: " (int the-poller.nwait) "\n"
          "  readers: " (join int->string " " (tree/keys the-poller.readers)) "\n"
          "  writers: " (join int->string " " (tree/keys the-poller.writers)) "\n"
          "}\n"))
