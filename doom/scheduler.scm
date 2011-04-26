;; -*- Mode: Irken -*-

;; Consider making the poller a top-level global, which would really simplify the API.

(define (make-poller)
  { kqfd	= (kqueue)
    runnable	= (queue/make)
    nwait	= 0 ;; how many events are waiting?
    filters	= (make-vector EVFILT_SYSCOUNT (tree/empty))
    ievents	= (make-changelist 1000)
    oevents	= (make-changelist 1000)
    })

(define the-poller (make-poller))

(define (poller/enqueue k)
  (queue/add the-poller.runnable k))

(define (poller/fork f)
  (poller/enqueue (getcc))
  (f)
  (poller/dispatch))

(define (poller/yield)
  (poller/enqueue (getcc))
  (poller/dispatch))

(define (poller/dispatch)
  (match (queue/pop the-poller.runnable) with
    (maybe:yes k) -> (putcc k #u)
    (maybe:no)	  -> (poller/wait-and-schedule)))

;; these funs know that EVFILT values are consecutive small negative ints
(define (poller/lookup-event ident filter)
  (tree/member the-poller.filters[(- 0 filter)] < ident))

(define (poller/add-event ident filter k)
  (set! the-poller.nwait (+ 1 the-poller.nwait))
  (set! the-poller.filters[(- 0 filter)]
	(tree/insert the-poller.filters[(- 0 filter)]
		     < ident k)))

(define (poller/delete-event ident filter)
  (set! the-poller.filters[(- 0 filter)]
	(tree/delete the-poller.filters[(- 0 filter)] ident < =))
  (set! the-poller.nwait (- the-poller.nwait 1)))

(define (poller/wait-for ident filter)
  (let ((k (getcc)))
    (match (poller/lookup-event ident filter) with
      (maybe:no)
      -> (begin
	   (add-kevent the-poller.ievents ident filter EV_ADDONE)
	   (poller/add-event ident filter k)
	   (poller/dispatch)
	   #u
	   )
      (maybe:yes _) -> (error "poller/wait-for: event already present")
      )))

(define (poller/wait-for-read fd)
  (poller/wait-for fd EVFILT_READ))

(define (poller/wait-for-write fd)
  (poller/wait-for fd EVFILT_WRITE))

(define poller/enqueue-waiting-thread
  (:kev ident filter)
  -> (match (poller/lookup-event ident filter) with
	 (maybe:yes k) -> (begin
			    (poller/delete-event ident filter)
			    (poller/enqueue k))
	 (maybe:no)    -> (error "poller/get-waiting-thread: no thread")))

(define (poller/wait-and-schedule)
  ;; all the runnable threads have done their bit, now
  ;; throw it to kevent().
  (if (= the-poller.nwait 0)
      (print-string "no events, will wait forever!\n"))
  (let ((n (kevent the-poller.kqfd the-poller.ievents the-poller.oevents)))
    (if (< n 0)
	(error "kevent() failed")
	(begin
	  (print-string (format "poller/wait-and-schedule: got " (int n) " events\n"))
	  (set! the-poller.ievents.index 0)
	  (for-range
	      i n
	      (poller/enqueue-waiting-thread
	       (get-kevent the-poller.oevents i)))
	  (poller/dispatch)
	  ))))

