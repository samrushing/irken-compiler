;; -*- Mode: Irken -*-

;; synchronization primitives go here.

(define (mutex/make)
  (let ((held-by -1)
        (fifo (queue/make)))
    (define (acquire*)
      (let ((k (getcc)))
        (queue/push! fifo (poller/make-save k))
        (poller/dispatch)))
    (define (acquire)
      (when (>= held-by 0)
        ;; (debugf "mutex held by " (int held-by) " , waiting...\n")
        (acquire*))
      ;; (debugf "acquire by " (int *thread-id*) "\n")
      (set! held-by *thread-id*))
    (define (release)
      ;; (debugf "release by " (int *thread-id*) "\n")
      (cond ((= held-by *thread-id*)
             (set! held-by -1)
             (when-maybe save (queue/pop! fifo)
               ;; (debugf "scheduling...\n")
               (poller/schedule save)))
            (else
             (raise (:Mutex/BadRelease held-by *thread-id*)))))
    {acquire=acquire release=release}
    ))

(defmacro with-mutex
  (with-mutex m body ...)
  -> (begin
       (m.acquire)
       (try
        (begin body ... (m.release))
        except
        EXN
        -> (begin
             (m.release)
             (raise EXN)))))

;; first pass, probably needs exception support
(define (cv/make)
  (let ((fifo (queue/make)))

    (define (wait)
      (let ((k (getcc)))
        (queue/push! fifo (poller/make-save k))
        (poller/dispatch)))

    (define (wake-one)
      (when-maybe save (queue/pop! fifo)
        (poller/schedule save)))

    (define (wake-all)
      (while-maybe save (queue/pop! fifo)
	(poller/schedule save)))

    { wait = wait
      wake-one = wake-one
      wake-all = wake-all
      }
    ))

(define (semaphore/make size)
  (let ((avail size)
        (cv (cv/make)))

    (define (acquire)
      (while (= avail 0)
        (cv.wait))
      (dec! avail 1))
    
    (define (release)
      (inc! avail 1)
      (cv.wake-one))

    {acquire = acquire release = release}
    ))
      
      
    
