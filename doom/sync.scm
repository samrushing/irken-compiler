;; -*- Mode: Irken -*-

;; synchronization primitives go here.

(define (mutex/make)
  (let ((held-by -1)
        (fifo (queue/make)))
    (define (acquire*)
      (let ((k (getcc)))
        (queue/add! fifo (poller/make-save k))
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
