;; -*- Mode: Irken -*-

;; we have two different styles of wait/retry:
;; 1) normal: try the syscall, if EWOULDBLOCK then retry it.
;; 2) connect: try the syscall, if EWOULDBLOCK then wait-for-write.

;; we may need a loop anyway because of EINTR - though signals
;; *should* be redirected when using kqueue.

(defmacro loop-nb-retry
  (loop-nb-retry s waitfun expression)
  -> (let $nbloop ()
       (try
        expression
        except
        (:OSError e)
        -> (cond ((= e EWOULDBLOCK)
                  (waitfun s.sock.fd)
                  ($nbloop))
                 (else
                  (raise (:OSError e))))
        )))

(defmacro loop-nb-read
  (loop-nb-read s exp)
  -> (loop-nb-retry s poller/wait-for-read exp))

(defmacro loop-nb-write
  (loop-nb-write s exp)
  -> (loop-nb-retry s poller/wait-for-write exp))

;; socket with buffers attached.
(define (doom/make* sock isize osize)
  (sock/set-nonblocking sock)
  {sock=sock ibuf=(buffer/make isize) obuf=(buffer/make osize)}
  )

;; create a doom socket with attached buffers.
(defmacro doom/make
  (doom/make sock isize osize)
  -> (doom/make* sock isize osize)
  (doom/make sock)
  -> (doom/make* sock 8192 8192))

(define (doom/listen s n)
  (sock/listen s.sock n))

(define (doom/bind s addr)
  (sock/bind s.sock addr))

(define (doom/accept s)
  (let (((sock addr) (loop-nb-read s (sock/accept s.sock))))
    (:tuple (doom/make sock) addr)))

;; non-blocking connect() is managed differently: if connect()
;; returns EINPROGRESS, then we just wait for write, there's no
;; need to call connect() again.
(define (doom/connect s addr)
  (try
   (sock/connect s.sock addr)
   except
   (:OSError e)
   -> (cond ((= e EINPROGRESS)
             (poller/wait-for-write s.sock.fd)
             0)
            (else
             (raise (:OSError e))))
   ))

(define (doom/recv s)
  (loop-nb-read s (sock/recv s.sock s.ibuf))
  (let ((r (buffer/contents s.ibuf)))
    (buffer/reset! s.ibuf)
    r))

(define (doom/recv-exact s n)
  (if (< n (- s.ibuf.end s.ibuf.pos))
      (buffer/get! s.ibuf n)
      (let ((have (- s.ibuf.end s.ibuf.pos))
            (left (- n have))
            (parts (LIST (buffer/get! s.ibuf have))))
        (while (> left 0)
          (let ((recvd (loop-nb-read s (sock/recv s.sock s.ibuf))))
            (when (= recvd 0)
              (raise (:Doom/EOF s)))
            (PUSH parts (buffer/get! s.ibuf (min left recvd)))
            (dec! left (min left recvd))
            ))
        (string-concat (reverse parts)))))

(define (doom/send s data)
  (let ((left (string-length data)))
    (buffer/reset! s.obuf)
    ;; this is cheating, we should loop here instead
    ;;  of just making the buffer larger.
    (buffer/add! s.obuf data)
    (while (> left 0)
      (let ((sent (loop-nb-write s (sock/send s.sock s.obuf))))
        (inc! s.obuf.pos sent)
        (dec! left sent)
        ))
    ))

(define (doom/close s)
  (sock/close s.sock))
