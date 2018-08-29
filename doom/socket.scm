;; -*- Mode: Irken -*-

(require "lib/net/socket.scm")
(require "doom/scheduler.scm")

;; we have two different styles of wait/retry:
;; 1) normal: try the syscall, if EWOULDBLOCK then wait & retry it.
;; 2) connect: try the syscall, if EWOULDBLOCK then wait-for-write.

;; we may need a loop anyway because of EINTR - though signals
;; *should* be redirected when using kqueue.

(defmacro loop-nb-retry
  (loop-nb-retry fd waitfun expression)
  -> (let $nbloop ()
       (try
        expression
        except
        (:OSError e)
        -> (cond ((= e EWOULDBLOCK)
                  (waitfun fd)
                  ($nbloop))
                 (else
                  (raise (:OSError e))))
        )))

(defmacro loop-nb-read
  (loop-nb-read fd exp)
  -> (loop-nb-retry fd poller/wait-for-read exp))

(defmacro loop-nb-write
  (loop-nb-write fd exp)
  -> (loop-nb-retry fd poller/wait-for-write exp))

;; create a doom socket with attached buffers.
;; NOTE: when specifying buffer sizes for a listening socket, those
;; are the sizes that will be given to each new connection.

(defmacro doom/make
  (doom/make sock isize osize)
  -> (doom/make* sock isize osize)
  (doom/make sock)
  -> (doom/make* sock 8192 8192))

;; socket with buffers attached.
(define (doom/make* sock isize osize)
  (let ((sock sock)
        (ibuf (buffer/make isize))
        (obuf (buffer/make osize)))

    (define (listen n)
      ;; XXX zilch the two buffers
      (sock/listen sock n))

    (define (bind addr)
      (sock/bind sock addr))

    (define (accept)
      (let (((sock0 addr) (loop-nb-read sock.fd (sock/accept sock))))
        (:tuple (lambda (isize osize)
                  (doom/make sock0 isize osize))
                addr)))

    ;; non-blocking connect() is managed differently: if connect()
    ;; returns EINPROGRESS, then we just wait for write, there's no
    ;; need to call connect() again.
    (define (connect addr)
      (try
       (sock/connect sock addr)
       except
       (:OSError e)
       -> (cond ((= e EINPROGRESS)
                 (poller/wait-for-write sock.fd)
                 0)
                (else
                 (raise (:OSError e))))
       ))

    (define (recv)
      (loop-nb-read sock.fd (sock/recv sock ibuf))
      (let ((r (buffer/contents ibuf)))
        (buffer/reset! ibuf)
        r))

    (define (recv-exact n)
      (if (< n (- ibuf.end ibuf.pos))
          (buffer/get! ibuf n)
          (let ((have (- ibuf.end ibuf.pos))
                (left (- n have))
                (parts (LIST (buffer/get! ibuf have))))
            (while (> left 0)
              (let ((recvd (loop-nb-read sock.fd (sock/recv sock ibuf))))
                (when (= recvd 0)
                  (raise (:Doom/EOF sock)))
                (push! parts (buffer/get! ibuf (min left recvd)))
                (dec! left (min left recvd))
                ))
            (string-concat (reverse parts)))))

    (define (send data)
      (let ((left (string-length data)))
        (buffer/reset! obuf)
        ;; this is cheating, we should loop here instead
        ;;  of just making the buffer larger.
        (buffer/add! obuf data)
        (while (> left 0)
          (let ((sent (loop-nb-write sock.fd (sock/send sock obuf))))
            (inc! obuf.pos sent)
            (dec! left sent)
            ))
        ))

    (define (close)
      (sock/close sock))

    (define (get-fd)
      sock.fd)

    ;; body of doom/make*
    (sock/set-nonblocking sock)

    {listen=listen
     bind=bind
     accept=accept
     connect=connect
     recv=recv
     recv-exact=recv-exact
     send=send
     close=close
     get-fd=get-fd
     sock=sock
     }
    ))
