;; -*- Mode: Irken -*-

;; will need two CVs eventually.  the obvious one for read,
;;  but if/when we honor window sizes, we will need a CV for write.

(define (make-channel c2s-id s2c-id winsz maxpkt session-handler channel-write trans-send)

  (let ((read-cv (cv/make))
        (write-cv (cv/make))
        (rqueue (queue/make))
        (wqueue (queue/make))
        (env (tree/empty))
        (term-size (:tuple 80 40))
        )

    (define (recv data)
      (queue/push! rqueue data)
      (read-cv.wake-one)
      )

    (define (read)
      (when (= rqueue.len 0)
        (read-cv.wait))
      (let ((pieces (list:nil)))
        (while (> rqueue.len 0)
          (when-maybe piece (queue/pop! rqueue)
            (push! pieces piece)))
        (string-concat (reverse pieces))
        ))

    ;; not yet.
    ;; (define (write data)
    ;;   (queue/push! wqueue data)
    ;;   (write-cv.wake_one))

    (define (write data)
      (channel-write c2s-id data))

    ;; [maybe] this stuff really belongs in transport, but with callbacks into
    ;;  the channel object?  on the other hand, if we want different channel types
    ;;  to implement their own behavior, we might want it this way.  e.g. x11 channel.

    (define (handle-pty-req extra)
      (let ((pkt (ssh-unpack-with-descriptor
                  extra 0
                  (ssh-pkt (TERM s) (w u32) (h u32) (pw u32) (ph u32) (modes s))))
            (TERM (ssh-pkt/get-string pkt 'TERM))
            (w (ssh-pkt/get-u32 pkt 'w))
            (h (ssh-pkt/get-u32 pkt 'h))
            (pw (ssh-pkt/get-u32 pkt 'pw))
            (ph (ssh-pkt/get-u32 pkt 'ph))
            (modes (ssh-pkt/get-string pkt 'modes)))
        (set! term-size (:tuple w h))
        (printf "pty request TERM " (string TERM) " w=" (int w) " h=" (int h) " pixw=" (int pw) " pixh=" (int ph) "\n")
        (send-channel-success)
        ))

    (define (handle-window-change extra)
      (let ((pkt (ssh-unpack-with-descriptor
                  extra 0
                  (ssh-pkt (w u32) (h u32) (pw u32) (ph u32))))
            (w (ssh-pkt/get-u32 pkt 'w))
            (h (ssh-pkt/get-u32 pkt 'h))
            (pw (ssh-pkt/get-u32 pkt 'pw))
            (ph (ssh-pkt/get-u32 pkt 'ph)))
        (set! term-size (:tuple w h))
        (printf "window-change w=" (int w) " h=" (int h) " pixw=" (int pw) " pixh=" (int ph) "\n")
        ))

    (define (get-terminal-size)
      term-size)

    (define (handle-env extra)
      (let ((pkt (ssh-unpack-with-descriptor extra 0 (ssh-pkt (name s) (val s))))
            (name (ssh-pkt/get-string pkt 'name))
            (val (ssh-pkt/get-string pkt 'val)))
        (printf "env " (string name) " = " (string val) "\n")
        (tree/insert! env string-cmp name val)))

    (define (request type want extra)
      (match type with
        "pty-req"       -> (handle-pty-req extra)
        "window-change" -> (handle-window-change extra)
        "env"           -> (handle-env extra)
        "shell"         -> (begin (send-channel-success) (spawn-handler))
        _ -> (printf "ignoring " (string type) " channel request\n")
        )
      )

    (define (close)
      (trans-send (build-channel-close c2s-id)))

    (define (getenv) env)

    (define (send-channel-success)
      (trans-send (build-channel-success c2s-id)))

    (define (send-channel-failure)
      (trans-send (build-channel-failure c2s-id)))

    (define (spawn-handler)
      (let ((handler {read=read
                      write=write
                      close=close
                      getenv=getenv
                      get-size=get-terminal-size}))
        (poller/fork (lambda () (session-handler handler)))))

    { c2s-id = c2s-id
      s2c-id = s2c-id
      winsz = winsz
      maxpkt = maxpkt
      recv = recv
      request = request
      }

    ))

(define (make-channel-manager)
  (let ((map (tree/empty))
        (counter 0))

    (define (next-id)
      (let ((r counter))
        (inc! counter)
        r))

    (define (new c2s-chan winsz maxpkt session-handler write trans-send)
      (let ((chan-id (next-id))
            (chan (make-channel c2s-chan chan-id winsz maxpkt session-handler write trans-send)))
        (tree/insert! map int-cmp chan-id chan)
        chan
        ))

    (define (close c2s-id s2c-id)
      (tree/delete! map int-cmp s2c-id))

    (define (lookup s2c-id)
      (tree/member map int-cmp s2c-id))

    { new = new
      lookup = lookup
      close = close }

    ))
