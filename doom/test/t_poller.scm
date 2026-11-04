;; -*- Mode: Irken -*-

;; exercise the poller (kqueue or epoll) over loopback TCP:
;;
;; * an echo server, one thread per connection.
;; * several clients, each with a writer thread sending a few MB and a
;;   reader thread reading the echo back, on the same socket.  the
;;   writes outrun the socket buffers, so each client socket ends up
;;   with a thread waiting to read *and* one waiting to write.
;; * a thread sleeping on the timer queue.

(require "lib/basis.scm")
(require "doom/doom.scm")

(define PORT 47330)
(define NCLIENTS 4)
(define NBYTES (* 3 1024 1024))
(define done 0)

(define (pattern i n)
  (let ((s (make-string n)))
    (for-range j n
      (string-set! s j (int->char (+ 97 (mod (+ i j) 26)))))
    s))

(define (server)
  (let ((sock (doom/make (tcp4-sock))))
    (sock.bind (address/make4 "127.0.0.1" PORT))
    (sock.listen 10)
    (let loop ()
      (let (((sockfun _) (sock.accept)))
        (poller/fork (lambda () (echo (sockfun 65536 65536))))
        (loop)))))

(define (echo sock)
  (let loop ()
    (let ((data (sock.recv)))
      (when (> (string-length data) 0)
        (sock.send data)
        (loop))))
  (sock.close))

(define (client i)
  (let ((sock (doom/make (tcp4-sock) 65536 65536))
        (msg (pattern i NBYTES))
        (got '())
        (ngot 0))
    (sock.connect (address/make4 "127.0.0.1" PORT))
    ;; the writer
    (poller/fork (lambda () (sock.send msg)))
    ;; the reader (this thread)
    (while (< ngot NBYTES)
      (let ((data (sock.recv)))
        (when (= 0 (string-length data))
          (raise (:EarlyEOF i ngot)))
        (push! got data)
        (set! ngot (+ ngot (string-length data)))))
    (when (not (string=? msg (string-concat (reverse got))))
      (raise (:Mismatch i)))
    (sock.close)
    (printf "client " (int i) ": " (int ngot) " bytes ok\n")
    (finished)))

;; exit once the clients and the ticker are all done.
(define (finished)
  (inc! done)
  (when (= done (+ NCLIENTS 1))
    (printf "all ok\n")
    (%exit #f 0)))

(define (ticker)
  (for-range i 5
    (timeq/sleep 20))
  (printf "timer ok\n")
  (finished))

(poller/fork server)
(poller/fork ticker)
(for-range i NCLIENTS
  (poller/fork (lambda () (client i))))
(poller/wait-and-schedule)
