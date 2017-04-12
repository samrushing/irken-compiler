;; -*- Mode: Irken -*-

(cinclude "sys/types.h")
(cinclude "sys/socket.h")
(cinclude "netinet/in.h")
(cinclude "arpa/inet.h")

;; better than trying to muck about with the variadic (and evil) fcntl.
(cverbatim "
void
set_nonblocking (int fd)
{
  int flag;
  flag = fcntl (fd, F_GETFL, 0);
  flag |= (O_NDELAY);
  fcntl (fd, F_SETFL, flag);
}
")

(define SOCK_STREAM	(%%cexp int "SOCK_STREAM"))
(define SOCK_DGRAM      (%%cexp int "SOCK_DGRAM"))
(define AF_INET         (%%cexp int "AF_INET"))

;; it'd be nice if we could have these act more like #define
;;   so we could use them in pattern matches
(define EAGAIN		(%%cexp int "EAGAIN"))
(define EINPROGRESS	(%%cexp int "EINPROGRESS"))
(define EWOULDBLOCK	(%%cexp int "EWOULDBLOCK"))

(define (socket family type protocol)
  (let ((fd (syscall
	     (%%cexp (int int int -> int)
		     "socket (%0, %1, %2)"
		     family type protocol))))
    (set-nonblocking fd)
    fd))

(define (set-nonblocking fd)
  (%%cexp (int -> undefined) "set_nonblocking (%0)" fd))

(define (inet_pton af ascii buf)
  (syscall
   (%%cexp (int string (buffer (struct sockaddr_in)) -> int)
	   "inet_pton (%0, %1, &(%2->sin_addr))"
	   af ascii buf)))

(define (inet_ntop af buf)
  (let ((ascii (make-string 100)))
    (%%cexp (int (buffer (struct sockaddr_in)) string int -> int)
		   "inet_ntop (%0, &(%1->sin_addr), %2, %3)"
		   af buf ascii (string-length ascii))
    ;; should strip this to NUL
    ascii))

(define (make-in-addr ip port)
  (let ((ss (%callocate (struct sockaddr_in) 1)))
    (%%cexp ((buffer (struct sockaddr_in)) -> undefined) "%0->sin_family = PF_INET" ss)
    (%%cexp ((buffer (struct sockaddr_in)) int -> undefined) "%0->sin_port = htons(%1)" ss port)
    (inet_pton AF_INET ip ss)
    ss))

(define (bind fd addr)
  (syscall
   (%%cexp (int (buffer (struct sockaddr_in)) -> int)
	   "bind (%0, (struct sockaddr *) %1, sizeof(struct sockaddr_in))"
	   fd addr)))

(define (listen fd backlog)
  (syscall
   (%%cexp (int int -> int) "listen (%0, %1)" fd backlog)))

(define (accept fd)
  (let ((sockaddr (%callocate (struct sockaddr_in) 1))
	(address-len (%callocate socklen_t 1)))
    (%%cexp ((buffer socklen_t) -> undefined) "*%0 = sizeof(struct sockaddr_in)" address-len)
    (let loop ()
      (try
       (syscall
	(%%cexp (int (buffer (struct sockaddr_in)) (buffer socklen_t) -> int)
		"accept (%0, (struct sockaddr *) %1, %2)"
		fd sockaddr address-len))
       except
       (:OSError e) -> (if (eq? e EWOULDBLOCK)
			   (begin (poller/wait-for-read fd) (loop))
			   (raise (:OSError e)))
       ))))

(define (connect fd addr)
  (try
   (syscall
    (%%cexp (int (buffer (struct sockaddr_in)) -> int)
	    "connect (%0, (struct sockaddr *) %1, sizeof (struct sockaddr_in))"
	    fd addr))
   except
   (:OSError e) -> (if (or (eq? e EINPROGRESS)
			   (eq? e EWOULDBLOCK))
		       (begin (poller/wait-for-write fd) 0)
		       (raise (:OSError e)))
   ))

(define (recv-buffer fd buf)
  (let loop ()
    (try
     (syscall
      (%%cexp (int string int -> int)
	      "recv (%0, %1, %2, 0)"
	      fd buf (string-length buf)))
     except
     (:OSError e) -> (if (eq? e EWOULDBLOCK)
			 (begin (poller/wait-for-read fd) (loop))
			 (raise (:OSError e)))
     )))

(define (recv fd size)
  (let ((buffer (make-string size))
	(r (recv-buffer fd buffer)))
    (if (= r size)
	buffer
	(copy-string buffer r))))

(define (recv-exact* fd acc left)
  (let ((size0 (min left 8192))
        (part (recv fd size0))
        (n (string-length part))
        (left0 (- left n)))
    (if (or (= n 0) (= 0 left0))
        (string-concat (reverse (list:cons part acc)))
        (recv-exact* fd (list:cons part acc) left0))))

(define (recv-exact fd size)
  (let ((r (recv-exact* fd '() size)))
    (if (= (string-length r) size)
        r
        (raise (:RecvExactShort size (string-length r))))))

(define (send fd s)
  (let loop ()
    (try
     (syscall
      (%%cexp (int string int -> int)
	      "send (%0, %1, %2, 0)"
	      fd s (string-length s)))
     except
     (:OSError e) -> (if (eq? e EWOULDBLOCK)
			 (begin (poller/wait-for-write fd) (loop))
			 (raise (:OSError e)))
     )))

