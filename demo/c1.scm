;; -*- Mode: Irken -*-

;; ok, how about some socket action?

(include "lib/basis.scm")

(cinclude "sys/socket.h")
(cinclude "arpa/inet.h")

(define PF_INET		(%%cexp int "PF_INET"))
(define SOCK_STREAM	(%%cexp int "SOCK_STREAM"))
(define AF_INET         (%%cexp int "AF_INET"))

(define (socket family type protocol)
  (%%cexp (int int int -> int)
	  "socket (%0, %1, %2)"
	  family type protocol))

(define (ascii2addr af ascii buf)
  (%%cexp (int string (buffer (struct sockaddr_in)) -> int)
	  "ascii2addr (%0, %1, &(%2->sin_addr))"
	  af ascii buf))

(define (addr2ascii af buf)
  (let ((r (%%cexp (int (buffer (struct sockaddr_in)) -> cstring)
		   "addr2ascii (%0, &(%1->sin_addr), sizeof(struct in_addr), 0)"
		   af buf)))
    (copy-cstring r)))

(define (make-in-addr ip port)
  (let ((ss (%callocate (struct sockaddr_in) 1)))
    (%%cexp ((buffer (struct sockaddr_in)) -> undefined) "(%0->sin_family = PF_INET, PXLL_UNDEFINED)" ss)
    (%%cexp ((buffer (struct sockaddr_in)) int -> undefined) "(%0->sin_port = htons(%1), PXLL_UNDEFINED)" ss port)
    (trysys (ascii2addr AF_INET ip ss))
    ss))

(define (bind fd addr)
  (%%cexp (int (buffer (struct sockaddr_in)) -> int)
	  "bind (%0, (struct sockaddr *) %1, sizeof(struct sockaddr_in))"
	  fd addr))

(define (listen fd backlog)
  (%%cexp (int int -> int) "listen (%0, %1)" fd backlog))

(define (accept fd)
  (let ((sockaddr (%callocate (struct sockaddr_in) 1))
	(address-len (%callocate socklen_t 1)))
    (%%cexp ((buffer socklen_t) -> undefined) "(*%0 = sizeof(struct sockaddr_in), PXLL_UNDEFINED)" address-len)
    (%%cexp (int (buffer (struct sockaddr_in)) (buffer socklen_t) -> int)
	    "accept (%0, (struct sockaddr *) %1, %2)"
	    fd sockaddr address-len)))

(cinclude "sys/errno.h")

(define (trysys retval)
  (if (< retval 0)
      (error1 "system error" (copy-cstring (%%cexp (-> cstring) "strerror(errno)" )))
      retval))

(let ((s (socket PF_INET SOCK_STREAM 0))
      (ss (%callocate (struct sockaddr_in) 1))
      (addrlen (ascii2addr AF_INET "16.1.0.2" ss))
      (in-addr (make-in-addr "127.0.0.1" 8888))
      )
  (print-string (format "s = " (int s) "\n"))
  (printn addrlen)
  (if (> addrlen 0)
      (printn (addr2ascii AF_INET ss))
      (print-string "ascii2addr failed\n"))
  (trysys (bind s in-addr))
  (trysys (listen s 5))
  (let ((fd (trysys (accept s))))
    (printn (write fd "testing, testing!\r\n"))
    (close fd)
    (close s)))

