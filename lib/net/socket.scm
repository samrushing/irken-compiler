;; -*- Mode: Irken -*-

(require-ffi 'libc)
(require-ffi 'posix)
(require-ffi 'socket)

;; Note: we need record datatypes for:
;; buffers
;; sockets
;; addresses

;; I wonder if we could use a macro to generate datatype/enum code like this?

(datatype AF
  (:INET)
  (:INET6)
  (:UNIX)
  )

(define AF->int
  (AF:INET)  -> AF_INET
  (AF:INET6) -> AF_INET6
  (AF:UNIX)  -> AF_UNIX
  )

(datatype SOCK
  (:STREAM)
  (:DGRAM)
  (:RAW)
  )

(define SOCK->int
  (SOCK:STREAM) -> SOCK_STREAM
  (SOCK:DGRAM)  -> SOCK_DGRAM
  (SOCK:RAW)    -> SOCK_RAW
  )

;; --------------------------------------------------------------------------
;;                            buffer object
;; --------------------------------------------------------------------------

;; questions:
;;  whether to make recv/send buffers the same.
;;  do we need to copy irken strings into buffer objects??
;;    likely yes: because a GC at any time can move the buffer,
;;      and even if we use an origin+offset we might still need
;;      to worry about non-blocking C functions trying to write
;;      to the older address.  I think sticking with C buffers is
;;      the right thing to do until the last possible moment.

(define (buffer/make size)
  {buf=(malloc char size) size=size pos=0 end=0})

(define (buffer/contents buf)
  (%cref->string #f (%c-aref char buf.buf buf.pos) (- buf.end buf.pos)))

(define (buffer/reset! buf)
  (set! buf.pos 0)
  (set! buf.end 0))

(define (buffer/free buf)
  (free buf.buf))

;; --------------------------------------------------------------------------
;;                            address object
;; --------------------------------------------------------------------------

;; XXX we also need an enum for AF_XXX, because the c-centric temptation
;;  to use AF_INET in a pattern match is VERY confusing. [actually, we just
;;  need a real datatype that is converted to a sockaddr when needed]

;; consider calling this `sockaddr` instead of `address`

(typealias address {addr=(cref (struct sockaddr)) size=int})

(define (inet_pton4 ip addr*) : (string (cref (struct in_addr)) -> int)
  (syscall
   (socket/inet_pton
    AF_INET
    (%string->cref #f (zero-terminate ip))
    (%c-cast (* void) addr*))
    ;;addr*)
   ))

(define (inet_pton6 ip addr*) : (string (cref (struct in6_addr)) -> int)
  (syscall
   (socket/inet_pton
    AF_INET6
    (%string->cref #f (zero-terminate ip))
    (%c-cast (* void) addr*))
   ))

(define (address/make4 ip port)
  (let ((addr* (halloc (struct sockaddr_in))))
    (%c-set-int u8 AF_INET (%c-sref sockaddr_in.sin_family addr*))
    (%c-set-int u16 (socket/htons port) (%c-sref sockaddr_in.sin_port addr*))
    (inet_pton4 ip (%c-sref sockaddr_in.sin_addr addr*))
    {addr=(%c-cast (* (struct sockaddr)) addr*) size=(%c-sizeof (struct sockaddr_in))}
    ))

(define (address/make6 ip port)
  (let ((addr* (halloc (struct sockaddr_in6))))
    (%c-set-int u8 AF_INET6 (%c-sref sockaddr_in6.sin6_family addr*))
    (%c-set-int u16 (socket/htons port) (%c-sref sockaddr_in6.sin6_port addr*))
    (inet_pton6 ip (%c-cast (* void) (%c-sref sockaddr_in6.sin6_addr addr*)))
    {addr=(%c-cast (* (struct sockaddr)) addr*) size=(%c-sizeof (struct sockaddr_in6))}
    ))

(define (address/make)
  {addr=(halloc (struct sockaddr)) size=(%c-sizeof (struct sockaddr))}
  )

(define (unparse-ipv4-address addr*)
  (let ((r0 (halloc char 16))
        (r1 (%c-aref char r0 0))
        (sa* (%c-cast (struct sockaddr_in) addr*))
        (a* (%c-sref sockaddr_in.sin_addr sa*))
        (port (socket/htons (%c-get-int u16 (%c-sref sockaddr_in.sin_port sa*)))))
    (socket/inet_ntop AF_INET (%c-cast (* void) a*) r1 16)
    (:tuple (%cref->string #f r1 (libc/strlen r1)) port)))

(define (unparse-ipv6-address addr*)
  (let ((r0 (halloc char 80))
        (r1 (%c-aref char r0 0))
        (sa* (%c-cast (struct sockaddr_in6) addr*))
        (a* (%c-sref sockaddr_in6.sin6_addr sa*))
        (port (socket/htons (%c-get-int u16 (%c-sref sockaddr_in6.sin6_port sa*)))))
    (socket/inet_ntop AF_INET6 (%c-cast (* void) a*) r1 80)
    (:tuple (%cref->string #f r1 (libc/strlen r1)) port)))

(define (unparse-address addr*)
  (let ((family (%c-get-int u8 (%c-sref sockaddr.sa_family addr*))))
    (match (cond ((eq? family AF_INET) (unparse-ipv4-address addr*))
                 ((eq? family AF_INET6) (unparse-ipv6-address addr*))
                 (else (raise (:UnknownAddressFamily family))))
      with
      (:tuple ip port)
      -> (if (eq? family AF_INET6)
             (format "[" ip "]:" (int port))
             (format ip ":" (int port))))))

(define (address/unparse addr)
  (unparse-address addr.addr))

;; --------------------------------------------------------------------------
;;                           socket interface
;; --------------------------------------------------------------------------

(typealias sock {fd=int ofd=int fam=AF type=SOCK})

(define (sock/make fam type) : (AF SOCK -> sock)
  (let ((fd (syscall (socket/socket (AF->int fam) (SOCK->int type) 0))))
    {fd=fd fam=fam type=type ofd=fd}
    ))

(define (sock/fromfd fd fam type)
  {fd=fd fam=fam type=type ofd=fd}
  )

(define (tcp4-sock) (sock/make (AF:INET) (SOCK:STREAM)))
(define (udp4-sock) (sock/make (AF:INET) (SOCK:DGRAM)))
(define (tcp6-sock) (sock/make (AF:INET6) (SOCK:STREAM)))
(define (udp6-sock) (sock/make (AF:INET6) (SOCK:DGRAM)))

(define (sock/recv sock buf)
  (let ((buf* (%c-aref char buf.buf buf.pos))
        (nbytes (syscall (socket/recv sock.fd (%c-cast (* void) buf*) (- buf.size buf.pos) 0))))
    (set! buf.end (+ buf.end nbytes)) ;; trust recv(2) to not overrun?
    nbytes))

(define (sock/send sock buf)
  (let ((buf* (%c-aref char buf.buf buf.pos))
        (nbytes (syscall (socket/send sock.fd buf* (- buf.end buf.pos) 0))))
    nbytes))

(define (sock/bind sock addr)
  (syscall (socket/bind sock.fd addr.addr addr.size)))

(define (sock/listen sock backlog)
  (syscall (socket/listen sock.fd backlog)))

(define (sock/accept sock)
  (let ((addr (address/make))
        (addrlen (halloc uint)))
    (%c-set-int uint addr.size addrlen)
    (:tuple (sock/fromfd (syscall (socket/accept sock.fd addr.addr addrlen)) sock.fam sock.type)
            addr)))

(define (sock/close sock)
  (posix/close sock.fd)
  (set! sock.fd -1)
  )
