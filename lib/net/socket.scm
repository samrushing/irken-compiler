;; -*- Mode: Irken -*-

(require-ffi 'libc)
(require-ffi 'posix)
(require-ffi 'socket)

(make-enum AF
  (INET  AF_INET)
  (INET6 AF_INET6)
  (UNIX  AF_UNIX))

(make-enum SOCK
  (STREAM SOCK_STREAM)
  (DGRAM  SOCK_DGRAM)
  (RAW    SOCK_RAW)
  )

;; --------------------------------------------------------------------------
;;                            buffer object
;; --------------------------------------------------------------------------

;; buffer has two position markers:
;;   pos: beginning of data
;;   end: end of data
;;
;; these work for both send and recv buffers.
;; to send data, bracket it with (pos,end)
;; to recv data, set pos, then recv, then data is in (pos,end)

(typealias buffer {buf=(cref (array char)) size=int pos=int end=int})

(define (buffer/make size)
  {buf=(malloc char size) size=size pos=0 end=0})

(define (buffer/contents buf)
  (cref->string (c-aref buf.buf buf.pos) (- buf.end buf.pos)))

(define (buffer/ready buf)
  (- buf.end buf.pos))

;; add data to a buffer, adjusting `end`.
(define (buffer/add! buf s)
  (let ((slen (string-length s))
        (newend (+ buf.end slen)))
    (when (< buf.size newend)
      (buffer/grow! buf newend))
    (let ((dst* (c-aref buf.buf buf.pos)))
      (libc/memcpy (%c-cast void dst*)
                   (%c-cast void (%string->cref #f s))
                   slen)
      (inc! buf.end slen))))

;; reads data from a buffer, adjusting `pos`.
(define (buffer/get! buf n)
  (if (< (- buf.end buf.pos) n)
      (raise (:Socket/BufferUnderflow))
      (let ((r (cref->string (c-aref buf.buf buf.pos) n)))
        (inc! buf.pos n)
        (when (= buf.pos buf.end)
          (buffer/reset! buf))
        r)))

(define (buffer/get-all! buf)
  (let ((ready (buffer/ready buf)))
    (if (<= ready 0)
        (raise (:Socket/BufferUnderflow))
        (buffer/get! buf ready))))

(define (buffer/grow! buf newsize)
  (let ((newbuf (%c-cast (array char) (libc/realloc (%c-cast void buf.buf) newsize))))
    (cond ((cref-null? newbuf) (raise (:Socket/MallocFailed)))
          (else
           (set! buf.buf newbuf)
           (set! buf.size newsize)))))

(define (buffer/grow-50%! buf)
  (buffer/grow! buf (* 2 (+ buf.size 1))))

(define (buffer/maybe-grow! buf newsize)
  (if (< buf.size newsize)
      (buffer/grow! buf newsize)
      #u))

(define (buffer/reset! buf)
  (set! buf.pos 0)
  (set! buf.end 0))

(define (buffer/free buf)
  (free buf.buf))

;; --------------------------------------------------------------------------
;;                            address object
;; --------------------------------------------------------------------------

;; consider calling this `sockaddr` instead of `address`

(typealias address {addr=(cref (struct sockaddr)) size=int})

(define (inet_pton4 ip addr*) : (string (cref (struct in_addr)) -> int)
  (syscall
   (socket/inet_pton AF_INET (cstring ip) (%c-cast void addr*))
   ))

(define (inet_pton6 ip addr*) : (string (cref (struct in6_addr)) -> int)
  (syscall
   (socket/inet_pton AF_INET6 (cstring ip) (%c-cast void addr*)
    )))

(define (address/make4 ip port)
  (let ((addr* (halloc (struct sockaddr_in))))
    (c-set-int (%c-sref sockaddr_in.sin_family addr*) AF_INET)
    (c-set-int (%c-sref sockaddr_in.sin_port addr*) (socket/htons port))
    (inet_pton4 ip (%c-sref sockaddr_in.sin_addr addr*))
    {addr=(%c-cast (struct sockaddr) addr*) size=(%c-sizeof (struct sockaddr_in))}
    ))

(define (address/make6 ip port)
  (let ((addr* (halloc (struct sockaddr_in6))))
    (c-set-int (%c-sref sockaddr_in6.sin6_family addr*) AF_INET6)
    (c-set-int (%c-sref sockaddr_in6.sin6_port addr*) (socket/htons port))
    (inet_pton6 ip (%c-sref sockaddr_in6.sin6_addr addr*))
    {addr=(%c-cast (struct sockaddr) addr*) size=(%c-sizeof (struct sockaddr_in6))}
    ))

(define (address/make)
  {addr=(halloc (struct sockaddr)) size=(%c-sizeof (struct sockaddr))}
  )

(define (unparse-ipv4-address addr*)
  (let ((r0 (halloc char 16))
        (r1 (%c-aref char r0 0))
        (sa* (%c-cast (struct sockaddr_in) addr*))
        (a* (%c-sref sockaddr_in.sin_addr sa*))
        (port (socket/htons (c-get-int (%c-sref sockaddr_in.sin_port sa*)))))
    (socket/inet_ntop AF_INET (%c-cast void a*) r1 16)
    (:tuple (cref->string r1 (libc/strlen r1)) port)))

(define (unparse-ipv6-address addr*)
  (let ((r0 (halloc char 80))
        (r1 (%c-aref char r0 0))
        (sa* (%c-cast (struct sockaddr_in6) addr*))
        (a* (%c-sref sockaddr_in6.sin6_addr sa*))
        (port (socket/htons (c-get-int (%c-sref sockaddr_in6.sin6_port sa*)))))
    (socket/inet_ntop AF_INET6 (%c-cast void a*) r1 80)
    (:tuple (cref->string r1 (libc/strlen r1)) port)))

(define (unparse-address addr*)
  (let ((family (c-get-int (%c-sref sockaddr.sa_family addr*))))
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
(define (unix-sock) (sock/make (AF:UNIX) (SOCK:STREAM)))
;; if you need a unix datagram type it out yourself.

(define (sock/set-nonblocking sock)
  (let ((flags (socket/fcntl sock.fd F_GETFL 0)))
    (syscall
     (socket/fcntl sock.fd F_SETFL (logior flags O_NDELAY)))
    ))

(define (sock/set-reuseaddr sock)
  (let ((val* (halloc int)))
    (c-set-int val* 1)
    (syscall
     (socket/setsockopt sock.fd SOL_SOCKET SO_REUSEADDR (%c-cast void val*) (%c-sizeof int)))
    ))

(define (sock/recv sock buf) : (sock buffer -> int)
  (let ((buf* (%c-cast void (c-aref buf.buf buf.pos)))
        (nbytes (syscall (socket/recv sock.fd buf* (- buf.size buf.pos) 0))))
    (set! buf.end (+ buf.end nbytes)) ;; trust recv(2) to not overrun?
    nbytes))

(define (sock/send sock buf) : (sock buffer -> int)
  (let ((buf* (%c-cast void (c-aref buf.buf buf.pos)))
        (nbytes (syscall (socket/send sock.fd buf* (- buf.end buf.pos) 0))))
    nbytes))

(define (sock/bind sock addr)
  (syscall (socket/bind sock.fd addr.addr addr.size)))

(define (sock/listen sock backlog)
  (syscall (socket/listen sock.fd backlog)))

(define (sock/accept sock)
  (let ((addr (address/make))
        (addrlen* (halloc uint)))
    (c-set-int addrlen* addr.size)
    (:tuple (sock/fromfd (syscall (socket/accept sock.fd addr.addr addrlen*))
                         sock.fam sock.type)
            addr)))

(define (sock/connect sock addr)
  (syscall (socket/connect sock.fd addr.addr addr.size)))

(define (sock/close sock)
  (posix/close sock.fd)
  (set! sock.fd -1)
  )

