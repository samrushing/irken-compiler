;; -*- Mode: Irken -*-

;; dns cache for PTR records

(require "lib/net/dns.scm")

;; there are a few problems with this design as-is:
;; we make no attempt to match up requests & replies.
;; if a query is taking too long, we can end up sending
;; lots of duplicate queries, which will end up adding
;; multiple entries to the map if/when the replies come back.
;;
;; so what's needed is an actual request/reply map.

(define (dnscache/make ip size) ;; size currently ignored
  (let ((sock (doom/make (udp4-sock) 1024 1024))
        (addr (address/make4 ip 53))
        (counter 0)
        (map (tree:empty))
        (pending (tree:empty)))
    ;; 'connect' to the udp server.  this simplifies the code, and
    ;;   ensures that we won't receive unwanted packets from nefarious sources.
    (sock.connect addr)

    ;; XXX this needs updating for ipv6 addrs!
    (define (ip->name ip)
      (let ((quad (string-split ip #\.)))
        (format (join "." (reverse quad)) ".in-addr.arpa")))

    (define (next-counter)
      (inc! counter)
      (when (> counter 65530)
        (set! counter 1))
      counter)

    (define (recv-thread)
      (try
       (let ((reply (sock.recv))
             ((header reply) (unpack-reply reply)))
         (match (tree/member pending int-cmp header.qid) with
           (maybe:yes cell)
           -> (begin
                (for-list item reply.an
                  (match item.data with
                    ;; XXX catch CNAME here and loop.
                    (dnsrr:ptr ptrname)
                    -> (set! cell.val ptrname)
                    _
                    -> #u
                    ))
                (tree/delete! pending int-cmp header.qid)
                )
           (maybe:no)
           -> (debugf "reply for non-existent qid = " (int header.qid) "\n")
           ))
      except
      (:DNSTruncated) ;; PTR replies shouldn't be this big, ignore.
      -> (debugf "dns reply truncated for " (string ip) "\n")
      )
      (recv-thread) ;; forever
      )
       
    (define (ask ip)
      (sock.send
       (build-question
        (next-counter)
        (ip->name ip)
        (dnstype:ptr)
        dnsclass-in
        1))
      (let ((cell {ip=ip val=""}))
        (tree/insert! pending int-cmp counter cell)
        (tree/insert! map string-cmp ip cell)))

    (define (get addr)
      (match (string-split addr #\:) with
        (ip port)
        -> (match (tree/member map string-cmp ip) with
             (maybe:no)
             -> (begin (ask ip) "")
             (maybe:yes r)
             -> r.val
             )
        _
        -> ""
        ))

    (poller/fork recv-thread)
    ;; XXX make a record and add a 'clear' method
    get
    ))
