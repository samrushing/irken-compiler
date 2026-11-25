;; -*- Mode: Irken -*-

;; a recursive DNS resolver: it starts at the root servers and follows
;;  referrals itself, trusting as little as it can.
;;
;; the algorithm is shrapnel's (coro/dns/cache.py), after djbdns:
;;  djb's five kinds of reply (CNAME, NXDOMAIN, an answer, a referral,
;;  NODATA), glue only from within the zone that sent it, lame servers
;;  detected and skipped, and a budget for the work of one resolution.
;;  added since: EDNS0 (rfc6891, 1232 bytes), unknown record types
;;  carried as they are (rfc3597), replies checked for our question as
;;  well as our id, dependency cycles between nameservers detected
;;  (TsuNAME), negative caching (rfc2308), and optionally 0x20 (mixed
;;  case names, checked in the reply).
;;
;; not yet: DNSSEC, IPv6 transport (AAAA glue is ignored), QNAME
;;  minimisation, merging identical queries in flight, remembering
;;  servers that are down, root priming.
;;
;; (dns/resolver config) -> {resolve flush cache-size}
;;   (resolve name qtype) -> {status answers cnames trace}
;;     status: (dns-status:answer), :nodata, :nxdomain, or (:failed why).
;;     answers: the records of type <qtype> (after any CNAMEs), cnames:
;;     the aliases followed, trace: what happened, line by line.
;;
;; a failure (:failed) is temporary: servers didn't answer, or answered
;;  nonsense, or the work ran out.  NXDOMAIN and NODATA are answers.

(require "lib/net/dns.scm")
(require "lib/urandom.scm")
(require "doom/doom.scm")
(require "doom/timeout.scm")

(datatype dns-status
  (:answer)
  (:nodata)
  (:nxdomain)
  (:failed string)
  )

(define dns-status-repr
  (dns-status:answer)     -> "answer"
  (dns-status:nodata)     -> "NODATA"
  (dns-status:nxdomain)   -> "NXDOMAIN"
  (dns-status:failed why) -> (format "failed: " why))

;; what a lookup found (before it becomes a dns-status)
(datatype dns-found
  (:rrs (list {name=string type=dnstype class=int ttl=int data=dnsrr}))
  (:nodata)
  (:nxdomain)
  )

;; what one server's reply means
(datatype dns-step
  (:cname string int)                       ;; target, ttl
  (:nxdomain int)                           ;; negative ttl, or -1
  (:answer (list {name=string type=dnstype class=int ttl=int data=dnsrr}))
  (:referral string (list string) int       ;; zone, its nameservers, ttl,
             (list (:tuple string (list {name=string type=dnstype class=int ttl=int data=dnsrr}))))  ;; and their glue
  (:nodata int)                             ;; negative ttl, or -1
  (:lame string)                            ;; why
  )

;; a cache entry: expiry time (seconds; -1 never), and what's known.
(datatype dns-centry
  (:rrs int (list {name=string type=dnstype class=int ttl=int data=dnsrr}))
  (:nxdomain int)
  (:nodata int)
  )

;; the root servers (https://www.internic.net/domain/named.root,
;;  last updated 2026-11-12).  IPv4 only, for now.
(define dns/root-hints
  (list (:tuple "a.root-servers.net" "198.41.0.4")
        (:tuple "b.root-servers.net" "170.247.170.2")
        (:tuple "c.root-servers.net" "192.33.4.12")
        (:tuple "d.root-servers.net" "199.7.91.13")
        (:tuple "e.root-servers.net" "192.203.230.10")
        (:tuple "f.root-servers.net" "192.5.5.241")
        (:tuple "g.root-servers.net" "192.112.36.4")
        (:tuple "h.root-servers.net" "198.97.190.53")
        (:tuple "i.root-servers.net" "192.36.148.17")
        (:tuple "j.root-servers.net" "192.58.128.30")
        (:tuple "k.root-servers.net" "193.0.14.129")
        (:tuple "l.root-servers.net" "199.7.83.42")
        (:tuple "m.root-servers.net" "202.12.27.33")))

;; settings: make one with dns/config, and change what you like.
;;   hints        the root servers: (:tuple name ip)
;;   timeout      for each attempt at a server, in milliseconds
;;   edns         the UDP size we offer (0: no EDNS)
;;   caps         0x20: mix the case of names, and check the echo
;;   rounds       how many times to go through a zone's servers
;;   max-queries  queries one resolution may send
;;   max-depth    nameservers whose addresses need looking up, one
;;                inside another (glueless delegations)
;;   max-cnames   aliases one resolution may follow
;;   max-ttl      the longest we'll cache anything (seconds)
;;   max-entries  cache size
;;   addr-of      where to send to a server's address: ip -> (:tuple ip port)
;;                (for tests; normally port 53)
(define (dns/config)
  {hints       = dns/root-hints
   timeout     = 2000
   edns        = 1232
   caps        = #f
   rounds      = 2
   max-queries = 100
   max-depth   = 4
   max-cnames  = 8
   max-ttl     = 86400
   max-entries = 10000
   addr-of     = (lambda (ip) (:tuple ip 53))
   })

;; --- names ---

;; lower case, no final dot; the root is "".
(define (dns/normalize name)
  (let ((n (downcase name)))
    (if (and (> (string-length n) 0) (eq? (string-ref n (- (string-length n) 1)) #\.))
        (substring n 0 (- (string-length n) 1))
        n)))

;; is <name> <zone>, or below it?
(define (dns/under? name zone)
  (or (= 0 (string-length zone))
      (string=? name zone)
      (and (> (string-length name) (string-length zone))
           (ends-with name zone)
           (eq? (string-ref name (- (string-length name) (string-length zone) 1)) #\.))))

(define (dns/zone-repr zone)
  (if (= 0 (string-length zone)) "." zone))

(define (dns/ip-of chars)
  (string-join (map int->string (map char->ascii (vector->list chars))) "."))

;; the number in a two-byte field of <s>
(define (dns/u16 s pos)
  (logior (<< (char->int (string-ref s pos)) 8) (char->int (string-ref s (+ pos 1)))))

;; --- the resolver ---

(define (dns/resolver config)

  (let ((cache (tree/empty))
        (centries 0)
        (rng (urandom-make)))

    ;; --- time and randomness ---

    (define (now) (/ (timespec->nanoseconds (gettime/monotonic)) 1000000000))

    (define (random-int n)    ;; 0 <= r < n, n <= 65536
      (let ((b (rng 2)))
        (mod (dns/u16 b 0) n)))

    (define (shuffle l)
      (let ((v (list->vector l))
            (n (vector-length v)))
        (for-range i n
          (let ((j (+ i (random-int (- n i))))
                (t v[i]))
            (set! v[i] v[j])
            (set! v[j] t)))
        (vector->list v)))

    ;; 0x20: each letter's case at random
    (define (mix-case name)
      (let ((bits (rng (+ 1 (string-length name))))
            (r (copy-string name (string-length name))))
        (for-range i (string-length name)
          (let ((c (char->int (string-ref name i))))
            (when (and (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122)))
                       (= 1 (logand 1 (char->int (string-ref bits i)))))
              (string-set! r i (int->char (logxor c 32))))))
        r))

    ;; --- the cache ---

    (define (cache-key name qtype) (format name "/" (int (dnstype->int qtype))))

    (define (cache-get name qtype)
      (match (tree/member cache magic-cmp (cache-key name qtype)) with
        (maybe:no) -> (maybe:no)
        (maybe:yes entry)
        -> (let ((expiry (match entry with
                           (dns-centry:rrs e _) -> e
                           (dns-centry:nxdomain e) -> e
                           (dns-centry:nodata e) -> e)))
             (if (or (< expiry 0) (> expiry (now)))
                 (maybe:yes entry)
                 (maybe:no)))))

    (define (cache-put name qtype entry)
      (when (>= centries config.max-entries)
        (cache-trim))
      (match (tree/member cache magic-cmp (cache-key name qtype)) with
        (maybe:yes _) -> (tree/delete! cache magic-cmp (cache-key name qtype))
        (maybe:no) -> (inc! centries))
      (tree/insert! cache magic-cmp (cache-key name qtype) entry))

    ;; drop what's expired; if that isn't enough, everything but the
    ;;  root hints.  (crude: an LRU would be better.)
    (define (cache-trim)
      (let ((t (now))
            (keep (tree/empty))
            (n 0))
        (define (add k v)
          (tree/insert! keep magic-cmp k v)
          (inc! n))
        (for-map k v cache
          (match v with
            (dns-centry:rrs e _)    -> (when (or (< e 0) (> e t)) (add k v))
            (dns-centry:nxdomain e) -> (when (> e t) (add k v))
            (dns-centry:nodata e)   -> (when (> e t) (add k v))))
        (when (>= n config.max-entries)
          (set! keep (tree/empty))
          (set! n 0)
          (for-map k v cache
            (match v with
              (dns-centry:rrs -1 _) -> (add k v)
              _ -> #u)))
        (set! cache keep)
        (set! centries n)))

    (define (expiry ttl)
      (+ (now) (max 0 (min ttl config.max-ttl))))

    (define (pin-hints)
      (cache-put "" (dnstype:ns)
                 (dns-centry:rrs -1 (map (lambda (h) (match h with
                                                      (:tuple name _)
                                                      -> {name="" type=(dnstype:ns) class=1 ttl=0 data=(dnsrr:ns name)}))
                                         config.hints)))
      (for-list h config.hints
        (match h with
          (:tuple name ip)
          -> (let ((parts (map string->int (string-split ip #\.)))
                   (v (list->vector (map int->char parts))))
               (cache-put (dns/normalize name) (dnstype:a)
                          (dns-centry:rrs -1 (list {name=name type=(dnstype:a) class=1 ttl=0 data=(dnsrr:a v)})))))))

    ;; --- one resolution's work ---

    (define (new-work)
      {queries=0 depth=0 trace=(list:nil) cnames=(list:nil) inflight=(list:nil)})

    (define (note work line)
      (push! work.trace (format (repeat (* 2 work.depth) " ") line)))

    (define (fail work why)
      (note work (format "FAIL: " why))
      (raise (:DNS/Fail why)))

    ;; --- the network ---

    ;; how a reply looks before it's unpacked: is it ours?
    (define (reply-id s) (if (< (string-length s) 12) -1 (dns/u16 s 0)))
    (define (reply-flags s) (dns/u16 s 2))

    ;; one exchange over UDP: send, then read until a reply with our
    ;;  id comes, or the time's up.  replies with the wrong id are noted
    ;;  and skipped (they may be spoofed).
    (define (udp-exchange work ip addr port packet qid)
      (let ((s (doom/make (udp4-sock) 4096 1024))
            (deadline (+ (timespec->nanoseconds (gettime/monotonic)) (* config.timeout 1000000))))
        (try
         (begin
           (s.connect (address/make4 addr port))
           (s.send packet)
           (let loop ()
             (let ((left (/ (- deadline (timespec->nanoseconds (gettime/monotonic))) 1000000)))
               (if (and (> left 0) (poller/wait-for-read-timeout (s.get-fd) left))
                   (let ((r (s.recv)))
                     (if (= (reply-id r) qid)
                         r
                         (begin
                           (note work (format "ignored a reply from " ip " with the wrong id"))
                           (loop))))
                   (raise (:DNS/Soft (format "timeout from " ip)))))))
         finally
         (s.close))))

    ;; read from <s> until we <have> <n> bytes, or the deadline.
    (define (read-n s have n deadline ip)
      (let loop ((have have))
        (if (>= (string-length have) n)
            have
            (let ((left (/ (- deadline (timespec->nanoseconds (gettime/monotonic))) 1000000)))
              (if (and (> left 0) (poller/wait-for-read-timeout (s.get-fd) left))
                  (let ((more (s.recv)))
                    (when (= 0 (string-length more))
                      (raise (:DNS/Soft (format "TCP connection closed by " ip))))
                    (loop (string-append have more)))
                  (raise (:DNS/Soft (format "TCP timeout from " ip))))))))

    (define (tcp-exchange work ip addr port packet)
      (let ((s (doom/make (tcp4-sock) 8192 1024))
            (deadline (+ (timespec->nanoseconds (gettime/monotonic)) (* 2 config.timeout 1000000))))
        (try
         (begin
           (try
            (begin (sock/connect s.sock (address/make4 addr port)) #u)
            except
            (:OSError e)
            -> (if (= e EINPROGRESS)
                   (when (not (poller/wait-for-write-timeout (s.get-fd) config.timeout))
                     (raise (:DNS/Soft (format "TCP connect timeout to " ip))))
                   (raise (:OSError e))))
           (s.send (string-append (tcp-enc-size-prefix (string-length packet)) packet))
           (let ((head (read-n s "" 2 deadline ip))
                 (len (tcp-dec-size-prefix head))
                 ;; (the first read may well have brought it all)
                 (r (read-n s head (+ 2 len) deadline ip)))
             (substring r 2 (+ 2 len))))
         finally
         (s.close))))

    ;; ask <ip> about qname/qtype: the reply, unpacked and checked.
    ;;  raises DNS/Soft if this server can't help.
    (define (ask work ip qname qtype)
      (when (>= work.queries config.max-queries)
        (fail work (format "too much work (" (int config.max-queries) " queries)")))
      (inc! work.queries)
      (match (config.addr-of ip) with
        (:tuple addr port)
        -> (let ((sent-name (if config.caps (mix-case qname) qname)))
             (define (attempt edns tcp?)
               (let ((qid (random-int 65536))
                     (packet (build-query qid sent-name qtype (if tcp? 0 edns)))
                     (raw (if tcp?
                              (tcp-exchange work ip addr port packet)
                              (udp-exchange work ip addr port packet qid))))
                 (cond ((not (= (reply-id raw) qid))
                        (raise (:DNS/Soft (format "TCP reply with the wrong id from " ip))))
                       ((= 0 (logand (reply-flags raw) #x8000))
                        (raise (:DNS/Soft (format "not a reply, from " ip))))
                       ((and (not tcp?) (not (= 0 (logand (reply-flags raw) #x0200))))
                        (note work (format "truncated reply from " ip ": trying TCP"))
                        (attempt edns #t))
                       (else (check-reply raw edns tcp?)))))
             (define (check-reply raw edns tcp?)
               (match (unpack-reply raw) with
                 (:tuple h r)
                 -> (let ((rcode (logior h.rcode (<< (ext-rcode r) 4))))
                      (match r.qd with
                        (q)
                        -> (when (or (not (= q.qtype (dnstype->int qtype)))
                                     (not (= q.qclass dnsclass-in))
                                     (not (if config.caps
                                              (string=? q.name sent-name)
                                              (string=? (downcase q.name) (downcase sent-name)))))
                             (raise (:DNS/Soft (format "a reply to another question, from " ip))))
                        _ -> (raise (:DNS/Soft (format "a reply without our question, from " ip))))
                      (cond ((and (> edns 0) (not tcp?) (member? rcode '(1 4 16) =))
                             ;; FORMERR, NOTIMP or BADVERS to our OPT: once more without
                             (note work (format ip " refused EDNS (rcode " (int rcode) "): trying without"))
                             (attempt 0 #f))
                            ((or (= rcode 0) (= rcode 3)) (:tuple h r))
                            (else
                             (raise (:DNS/Soft (format "rcode " (int rcode) " (" (rcode-name rcode) ") from " ip))))))))
             (try
              (attempt config.edns #f)
              except
              (:OSError e) -> (raise (:DNS/Soft (format "error from " ip ": " (strerror e))))
              (:UnpackUnderflow _) -> (raise (:DNS/Soft (format "a short reply from " ip)))
              (:BadDNSPacket why _) -> (raise (:DNS/Soft (format "a bad reply from " ip ": " why)))
              (:DNSTruncated) -> (raise (:DNS/Soft (format "a truncated reply from " ip " over TCP")))))))

    (define (rcode-name n)
      (match n with
        1 -> "FORMERR" 2 -> "SERVFAIL" 4 -> "NOTIMP" 5 -> "REFUSED" 16 -> "BADVERS"
        _ -> "?"))

    ;; the extended rcode bits, from an OPT record
    (define (ext-rcode r)
      (let loop ((l r.ar))
        (match l with
          () -> 0
          (rr . tl) -> (match rr.type with
                         (dnstype:opt) -> (>> rr.ttl 24)
                         _ -> (loop tl)))))

    ;; --- what a reply means (djb's five cases) ---

    (define (classify work h r qname qtype zone)
      (let ((qt (dnstype->int qtype))
            (answers (list:nil))
            (cname (maybe:no))
            (ref-zone (maybe:no))
            (ref-ns (list:nil))
            (ref-ttl 0)
            (odd-ns #f)
            (soa (maybe:no)))

        (define (neg-ttl)
          (match soa with
            (maybe:yes ttl) -> (min ttl 10800)
            (maybe:no) -> -1))

        ;; the answer section: records for qname, from within this zone
        (for-list rr r.an
          (when (and (= rr.class dnsclass-in) (string=? (downcase rr.name) qname))
            (if (not (dns/under? qname zone))
                (note work (format "ignored an answer for " rr.name " from outside " (dns/zone-repr zone)))
                (match rr.type rr.data with
                  (dnstype:cname) (dnsrr:cname target)
                  -> (set! cname (maybe:yes (:tuple (dns/normalize target) rr.ttl)))
                  _ _
                  -> (when (= (dnstype->int rr.type) qt)
                       (push! answers rr))))))

        ;; the authority section: a referral (NS), or an SOA
        (for-list rr r.ns
          (match rr.type rr.data with
            (dnstype:ns) (dnsrr:ns target)
            -> (let ((owner (dns/normalize rr.name)))
                 (match ref-zone with
                   (maybe:no) -> (begin (set! ref-zone (maybe:yes owner)) (set! ref-ttl rr.ttl) (push! ref-ns (dns/normalize target)))
                   (maybe:yes z) -> (if (string=? z owner)
                                        (begin (push! ref-ns (dns/normalize target)) (set! ref-ttl (min ref-ttl rr.ttl)))
                                        (set! odd-ns #t))))
            (dnstype:soa) (dnsrr:soa _ _ _ _ _ _ minimum)
            -> (let ((owner (dns/normalize rr.name)))
                 (when (and (dns/under? owner zone) (dns/under? qname owner))
                   (set! soa (maybe:yes (min rr.ttl minimum)))))
            _ _ -> #u))

        (match cname answers with
          ;; 1. an alias (unless CNAMEs are what we asked for)
          (maybe:yes (:tuple target ttl)) _
          -> (if (= qt (dnstype->int (dnstype:cname)))
                 (dns-step:answer (filter (lambda (rr) (string=? (downcase rr.name) qname))
                                          (filter (lambda (rr) (match rr.type with (dnstype:cname) -> #t _ -> #f)) r.an)))
                 (dns-step:cname target ttl))
          ;; 3. an answer
          (maybe:no) (_ . _)
          -> (dns-step:answer (reverse answers))
          (maybe:no) ()
          -> (cond ((= h.rcode 3)
                    ;; 2. no such name
                    (dns-step:nxdomain (neg-ttl)))
                   (else (referral-or-nodata work h r qname zone ref-zone ref-ns ref-ttl odd-ns soa (neg-ttl))))
          )))

    ;; 4. a referral (NS records, no SOA), or 5. no records of this type
    (define (referral-or-nodata work h r qname zone ref-zone ref-ns ref-ttl odd-ns soa neg-ttl)
      (match ref-zone soa with
        (maybe:yes ref) (maybe:no)
        -> (cond (odd-ns (dns-step:lame "NS records for more than one zone"))
                 ((or (string=? ref zone) (not (dns/under? ref zone)))
                  (dns-step:lame (format "a referral from " (dns/zone-repr zone) " to " (dns/zone-repr ref))))
                 ((not (dns/under? qname ref))
                  (dns-step:lame (format "a referral to " ref ", which isn't above " qname)))
                 (else
                  (dns-step:referral ref ref-ns ref-ttl (accept-glue work r zone ref-ns))))
        _ _
        -> (if (and (= h.aa 0) (eq? soa (maybe:no)))
               ;; an empty answer that isn't authoritative: this server
               ;;  isn't serving the zone.
               (dns-step:lame (format "an empty, non-authoritative reply for " (dns/zone-repr zone)))
               (dns-step:nodata neg-ttl))))

    ;; glue: addresses for the nameservers of a referral, only if they're
    ;;  within the zone that sent it: (list (:tuple name rrs)).
    (define (accept-glue work r zone ref-ns)
      (let ((glue (tree/empty)))
        (for-list rr r.ar
          (match rr.type rr.data with
            (dnstype:a) (dnsrr:a _)
            -> (let ((name (dns/normalize rr.name)))
                 (cond ((not (member? name ref-ns string=?))
                        (note work (format "ignored an address for " name ": not a nameserver of the referral")))
                       ((not (dns/under? name zone))
                        (note work (format "ignored glue for " name ": outside " (dns/zone-repr zone))))
                       (else
                        (tree/insert! glue magic-cmp name
                                      (list:cons rr (match (tree/member glue magic-cmp name) with
                                                      (maybe:yes l) -> l
                                                      (maybe:no) -> (list:nil)))))))
            _ _ -> #u))
        (let ((result (list:nil)))
          (for-map name rrs glue
            (push! result (:tuple name rrs)))
          result)))

    ;; --- finding servers ---

    ;; the closest zone above <name> whose nameservers we know, and can
    ;;  reach: a zone whose servers are all within it, with their glue
    ;;  gone (expired, or dropped), can only be reached again through
    ;;  its parent's referral.
    (define (best-zone name)
      (define (parent name)
        (let ((dot (string-find "." name)))
          (if (= dot -1) "" (substring name (+ dot 1) (string-length name)))))
      (define (reachable? zone nss)
        (some? (lambda (ns)
                 (or (not (dns/under? ns zone))
                     (match (cache-get ns (dnstype:a)) with
                       (maybe:yes (dns-centry:rrs _ _)) -> #t
                       _ -> #f)))
               nss))
      (let loop ((name name))
        (match (cache-get name (dnstype:ns)) with
          (maybe:yes (dns-centry:rrs _ rrs))
          -> (let ((nss (map (lambda (rr) (match rr.data with (dnsrr:ns n) -> (dns/normalize n) _ -> "")) rrs)))
               (if (or (= 0 (string-length name)) (reachable? name nss))
                   (:tuple name nss)
                   (loop (parent name))))
          _ -> (if (= 0 (string-length name))
                   (raise (:DNS/Fail "no root hints"))
                   (loop (parent name))))))

    (define (ips-of rrs)
      (filter (lambda (ip) (> (string-length ip) 0))
              (map (lambda (rr) (match rr.data with (dnsrr:a v) -> (dns/ip-of v) _ -> "")) rrs)))

    ;; the addresses of nameserver <ns>, looking them up if need be.
    (define (addresses-of work ns)
      (match (cache-get ns (dnstype:a)) with
        (maybe:yes (dns-centry:rrs _ rrs)) -> (ips-of rrs)
        _ -> (let ((key (cache-key ns (dnstype:a))))
               (cond ((member? key work.inflight string=?)
                      (raise (:DNS/Soft (format "a dependency cycle: already looking for " ns))))
                     ((>= work.depth config.max-depth)
                      (raise (:DNS/Soft (format "too many nameservers without glue, one inside another, at " ns))))
                     (else (look-up-nameserver work ns key))))))

    (define (look-up-nameserver work ns key)
      (note work (format "looking up the nameserver " ns))
      (push! work.inflight key)
      (inc! work.depth)
      (try
       (try
        (match (lookup ns (dnstype:a) work) with
          (dns-found:rrs rrs) -> (ips-of rrs)
          _ -> (raise (:DNS/Soft (format "the nameserver " ns " has no address"))))
        except
        (:DNS/Fail why) -> (raise (:DNS/Soft (format "can't find " ns))))
       finally
       (begin
         (dec! work.depth)
         (set! work.inflight (filter (lambda (k) (not (string=? k key))) work.inflight)))))

    ;; --- lookups ---

    (define (lookup qname qtype work)
      (when (not (dns/sane-name? qname))
        (fail work (format "a bad name: " (string qname))))
      (match (cache-get qname qtype) with
        (maybe:yes (dns-centry:rrs _ rrs)) -> (begin (note work (format "cached: " qname " " (dnstype-name qtype))) (dns-found:rrs rrs))
        (maybe:yes (dns-centry:nxdomain _)) -> (begin (note work (format "cached: " qname " NXDOMAIN")) (dns-found:nxdomain))
        (maybe:yes (dns-centry:nodata _)) -> (begin (note work (format "cached: " qname " " (dnstype-name qtype) " NODATA")) (dns-found:nodata))
        (maybe:no) -> (lookup-cname qname qtype work)))

    (define (lookup-cname qname qtype work)
      (match (cache-get qname (dnstype:cname)) with
        (maybe:yes (dns-centry:rrs _ (rr . _)))
        -> (match rr.data with
             (dnsrr:cname target)
             -> (begin
                  (note work (format "cached: " qname " is an alias for " target))
                  (follow-cname qname (dns/normalize target) qtype work))
             _ -> (ask-zone qname qtype work))
        _ -> (ask-zone qname qtype work)))

    (define (follow-cname from target qtype work)
      (when (or (>= (length work.cnames) config.max-cnames)
                (member? target work.cnames string=?)
                (string=? target from))
        (fail work (format "too many aliases, or a loop, at " target)))
      (push! work.cnames target)
      (lookup target qtype work))

    (define (ask-zone qname qtype work)
      (match (best-zone qname) with
        (:tuple zone servers) -> (ask-servers qname qtype zone servers work)))

    ;; ask each of the zone's servers (in random order) until one helps;
    ;;  if none does, go round once more (a packet may have been lost).
    (define (ask-servers qname qtype zone servers work)
      (let loop ((left (shuffle servers))
                 (round 1))
        (match left with
          () -> (if (< round config.rounds)
                    (begin
                      (note work (format "no server for " (dns/zone-repr zone) " answered: trying again"))
                      (loop (shuffle servers) (+ round 1)))
                    (fail work (format "no server for " (dns/zone-repr zone) " gave an answer")))
          (ns . rest)
          -> (match (try-server qname qtype zone ns work) with
               (maybe:yes found) -> found
               (maybe:no) -> (loop rest round)))))

    ;; one nameserver (each of its addresses, until one answers): the
    ;;  result, or (maybe:no) to try the next server.
    (define (try-server qname qtype zone ns work)
      (try
       (try-addresses qname qtype zone ns (addresses-of work ns) work)
       except
       (:DNS/Soft why) -> (begin (note work why) (maybe:no))
       (:DNS/Lame why) -> (begin (note work (format "lame: " ns ": " why)) (maybe:no))))

    (define (try-addresses qname qtype zone ns ips work)
      (match ips with
        () -> (maybe:no)
        (ip . rest)
        -> (begin
             (note work (format "asking " ns " (" ip ") about " qname " " (dnstype-name qtype)
                                " in " (dns/zone-repr zone)))
             (match (try
                     (maybe:yes (ask work ip qname qtype))
                     except
                     (:DNS/Soft why) -> (begin (note work why) (maybe:no))) with
               (maybe:no) -> (try-addresses qname qtype zone ns rest work)
               (maybe:yes (:tuple h r)) -> (maybe:yes (step qname qtype zone h r work))))))

    ;; act on what one server said.
    (define (step qname qtype zone h r work)
      (match (classify work h r qname qtype zone) with
        (dns-step:cname target ttl)
        -> (begin
             (note work (format qname " is an alias for " target))
             (cache-put qname (dnstype:cname)
                        (dns-centry:rrs (expiry ttl) (list {name=qname type=(dnstype:cname) class=1 ttl=ttl data=(dnsrr:cname target)})))
             (follow-cname qname target qtype work))
        (dns-step:nxdomain ttl)
        -> (begin
             (note work (format qname ": NXDOMAIN"))
             (when (> ttl 0) (cache-put qname qtype (dns-centry:nxdomain (expiry ttl))))
             (dns-found:nxdomain))
        (dns-step:answer rrs)
        -> (begin
             (note work (format "answer: " (int (length rrs)) " record" (if (= 1 (length rrs)) "" "s")))
             (cache-put qname qtype (dns-centry:rrs (expiry (fold min config.max-ttl (map (lambda (rr) rr.ttl) rrs))) rrs))
             (dns-found:rrs rrs))
        (dns-step:referral ref nss ttl glue)
        -> (begin
             (note work (format "referral to " ref ": " (join " " nss)))
             (cache-put ref (dnstype:ns)
                        (dns-centry:rrs (expiry ttl) (map (lambda (n) {name=ref type=(dnstype:ns) class=1 ttl=ttl data=(dnsrr:ns n)}) nss)))
             ;; (the glue after the NS records: storing those may have made
             ;;  room by dropping things, and the glue must survive it.)
             (for-list g glue
               (match g with
                 (:tuple name rrs)
                 -> (cache-put name (dnstype:a) (dns-centry:rrs (expiry (fold min config.max-ttl (map (lambda (rr) rr.ttl) rrs))) rrs))))
             (ask-servers qname qtype ref nss work))
        (dns-step:nodata ttl)
        -> (begin
             (note work (format qname ": no " (dnstype-name qtype) " records"))
             (when (> ttl 0) (cache-put qname qtype (dns-centry:nodata (expiry ttl))))
             (dns-found:nodata))
        (dns-step:lame why)
        -> (raise (:DNS/Lame why))))

    ;; --- the public part ---

    (define (resolve name qtype)
      (let ((work (new-work))
            (qname (dns/normalize name))
            (status (try
                     (match (lookup qname qtype work) with
                       (dns-found:rrs rrs) -> (:tuple (dns-status:answer) rrs)
                       (dns-found:nodata)  -> (:tuple (dns-status:nodata) (list:nil))
                       (dns-found:nxdomain) -> (:tuple (dns-status:nxdomain) (list:nil)))
                     except
                     (:DNS/Fail why) -> (:tuple (dns-status:failed why) (list:nil))
                     (:BadDNSName n) -> (:tuple (dns-status:failed (format "a bad name: " (string n))) (list:nil)))))
        (match status with
          (:tuple st rrs)
          -> {status=st answers=rrs cnames=(reverse work.cnames) trace=(reverse work.trace)})))

    (define (flush)
      (set! cache (tree/empty))
      (set! centries 0)
      (pin-hints))

    (pin-hints)
    {resolve=resolve flush=flush cache-size=(lambda () centries)}
    ))

(define (dnstype-name t)
  (match t with
    (dnstype:other n) -> (format "TYPE" (int n))
    _ -> (upcase (let loop ((l dnstype-table))
                   (match l with
                     () -> "?"
                     (item . tl) -> (if (eq? item.t t) item.name (loop tl)))))))
