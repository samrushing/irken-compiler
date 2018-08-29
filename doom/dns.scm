;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/pack.scm")
(require "lib/codecs/base64.scm")
(require "doom/doom.scm")

;; for now, we are only including types, opcodes, and rcodes
;;  that are actually used.

;; I think these can be done with `make-enum`.

(datatype dnstype
  (:a)      ;; a host address                           [RFC1035]
  (:ns)     ;; an authoritative name server             [RFC1035]
  (:cname)  ;; the canonical name for an alias          [RFC1035]
  (:soa)    ;; marks the start of a zone of authority   [RFC1035]
  (:ptr)    ;; a domain name pointer                    [RFC1035]
  (:mx)     ;; mail exchange                            [RFC1035]
  (:txt)    ;; text strings                             [RFC1035]
  (:aaaa)   ;; IP6 Address                              [Thomson]
  (:srv)    ;; Server Selection                         [RFC2782]
  (:dnskey) ;; DNSSEC Key                               [RFC4034]
  (:rrsig)  ;; DNSSEC RRSIG                             [RFC4034]
  (:any)    ;; A request for all records                [RFC1035]
  (:other int) ;; all other types.
  )

(define (DNST t name val) {name=name t=t val=val})

(define dnstype-table
  (LIST
   (DNST  (dnstype:a)      "a"      1)
   (DNST  (dnstype:ns)     "ns"     2)
   (DNST  (dnstype:cname)  "cname"  5)
   (DNST  (dnstype:soa)    "soa"    6)
   (DNST  (dnstype:ptr)    "ptr"    12)
   (DNST  (dnstype:mx)     "mx"     15)
   (DNST  (dnstype:txt)    "txt"    16)
   (DNST  (dnstype:aaaa)   "aaaa"   28)
   (DNST  (dnstype:srv)    "srv"    33)
   (DNST  (dnstype:dnskey) "dnskey" 48)
   (DNST  (dnstype:rrsig)  "rrsig"  46)
   (DNST  (dnstype:any)    "any"    255)
   ))

(define (int->dnstype n)
  (let/cc return
    (for-list item dnstype-table
      (if (= item.val n)
          (return item.t)))
    (dnstype:other n)))

(define (dnstype->int t)
  (let/cc return
    (for-list item dnstype-table
      (if (eq? item.t t)
          (return item.val)))
    (match t with
      (dnstype:other n) -> n
      _ -> (impossible)
      )))

(define (name->dnstype name)
  (let/cc return
    (for-list item dnstype-table
      (if (string=? item.name name)
          (return item.t)))
    (raise (:DNSUnknownType name))))

;; no need for a 'class' afaik see, "there is only one!".
(define dnsclass-in 1)
;; only one opcode in real use, QUERY := 0
(define dnsopcode-query 0)

(datatype dnsrcode
  (:NoError)
  (:ServFail)
  (:NXDomain)
  )

(define int->dnsrcode
  (alist/make
   (0  (dnsrcode:NoError))
   (2  (dnsrcode:ServFail))
   (3  (dnsrcode:NXDomain))
   ))

(define dnsrcode->int (alist/inverse int->dnsrcode))

(define (dnsheader-make)
  {qid=0 qr=0 opcode=0 aa=0 tc=0 rd=0 ra=0 z=0 rcode=0
         qdcount=0 ancount=0 nscount=0 arcount=0})

(define (dnsheader-repr r)
  (format "<head qid=" (int r.qid) " qr=" (int r.qr) " op=" (int r.opcode)
          " aa=" (int r.aa) " tc=" (int r.tc) " rd=" (int r.rd) " ra=" (int r.ra)
          " z=" (int r.z) " rcode=" (int r.rcode)
          " qd=" (int r.qdcount) " an=" (int r.ancount) " ns=" (int r.nscount)
          " ar=" (int r.arcount) ">"))

(define (add-header p h)
  (p::u16 h.qid)
  (p::u16
   (packbits
    (4 h.rcode)
    (3 0)
    (1 h.ra)
    (1 h.rd)
    (1 h.tc)
    (1 h.aa)
    (4 h.opcode)
    (1 h.qr)))
  (p::u16 h.qdcount)
  (p::u16 h.ancount)
  (p::u16 h.nscount)
  (p::u16 h.arcount)
  )

(define (build-question qid name qtype qclass recursion)
  (let ((h (dnsheader-make))
        (p (pbuf/make 512))
        (nameparts (map-maker magic-cmp)))

    (define (pack-name s)
      (let loop ((parts (string-split s #\.)))
        (match (nameparts::get parts) with
          (maybe:yes pos)
          -> (p::u16 (logior #xc0 pos))
          (maybe:no)
          -> (match parts with
               ()   -> (p::u8 0)   ;; normal name
               ("") -> (p::u8 0)   ;; name ending in dot
               ("" . tl)          ;; name with empty label
               -> (raise (:BadDNSName s))
               (label . tl) ;; normal case
               -> (let ((len (string-length label)))
                    (if (> len 63)
                        (raise (:BadDNSName s))
                        (begin
                          ;; record this partial name's location
                          (nameparts::add parts (p::pos))
                          (p::u8 len)
                          (p::string label)
                          (loop tl)))))
          )))

    (set! h.opcode dnsopcode-query)
    (set! h.qid 3141)
    (set! h.qdcount 1)
    (set! h.rd recursion)
    (add-header p h)
    (pack-name name)
    (p::u16 (dnstype->int qtype))
    (p::u16 dnsclass-in)
    (p::val)
    ))

(datatype dnsrr
  (:a (vector char))
  (:aaaa (vector char))
  (:cname string)
  (:mx int string)
  (:ns string)
  (:ptr string)
  (:soa string string int int int int int)
  (:txt string)
  (:srv int int int string)
  (:dnskey int int int string)
  (:rrsig int int int int int int int string string)
  (:other int string)
  )

(define dnsrr-repr
  (dnsrr:a chars)
  -> (format "<a " (join int->string "." (map char->ascii (vector->list chars))) ">")
  (dnsrr:aaaa bytes)
  -> (format "<aaaa " (join int->hex-string ":" (map char->ascii (vector->list bytes))) ">")
  (dnsrr:cname name)
  -> (format "<cname " name ">")
  (dnsrr:mx pref name)
  -> (format "<mx " (int pref) " " name ">")
  (dnsrr:ns name)
  -> (format "<ns " name ">")
  (dnsrr:ptr name)
  -> (format "<ptr " name ">")
  (dnsrr:soa mname rname serial refresh retry expire minimum)
  -> (format "<soa " mname " " rname " serial=" (int serial) " refresh=" (int refresh)
             " retry=" (int retry) " expire=" (int expire) " min=" (int minimum) ">")
  (dnsrr:txt data)
  -> (format "<txt " (string data) ">")
  (dnsrr:srv priority weight port target)
  -> (format "<srv pri=" (int priority) " weight= " (int weight)
             " port=" (int port) " target=" target ">")
  (dnsrr:dnskey flags proto algo key)
  -> (format "<dnskeky flags=#x" (hex flags) " proto=" (int proto) " algo=" (int algo)
             " key=" (base64 key) ">")
  (dnsrr:rrsig type algo labels ttl expire incept keytag name sig)
  -> (format "<rrsig type=" (int type) " algo=" (int algo) " labels=" (int labels)
             " ttl=" (int ttl) " exp=" (int expire) " inc=" (int incept)
             " tag=" (int keytag) " name=" (string name) " sig=" (base64 sig) ">")
  (dnsrr:other type data)
  -> (format "<other type=" (int type) " data=" (string data) ">")
  )

(define (unpack-reply data)

  (let ((u (ubuf/make data))
        (h (dnsheader-make))
        (qid (u::u16))
        (flags (u::u16))
        (nameparts (map-maker magic-cmp))
        (r {qd='() an='() ns='() ar='()}))

    (define (unpack-name* acc)
      (let ((pos (u::pos))
            (n0 (u::u8))
            (n1 (logand n0 #x3f)))
        (cond ((= #xc0 (logand n0 #xc0))
               ;; pointer
               (match (nameparts::get (logior (<< n1 8) (u::u8))) with
                 (maybe:yes parts) -> (append parts acc)
                 (maybe:no) -> (raise (:BadDNSPacket "bad name pointer" u))))
              ((= n1 0) acc)
              (else
               ;; normal label
               (let ((label (u::string n1))
                     (r (unpack-name* (list:cons label acc))))
                 ;; tricky: we want to store only the labels that came *after* this point.
                 (nameparts::add pos (slice r 0 (- (length r) (length acc))))
                 r)))
        ))

    (define (unpack-name)
      (let ((labels (unpack-name* '())))
        (string-join (reverse labels) ".")))

    (define (decode-rr type len)
      (match type with
        (dnstype:a)
        -> (let ((data (u::string len))
                 (r (make-vector len #\0)))
             (for-range i len
               (set! r[i] (string-ref data i)))
             (dnsrr:a r))
        (dnstype:aaaa)
        -> (let ((data (u::string len))
                 (r (make-vector len #\0)))
             (for-range i len
               (set! r[i] (string-ref data i)))
             (dnsrr:aaaa r))
        (dnstype:mx)
        -> (let ((pref (u::u16))
                 (name (unpack-name)))
             (dnsrr:mx pref name))
        (dnstype:soa)
        -> (let ((mname (unpack-name))
                 (rname (unpack-name))
                 (serial (u::u32))
                 (refresh (u::u32))
                 (retry (u::u32))
                 (expire (u::u32))
                 (minimum (u::u32)))
             (dnsrr:soa mname rname serial refresh retry expire minimum))
        ;; fun note: this wire format is actually not in the rfc.
        (dnstype:srv)
        -> (let ((priority (u::u16))
                 (weight (u::u16))
                 (port (u::u16))
                 (target (unpack-name)))
             (dnsrr:srv priority weight port target))
        (dnstype:dnskey)
        -> (let ((flags (u::u16))
                 (proto (u::u8))
                 (algo (u::u8)))
             (dnsrr:dnskey flags proto algo (u::string (- len 4))))
        (dnstype:rrsig)
        -> (let ((pos0 (u::pos))
                 (type (u::u16))
                 (algo (u::u8))
                 (labels (u::u8))
                 (ttl (u::u32))
                 (exp (u::u32))
                 (inc (u::u32))
                 (tag (u::u16))
                 (name (unpack-name))
                 (pos1 (u::pos)))
             (dnsrr:rrsig type algo labels ttl exp inc tag name
                          (u::string (- len (- pos1 pos0)))))
        (dnstype:ns)      -> (dnsrr:ns (unpack-name))
        (dnstype:cname)   -> (dnsrr:cname (unpack-name))
        (dnstype:ptr)     -> (dnsrr:ptr (unpack-name))
        (dnstype:txt)     -> (dnsrr:txt (u::string len))
        (dnstype:other x) -> (dnsrr:other x (u::string len))
        ;; this should not happen
        (dnstype:any)     -> (dnsrr:other 255 (u::string len))
        ))

    (define (unpack-rr)
      (let ((name (unpack-name))
            (type (int->dnstype (u::u16)))
            (class (u::u16)) ;; XXX verify as `IN`
            (ttl (u::u32))
            (rdlength (u::u16)))
        {name=name type=type class=class ttl=ttl
                   data=(decode-rr type rdlength)}
        ))

    (define (unpack-question)
      (let ((name (unpack-name))
            (qtype (u::u16))
            (qclass (u::u16)))
        {name=name qtype=qtype qclass=qclass}))

    (unpackbits
     flags
     (4 h.rcode)
     (3 h.z)
     (1 h.ra)
     (1 h.rd)
     (1 h.tc)
     (1 h.aa)
     (4 h.opcode)
     (1 h.qr))
    (set! h.qdcount (u::u16))
    (set! h.ancount (u::u16))
    (set! h.nscount (u::u16))
    (set! h.arcount (u::u16))
    (when (= h.tc 1)
      (printf "reply truncated\n")
      (raise (:DNSTruncated)))
    (printf "header: " (dnsheader-repr h) "\n")
    (for-range i h.qdcount (push! r.qd (unpack-question)))
    (for-range i h.ancount (push! r.an (unpack-rr)))
    (for-range i h.nscount (push! r.ns (unpack-rr)))
    (for-range i h.arcount (push! r.ar (unpack-rr)))
    (:tuple h r)
    ))

(define rr-repr
  {name=name type=type class=class ttl=ttl data=rr}
  -> (format "(" name " ttl=" (int ttl) " " (dnsrr-repr rr) ")"))

(define print-reply
  (:tuple h r)
  -> (begin
       (printf "reply: {\n")
       (printf "  " (dnsheader-repr h) "\n")
       (for-list item r.qd
         (printf "  Q " item.name " qtype=" (int item.qtype) " qclass=" (int item.qclass) "\n"))
       (for-list item r.an (printf " AN " (rr-repr item) "\n"))
       (for-list item r.ns (printf " NS " (rr-repr item) "\n"))
       (for-list item r.ar (printf " AR " (rr-repr item) "\n"))
       (printf "}\n")
       ))

(define (tcp-enc-size-prefix len)
  (list->string
   (LIST (ascii->char (>> len 8))
         (ascii->char (logand #xff len)))))

(define (tcp-dec-size-prefix s)
  (logior
   (<< (char->ascii (string-ref s 0)) 8)
   (char->ascii (string-ref s 1))))

(define (go ip name)
  (let ((sock (doom/make (udp4-sock) 1024 1024))
        (addr (address/make4 ip 53))
        (packet (build-question 3141 name (dnstype:any) dnsclass-in 1)))
    (try
     (begin
       (sock.connect addr)
       (sock.send packet)
       (let ((reply (sock.recv))
             (unpacked (unpack-reply reply)))
         (print-reply unpacked)
         (sock.close)))
     except
     (:DNSTruncated)
     -> (begin
          (printf "trying tcp...\n")
          (sock.close)
          (set! sock (doom/make (tcp4-sock) 8192 1024))
          (sock.connect addr)
          (sock.send (string-append (tcp-enc-size-prefix (string-length packet)) packet))
          (let ((size (tcp-dec-size-prefix (sock.recv-exact 2)))
                (_ (printf "tcp reply size = " (int size) "\n"))
                (reply (sock.recv-exact size))
                (unpacked (unpack-reply reply)))
            (print-reply unpacked)
            (sock.close))
          )
     )))

(poller/fork (lambda () (go "10.0.0.1" sys.argv[1])))
(poller/wait-and-schedule)
