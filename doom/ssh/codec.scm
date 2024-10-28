;; -*- Mode: Irken -*-

(require "lib/bignum.scm")
(require "lib/codecs/hex.scm")
(require "doom/ssh/protocol.scm")

;; really wants to live in bignum, which probably already depends on hex.scm
(define (big->hex n)
  (string->hex (big->u256 n)))

;; types in packets
(datatype sshT
  (:byte1)     ;; u8
  (:byten int) ;; IMPLIED length
  (:bool)      ;; u8 {\x00,\x01}
  (:uint32)    ;; u32
  (:uint64)    ;; (u32 u32)
  (:string)    ;; 8-bit, (uint32, data)
  (:mpint)     ;; two's-complement big endian (uint32, data)
  (:namelist)  ;; comma-separated, (uint32, data)
  )

;; data in packets
(datatype sshD
  (:byte1 int)
  (:byten string)
  (:bool bool)
  (:uint32 int)
  (:uint64 int int) ;; two u32s hi lo
  (:string string)
  (:mpint big)
  (:namelist (list string))
  )

(define sshD->sexp
  (sshD:byte1 n)     -> (sexp:int n)
  (sshD:byten bytes) -> (sexp (sym 'byten) (string (string->hex bytes)))
  (sshD:bool bool)   -> (sexp:bool bool)
  (sshD:uint32 n)    -> (sexp (sym 'u32) (int n))
  (sshD:uint64 a b)  -> (sexp (sym 'u64) (int a) (int b))
  (sshD:string s)    -> (sexp:string s)
  (sshD:mpint n)     -> (sexp (sym 'mpint) (string (big->hex n)))
  (sshD:namelist nl) -> (sexp1 'namelist (map sexp:string nl))
  )

(define (ssh-data->sexp data)
  (define R
    acc (alist:nil)          -> (sexp:list (reverse acc))
    acc (alist:entry k v tl) -> (R (list:cons (sexp (sym k) (sshD->sexp v)) acc) tl)
    )
  (R (list:nil) data))

(defmacro sshP
  (sshP <b>)        -> (sshT:byte1)
  (sshP (<b> size)) -> (sshT:byten size)
  (sshP <bool>)     -> (sshT:bool)
  (sshP <u32>)      -> (sshT:uint32)
  (sshP <u64>)      -> (sshT:uint64)
  (sshP <s>)        -> (sshT:string)
  (sshP <i>)        -> (sshT:mpint)
  (sshP <nl>)       -> (sshT:namelist)
  )

(defmacro ssh-pkt
  (ssh-pkt)
  -> (alist:nil)
  (ssh-pkt (name kind) fields ...)
  -> (alist:entry
      (quote (%%symbol name))
      (sshP kind)
      (ssh-pkt fields ...))
  )

(defmacro ssh-data
  (ssh-data)
  -> (alist:nil)
  (ssh-data (name data) fields ...)
  -> (alist:entry
      (quote (%%symbol name))
      data
      (ssh-data fields ...))
  )

(define descriptors
  (alist/make
   ((ssh-mtype:kexinit)
    (ssh-pkt (cookie (b 16))
             (kex-algs nl)
             (hkey-algs nl)
             (enc-c2s nl)
             (enc-s2c nl)
             (mac-c2s nl)
             (mac-s2c nl)
             (cmp-c2s nl)
             (cmp-s2c nl)
             (lng-c2s nl)
             (lng-s2c nl)
             (first-follows bool)
             (reserved u32)
             ))
   ((ssh-mtype:disconnect)       (ssh-pkt (reason u32) (description s) (language s)))
   ((ssh-mtype:kex-ecdh-init)    (ssh-pkt (q-c s)))
   ((ssh-mtype:kex-ecdh-reply)   (ssh-pkt (k-s s) (q-s s) (sig s)))
   ((ssh-mtype:newkeys)          (ssh-pkt)) ;; no data
   ((ssh-mtype:ignore)           (ssh-pkt (data s)))
   ((ssh-mtype:debug)            (ssh-pkt (always-display bool) (message s) (language s)))
   ((ssh-mtype:unimplemented)    (ssh-pkt (packet u32)))
   ((ssh-mtype:service-request)  (ssh-pkt (name s)))
   ((ssh-mtype:service-accept)   (ssh-pkt (name s)))
   ((ssh-mtype:userauth-request) (ssh-pkt (user s) (service s) (method s)))
   ((ssh-mtype:userauth-failure) (ssh-pkt (auths nl) (partial bool)))
   ((ssh-mtype:userauth-success) (ssh-pkt))
   ((ssh-mtype:userauth-banner)  (ssh-pkt (message s) (language s)))
   ((ssh-mtype:userauth-pk-ok)   (ssh-pkt (alg s) (pkey s)))
   ((ssh-mtype:channel-open)     (ssh-pkt (type s) (schan u32) (winsz u32) (max u32)))
   ((ssh-mtype:channel-open-yes) (ssh-pkt (rchan u32) (schan u32) (winsz u32) (max u32)))
   ((ssh-mtype:channel-open-no)  (ssh-pkt (rchan u32) (reason u32) (why s) (lang s)))
   ((ssh-mtype:channel-request)  (ssh-pkt (rchan u32) (type s) (want bool)))
   ((ssh-mtype:channel-data)     (ssh-pkt (rchan u32) (data s)))
   ((ssh-mtype:channel-close)    (ssh-pkt (rchan u32)))
   ((ssh-mtype:channel-success)  (ssh-pkt (rchan u32)))
   ((ssh-mtype:channel-failure)  (ssh-pkt (rchan u32)))
   ))

(define (get-descriptor n)
  (match (alist/lookup ssh-mtype-rev-alist n) with
    (maybe:yes msg-type)
    -> (alist/lookup descriptors msg-type)
    (maybe:no)
    -> (maybe:no)
    ))

;; unfortunately, `int->msg-type` basically panics when
;;   given an out-of-range input, so we have to roll our own.
;;   XXX need to fix the definitions in lib/enum.scm to do something
;;       more useful!
(define (code->msg-type n)
  (alist/lookup*
   ssh-mtype-rev-alist n
   (ssh-mtype:unknown)))

(define (msg-type/code->name n)
  (alist/lookup*
   ssh-mtype-sym-alist
   (alist/lookup*
    ssh-mtype-rev-alist n (ssh-mtype:unknown))
   'unknown))

(define (ssh-pkt/get pkt name)
  (match (alist/lookup pkt name) with
    (maybe:yes val)
    -> val
    (maybe:no)
    -> (raise (:SSH/Error "unable to fetch field from packet"))
    ))

;; worth defining a macro to generate these?
(define (ssh-pkt/get-string pkt name)
  (match (ssh-pkt/get pkt name) with
    (sshD:string val) -> val
    other -> (raise (:SSH/Error "expected string in packet field"))
    ))

(define (ssh-pkt/get-mpint pkt name)
  (match (ssh-pkt/get pkt name) with
    (sshD:mpint val) -> val
    other -> (raise (:SSH/Error "expected mpint in packet field"))
    ))

(define (ssh-pkt/get-bool pkt name)
  (match (ssh-pkt/get pkt name) with
    (sshD:bool val) -> val
    other -> (raise (:SSH/Error "expected bool in packet field"))
    ))

(define (ssh-pkt/get-u32 pkt name)
  (match (ssh-pkt/get pkt name) with
    (sshD:uint32 val) -> val
    other -> (raise (:SSH/Error "expected uint32 in packet field"))
    ))

(define (ssh-pkt/extra pkt)
  (match (alist/lookup pkt 'extra) with
    (maybe:yes (sshD:string val)) -> val
    _ -> ""
    ))

;; this nonsensical thing is needed because we kinda 'misuse'
;;  alists as an ordered dictionary of key/val pairs.
(define alist/reverse-onto
  acc (alist:nil) -> acc
  acc (alist:entry k v tl) -> (alist/reverse-onto (alist:entry k v acc) tl)
  )

(define (alist/reverse al)
  (alist/reverse-onto (alist:nil) al))

(define (ssh-unpack-with-descriptor pkt pos desc)

  (let ((plen (string-length pkt))
        (r (alist:nil)))

    (define (assure n)
      (when (not (<= (+ pos n) plen))
        (raise (:SSH/Decode pkt pos "underflow"))))

    (define (bump exp n)
      (inc! pos n)
      exp)

    (define (bref s n)
      (char->int (string-ref s n)))

    (define (get-b1)
      (assure 1)
      (bump (char->int (string-ref pkt pos)) 1))

    (define (get-bn n)
      (assure n)
      (bump (substring pkt pos (+ pos n)) n))

    (define (get-bool)
      (match (get-b1) with
        0 -> #f
        1 -> #t
        _ -> (raise (:SSH/Decode pkt pos "bad bool"))
        ))

    (define (get-u32)
      (let ((s (get-bn 4)))
        (+ (<< (bref s 0) 24)
           (<< (bref s 1) 16)
           (<< (bref s 2)  8)
           (bref s 3))))

    (define (get-string)
      (let ((slen (get-u32))
            (data (get-bn slen)))
        data))

    (define (get-mpint)
      (let ((ilen (get-u32))
            (data (get-bn ilen)))
        (u256->big data)))

    (define (get-namelist)
      (let ((len (get-u32))
            (data (get-bn len)))
        (string-split data #\,)))

    (define unpack-item
      (sshT:byte1)    -> (sshD:byte1 (get-b1))
      (sshT:byten n)  -> (sshD:byten (get-bn n))
      (sshT:bool)     -> (sshD:bool (get-bool))
      (sshT:uint32)   -> (sshD:uint32 (get-u32))
      (sshT:uint64)   -> (sshD:uint64 (get-u32) (get-u32))
      (sshT:string)   -> (sshD:string (get-string))
      (sshT:mpint)    -> (sshD:mpint (get-mpint))
      (sshT:namelist) -> (sshD:namelist (get-namelist))
      )

    (for-alist name kind desc
      (let ((data (unpack-item kind)))
        (alist/push r name data)))
    (when (< pos plen)
      (let ((extra (substring pkt pos plen)))
        (alist/push r 'extra (sshD:string extra))))
    (alist/reverse r)
    ))

(define (dump-sshdata name pkt)
  (let ((pkt-sexp (ssh-data->sexp pkt)))
    (printf (sym name) ":\n")
    (pp pkt-sexp 120)
    ))

(define (ssh-unpack* data prefix)
  (let ((kind (char->int (string-ref data 0))))
    (match (get-descriptor kind) with
      (maybe:yes desc)
      -> (let ((result (ssh-unpack-with-descriptor data 1 desc))
               (name (msg-type/code->name kind)))
           ;; suppress channel data
           (when (not (= kind 94))
             (printf prefix)
             (dump-sshdata name result)
             (printf "\n"))
           result)
      (maybe:no)
      -> (raise (:SSH/Decode data 0 "unknown descriptor"))
      )))

(define (ssh-unpack data)
  (ssh-unpack* data "<<< "))

(define (as-char n)
  (int->char (logand #xff n)))

(define (u8->be n)
  (char->string (as-char n)))

(define (u32->be n)
  (let ((s (make-string 4)))
    (string-set! s 0 (as-char (>> n 24)))
    (string-set! s 1 (as-char (>> n 16)))
    (string-set! s 2 (as-char (>> n  8)))
    (string-set! s 3 (as-char (>> n  0)))
    s))

(define (be->int s)
  (define R
    acc ()           -> acc
    acc (dig . digs) -> (R (logior dig (<< acc 8)) digs)
    )
  (R 0 (map char->int (string->list s))))

(define (u64->be a b)
  (string-append (u32->be a) (u32->be b)))

(define (int->u64->be n)
  (let ((hi (logand (>> n 32) #xffffffff))
        (lo (logand (>> n  0) #xffffffff)))
    (u64->be hi lo)))

(define (packer pkt)

  (define pads
    (let ((v (make-vector 7 (rope:leaf "\x00"))))
      (for-range i 7
        (set! v[i] (rope:leaf (format (repeat (+ i 1) "\x00")))))
      v))

  (define (enc-bool b)
    (rope:leaf (if b "\x01" "\x00")))

  (define (enc-u8 n)
    (rope:leaf (u8->be n)))

  (define (enc-u32 n)
    (rope:leaf (u32->be n)))

  (define (enc-u64 a b)
    (rope:leaf (u64->be a b)))

  (define (enc-string s)
    (rope/build (enc-u32 (string-length s)) (rope:leaf s)))

  (define (enc-mpint n)
    (let ((u256 (big->u256 n))
          (bytes (if (= #x80 (logand #x80 (char->int (string-ref u256 0))))
                     (rope/build pads[0] (rope:leaf u256))
                     (rope:leaf u256)))
          (len (rope-length bytes)))
      (rope/build (enc-u32 len) bytes)))

  (define (enc-namelist names)
    (let ((data (rope/join (rope:leaf ",") (map rope:leaf names)))
          (dlen (rope-length data)))
      (rope/build (enc-u32 dlen) data)))

  (define (enc-item item)
    (match item with
      (sshD:byte1 n)     -> (enc-u8 n)
      (sshD:byten s)     -> (rope:leaf s)
      (sshD:bool b)      -> (enc-bool b)
      (sshD:uint32 n)    -> (enc-u32 n)
      (sshD:uint64 a b)  -> (enc-u64 a b)
      (sshD:string s)    -> (enc-string s)
      (sshD:mpint n)     -> (enc-mpint n)
      (sshD:namelist ns) -> (enc-namelist ns)
      ))

  (let ((r (list:nil)))
    (for-alist k v pkt
      (push! r (enc-item v)))
    (rope/cat (reverse r))
    ))

;; packer for a particular message type
(define (msg-pack msg-type data)
  ;; prefix the message type tag
  (packer (alist:entry 'tag (sshD:byte1 (ssh-mtype->int msg-type)) data)))

(define ssh/RNG (urandom-make))

(define (make-kexinit)
  (ssh-data
   (cookie (sshD:byten (ssh/RNG 16)))
   (kex-algs (sshD:namelist (list "curve25519-sha256")))
   (hkey-algs (sshD:namelist (list "ssh-ed25519")))
   (enc-c2s (sshD:namelist (list "chacha20-poly1305@openssh.com")))
   (enc-s2c (sshD:namelist (list "chacha20-poly1305@openssh.com")))
   ;; (enc-c2s (sshD:namelist (list "aes256-gcm@openssh.com")))
   ;; (enc-s2c (sshD:namelist (list "aes256-gcm@openssh.com")))
   ;; (mac-c2s (sshD:namelist (list "hmac-sha2-256-etm@openssh.com")))
   ;; (mac-s2c (sshD:namelist (list "hmac-sha2-256-etm@openssh.com")))
   (mac-c2s (sshD:namelist (list:nil)))
   (mac-s2c (sshD:namelist (list:nil)))
   (cmp-c2s (sshD:namelist (list "none")))
   (cmp-s2c (sshD:namelist (list "none")))
   (lng-c2s (sshD:namelist (list:nil)))
   (lng-s2c (sshD:namelist (list:nil)))
   (first-follows (sshD:bool #f))
   (reserved (sshD:uint32 0))
   ))

;; XXX do better
(define (pack-public-key pkey)
  (rope->string
   (packer
    (ssh-data
     (name (sshD:string "ssh-ed25519"))
     (val (sshD:string pkey))))))

(define (pack-signature sig)
  (rope->string
   (packer
    (ssh-data
     (name (sshD:string "ssh-ed25519"))
     (val (sshD:string sig))))))

(define (string->mpint s)
  (rope->string
   (packer
    (ssh-data
     (key (sshD:mpint (u256->big s)))))))

(define (build-ecdh-hash VC VS IC IS KS QC QS K)
  ;; this round-trip through bignum is necessary for compatibility
  ;; K is interpreted as if it were a big-endian number
  (let ((kbig (u256->big K))
        (data
         (ssh-data
          (vc (sshD:string VC))
          (vs (sshD:string VS))
          (ic (sshD:string IC))
          (is (sshD:string IS))
          (ks (sshD:string KS))
          (qc (sshD:string QC))
          (qs (sshD:string QS))
          (k (sshD:mpint kbig)))))
    (rope->string (packer data))
    ))

(define (build-ecdh-reply ks qs sig)
  (msg-pack
   (ssh-mtype:kex-ecdh-reply)
   (ssh-data
    (ks (sshD:string ks))
    (qs (sshD:string qs))
    (sig (sshD:string sig))
    )))

(define (build-channel-open-no chan reason why lang)
  (let ((reason0 (ssh-open->int reason)))
    (msg-pack
     (ssh-mtype:channel-open-no)
     (ssh-data
      (rchan (sshD:uint32 chan))
      (reason (sshD:uint32 reason0))
      (why (sshD:string why))
      (lang (sshD:string lang))))))

(define (build-channel-open-yes c2s-chan s2c-chan winsz maxpkt)
  (msg-pack
   (ssh-mtype:channel-open-yes)
   (ssh-data
    (c2s (sshD:uint32 c2s-chan))
    (s2c (sshD:uint32 s2c-chan))
    (winsz (sshD:uint32 winsz))
    (maxpkt (sshD:uint32 maxpkt))
    )))

(define (build-channel-data rchan data)
  (msg-pack
   (ssh-mtype:channel-data)
   (ssh-data
    (rchan (sshD:uint32 rchan))
    (data (sshD:string data)))))

(define (build-channel-close rchan)
  (msg-pack (ssh-mtype:channel-close) (ssh-data (rchan (sshD:uint32 rchan)))))

(define (build-channel-success rchan)
  (msg-pack (ssh-mtype:channel-success) (ssh-data (rchan (sshD:uint32 rchan)))))

(define (build-channel-failure rchan)
  (msg-pack (ssh-mtype:channel-failure) (ssh-data (rchan (sshD:uint32 rchan)))))
