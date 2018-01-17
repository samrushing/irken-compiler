;; -*- Mode: Irken -*-

;; runtime reflection. this should grow up to replace dump_object() eventually.

(datatype ident
  (:int int)
  (:userimm int int)   ;; tag payload
  (:usertup int int)   ;; tag length
  (:sysimm symbol int) ;; kind payload
  (:systup symbol int) ;; kind length
  )

(define (~~get-payload-len-tag o)
  (let ((raw0 (~~object->int o)))
    (cond ((= 1 (logand #x1 raw0)) ;; integer
           (:tuple (>> raw0 1) 0 0))
          ((> (logand #x3 raw0) 0) ;; immediate
           (:tuple (>> raw0 8) 0 (logand #xff raw0)))
          (else
           (let ((raw1 (~~object*->int o))) ;; tuple
             (:tuple 0 (>> raw1 8) (logand #xff raw1))))
          )))

;; note: these match the defines in `include/pxll.h`

(define ~~imm-tags #('char 'bool 'nil 'undefined 'empty-vector))
(define ~~ptr-tags #('none 'save 'closure 'env 'string 'vector 'pair 'symbol 'buffer 'foreign))

(define (~~whatisthis ob)
  (let (((payload len tag) (~~get-payload-len-tag ob)))
    (cond ((and (= len 0) (= tag 0))
           (ident:int payload))
          ((and (> len 0) (= payload 0))
           (if (>= tag #x28)
               (ident:usertup (- (>> tag 2) 10) len)
               (ident:systup ~~ptr-tags[(>> tag 2)] len)))
          (else
           (if (>= tag #x16)
               (ident:userimm (- (>> tag 2) 5) payload)
               (ident:sysimm ~~imm-tags[(>> tag 2)] payload)))
          )))

(define (~~identify ob)
  (match (~~whatisthis ob) with
    (ident:int n)
    -> (printf "integer " (int n) "\n")
    (ident:usertup tag len)
    -> (printf "user tuple " (int tag) " len=" (int len) "\n")
    (ident:systup kind len)
    -> (printf "builtin tuple " (sym kind) " len=" (int len) "\n")
    (ident:userimm tag payload)
    -> (printf "user immediate " (int tag) " payload = " (int payload) "\n")
    (ident:sysimm kind payload)
    -> (printf "builtin immediate " (sym kind) " payload = " (int payload) "\n")
    ))

(define (variant->name v)
  (when (= 0 (vector-length the-variant-label-map))
    (build-variant-label-map))
  (match (~~whatisthis v) with
    (ident:userimm tag payload) -> the-variant-label-map[payload]
    (ident:usertup tag len)     -> the-variant-label-map[tag]
    _                           -> (impossible)
    ))
