;; -*- Mode: Irken -*-

;; packbits/unpackbits (lib/pack.scm).  unpackbits used to substitute
;;  its value into both the mask and the shift at every field, so code
;;  grew 2^n with n fields: dns.scm's 8-field header ran the native
;;  backends out of registers.  the value must be evaluated once.

(include "lib/basis.scm")
(include "lib/pack.scm")

(define evals 0)
(define (value n) (set! evals (+ evals 1)) n)

(let ((h {rcode=0 z=0 ra=0 rd=0 tc=0 aa=0 opcode=0 qr=0}))
  ;; a dns header's flags: qr=1 opcode=2 aa=1 tc=0 rd=1 ra=1 z=0 rcode=3
  (let ((left (unpackbits
               (value #b1001010110000011)
               (4 h.rcode) (3 h.z) (1 h.ra) (1 h.rd) (1 h.tc) (1 h.aa) (4 h.opcode) (1 h.qr))))
    (printf (int h.qr) " " (int h.opcode) " " (int h.aa) " " (int h.tc) " "
            (int h.rd) " " (int h.ra) " " (int h.z) " " (int h.rcode) "\n")
    (printf "left " (int left) " evals " (int evals) "\n")
    (printf (int (packbits (4 h.rcode) (3 h.z) (1 h.ra) (1 h.rd) (1 h.tc) (1 h.aa) (4 h.opcode) (1 h.qr))) "\n")))

;; => 1 2 1 0 1 1 0 3
;; => left 0 evals 1
;; => 38275
