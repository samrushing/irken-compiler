;; -*- Mode: Irken -*-

(require "lib/codecs/base64.scm")

;; XXX figure out how to combine push/pull with
;;  generators so we can feed the base64 decoder
;;  as a stream.

(define (line-gen chargen)
  (makegen emit
    (let ((acc '()))
      (for ch chargen
        (cond ((char=? ch #\newline)
               (emit (list->string (reverse acc)))
               (set! acc '()))
              (else (PUSH acc ch)))
        ))))

(define (pem-b64-gen linegen)
  (makegen emit
    (let ((label "null")
          (data '()))
      (for line linegen
        ;; we use <label> as a state variable.
        (match label with
          "null" -> (cond ((and
                            (starts-with line "-----BEGIN ")
                            (ends-with line "-----"))
                           (set! label (substring line 11 (- (string-length line) 5))))
                          (else
                           ;; ignore noise lines
                           #u))
          label -> (cond ((and
                           (starts-with line "-----END ")
                           (ends-with line "-----"))
                          (emit (:tuple label (string-concat (reverse data))))
                          (set! label "null")
                          (set! data '()))
                         (else
                          (PUSH data line)))
          )))))

(define (pem-gen chargen)
  (makegen emit
    (for section (pem-b64-gen (line-gen chargen))
      (match section with
        (:tuple label b64)
        -> (emit (:tuple label (b64-decode b64)))
        ))))

