;; -*- Mode: Irken -*-

;; XXX: take another look at this compared to the base85 codec, which is
;;   more elegant.

(define *base64-digits* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

(define *base64-reverse*
  (let ((v (make-vector 256 -1)))
    (for-range i 64
      (set! v[(char->ascii (string-ref *base64-digits* i))] i))
    v))

(datatype b64enc (:a) (:b) (:c))

;; 000000 001111 111122 222222

(define (b64-enc gen)
  (make-generator
   (lambda (consumer)

     (define (emit index)
       (consumer (maybe:yes (string-ref *base64-digits* index))))

     (let ((b0 0)
           (state (b64enc:a)))
       (for ch0 gen
         (let ((ch (char->ascii ch0)))
           (match state with
             (b64enc:a) -> (begin (emit (>> ch 2))
                                  (set! b0 (<< (logand #x3 ch) 4))
                                  (set! state (b64enc:b)))
             (b64enc:b) -> (begin (emit (logior b0 (>> ch 4)))
                                  (set! b0 (<< (logand #xf ch) 2))
                                  (set! state (b64enc:c)))
             (b64enc:c) -> (begin (emit (logior b0 (>> ch 6)))
                                  (emit (logand ch #x3f))
                                  (set! state (b64enc:a)))
             )))
       (match state with
         (b64enc:a) -> #u
         (b64enc:b) -> (begin (emit b0) (emit 64) (emit 64))
         (b64enc:c) -> (begin (emit b0) (emit 64))) ;; #\=
       (forever (consumer (maybe:no)))
       ))))

(datatype b64dec (:a) (:b) (:c) (:d))

;; 00000011 11112222 22333333

(define (b64-dec gen)
  (make-generator
   (lambda (consumer)

     (define (emit n)
       (consumer (maybe:yes (ascii->char n))))

     (define (D ch) *base64-reverse*[(char->ascii ch)])

     (let ((b0 0)
           (state (b64dec:a)))

       (for ch0 gen
         (match (D ch0) state with
           -1 _          -> #u ;; other chars
           ch (b64dec:a) -> (begin (set! b0 (<< ch 2))
                                   (set! state (b64dec:b)))
           ch (b64dec:b) -> (begin (emit (logior b0 (>> ch 4)))
                                   (set! b0 (<< (logand #xf ch) 4))
                                   (set! state (b64dec:c)))
           ch (b64dec:c) -> (begin (emit (logior b0 (>> ch 2)))
                                   (set! b0 (<< (logand #x3 ch) 6))
                                   (set! state (b64dec:d)))
           ch (b64dec:d) -> (begin (emit (logior b0 ch))
                                   (set! state (b64dec:a)))
           ))
       (forever (consumer (maybe:no)))
       ))))

(define (b64-decode s)
  (let ((dst (make-string (* 3 (how-many (string-length s) 4))))
        (i 0))
    (for ch (b64-dec (string-generator s))
      (string-set! dst i ch)
      (set! i (+ i 1)))
    (if (= i (string-length dst))
        dst
        (substring dst 0 i))
    ))

(define (b64-encode s)
  (let ((dst (make-string (* 4 (how-many (string-length s) 3))))
        (i 0))
    (for ch (b64-enc (string-generator s))
      (string-set! dst i ch)
      (set! i (+ i 1)))
    dst
    ))
