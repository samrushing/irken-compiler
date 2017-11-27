;; -*- Mode: Irken -*-

;; https://en.wikipedia.org/wiki/Ascii85
;; base85 is a 4->5 encoding

;; we implement this as two sets of codecs ...
;;
;;   1) a uint32 <=> char[4] codec
;;   2) a char[5] <=> uint32 codec
;;
;; ... which are chained together.

;; since there are several approaches to padding in the
;;  various 'standards', this issue is left to the user.
;; [hint: use padding/unpadding generators]

;; Note: no support for the special meanings for the 'z'
;;  and 'y' chars.

;; char * 4 -> uint32
(define (int32-enc gen)
  (make-generator
   (lambda (consumer)
     (define (get)
       (match (gen) with
         (maybe:no)      -> (forever (consumer (maybe:no)))
         (maybe:yes val) -> (char->int val)
         ))
     (while #t
       (consumer
        (maybe:yes
         (logior* (<< (get) 24)
                  (<< (get) 16)
                  (<< (get)  8)
                  (<< (get)  0)))))
     )))

;; uint32 -> char * 4
(define (int32-dec gen)
  (make-generator
   (lambda (consumer)
     (define (put n)
       (consumer (maybe:yes (int->char (logand #xff n)))))
     (while #t
       (match (gen) with
         (maybe:no) -> (forever (consumer (maybe:no)))
         (maybe:yes val)
         -> (begin
              (put (>> val 24))
              (put (>> val 16))
              (put (>> val  8))
              (put (>> val  0))))))))

;; uint32 -> char * 5
(define (b85/32-enc gen32)
  (make-generator
   (lambda (consumer)
     (define (put n)
       (consumer (maybe:yes (int->char (+ 33 n)))))
     (for val gen32
       (let loop ((i 0) (vals '()) (val val))
         (if (= i 5)
             (for-each put vals)
             (loop (+ i 1) (list:cons (mod val 85) vals) (/ val 85)))))
     (forever (consumer (maybe:no)))
     )))

;; char * 5 -> uint32
(define (b85/32-dec gen)
  (make-generator
   (lambda (consumer)
     (define (get)
       (match (gen) with
         (maybe:no)      -> (forever (consumer (maybe:no)))
         (maybe:yes val) -> (- (char->int val) 33)
         ))
     (while #t
       (consumer
        (maybe:yes
         (let ((val 0))
           (for-range i 5
             (set! val (+ (get) (* val 85))))
           val))))
     )))

(define (b85-enc gen)
  (b85/32-enc (int32-enc gen)))

(define (b85-dec gen)
  (int32-dec (b85/32-dec gen)))
