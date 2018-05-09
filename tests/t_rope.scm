;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/rope.scm")

;; `nstring` is an experiment: create `s*n` using a single copy
;;   of `s` and building a tree structure with repeated sharing.
;;   [i.e., create SS, ((SS*2)*2)*2...]

;; AAAA
;; x = (A A)
;; y = (x x) == AAAA
;; z = (y y) == AAAAAAAA
;;
;; we get the most efficient encoding when
;;  the size of the node is smaller than the plain string.
;;  on 64-bit, a plain string is:

;;  8 bytes: tag+len
;;  4 bytes: slen
;;  n bytes: string

;; 2 bytes: 8 + 4 + 8 = 20 bytes
;; 8 bytes: 8 + 4 + 8 = 20 bytes
;; 16 bytes: 8 + 4 + 16 = 28 bytes

;; node overhead = 24 bytes
;;  8 bytes: tag + len
;;  8 bytes: left tree
;;  8 bytes: right tree

;; so if we are building a string longer than 8 bytes,
;;  it makes sense to build it from a rope.

;; return `n` as a list of bits, msb-first.
(define n->bits
  0 acc -> acc
  n acc -> (n->bits (>> n 1) (list:cons (if (odd? n) #t #f) acc))
  )

;; first bit is always '1', meaning a single copy.
;; 1 -> A
;; 10 -> AA
;; 11 -> AA.A

;; we want string `s` repeated `n` times.
;; we share structure inside the tree to get an efficient
;; representation.

;; XXX we could use smart constructors to get the most efficient
;;  packing of strings (i.e., smaller than 8 bytes we just use a string).

(define (nstring s n)

  (define loop
    ()       acc -> acc
    (#f . tl) acc -> (loop tl (rope-make acc acc)) ;; 2 * acc
    (#t . tl) acc -> (loop tl (rope-make (rope-make acc acc) s)) ;; 2 * acc + acc
    )
  (if (= n 0)
      (rope:leaf "")
      (loop (cdr (n->bits n '())) s)))

;; test nstring
(define (t0)
  (let ((r (nstring (rope:leaf "bart ") 17)))
    (assert (string=? (rope->string r) (format (repeat 17 "bart "))))
    ))

;; test list->rope
(define (t1)
  (assert
   (eq?
    (cmp:=)
    (magic-cmp
     (list->rope (string-split "the quick brown fox jumped over the lazy dog" #\space))
     (rope:node 33
      (rope:node 16
       (rope:node 8
        (rope:node 3
           (rope:leaf "the")
           (rope:leaf "quick"))
        (rope:node 5
           (rope:leaf "brown")
           (rope:leaf "fox")))
       (rope:node 10
         (rope:node 6
           (rope:leaf "jumped")
           (rope:leaf "over"))
         (rope:node 3
           (rope:leaf "the")
           (rope:leaf "lazy"))))
      (rope:leaf "dog"))
     ))))

;; test the `rope` macro.
(define (t2)
  (assert (string=? "abcdef" (rope->string (rope "a" "b" "c" "d" "e" "f")))))

(t0)
(t1)
(t2)

