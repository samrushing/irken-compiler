;; -*- Mode: Irken -*-

;; by default, construct in big-endian form.
;; [the main use of this will be for network data]

(datatype pbuf-ob
  (:t {buf=string pos=int len=int})
  )

;; -------------- packer --------------

(define (pbuf-class)

  (define (grow self)
    (let ((new-len (+ self.len (/ self.len 2)))
          (new-buf (make-string new-len)))
      (buffer-copy self.buf 0 self.pos new-buf 0)
      (set! self.len new-len)
      (set! self.buf new-buf)))

  (define (ensure self n)
    (while (> (+ self.pos n) self.len)
      (grow self)))

  (define (byte self n)
    (string-set! self.buf self.pos (ascii->char n))
    (set! self.pos (+ 1 self.pos)))

  ;; range check: lo <= n < hi
  (define (rcheck n lo hi)
    (if (and (>= n lo) (< n hi))
        #u
        (error1 "packer range check failed: "
                (format (int lo) " <= " (int n) " < " (int hi)))))

  (define (i8 self n)
    (rcheck n -128 128)
    (ensure self 1)
    (byte self (if (< n 0) (+ 256 n) n)))

  (define (u8 self n)
    (rcheck n 0 256)
    (ensure self 1)
    (byte self n))

  (define (bool8 self b)
    (ensure self 1)
    (byte self (if b 1 0)))

  (define (n16 self n)
    (ensure self 2)
    (byte self (logand #xff (>> n 8)))
    (byte self (logand #xff n))
    )

  (define (i16 self n)
    (rcheck n #x-8000 #x8000)
    (n16 self (if (< 0 n) (+ #x10000 n) n)))

  (define (u16 self n)
    (rcheck n 0 #x10000)
    (n16 self n))

  (define (n32 self n)
    (ensure self 4)
    (byte self (logand #xff (>> n 24)))
    (byte self (logand #xff (>> n 16)))
    (byte self (logand #xff (>> n 8)))
    (byte self (logand #xff n)))

  (define (i32 self n)
    (rcheck n #x-80000000 #x80000000)
    (n32 self (if (< 0 n) (+ #x100000000 n) n)))

  (define (u32 self n)
    (rcheck n 0 #x100000000)
    (n32 self n))

  (define (string self s)
    (ensure self (string-length s))
    (buffer-copy s 0 (string-length s) self.buf self.pos)
    (set! self.pos (+ (string-length s) self.pos))
    )

  (define (val self)
    (substring self.buf 0 self.pos))

  (define (pos self) self.pos)

  (define un (pbuf-ob:t self) -> self)

  (let ((methods
         {i8=i8 u8=u8 i16=i16 u16=u16 i32=i32 u32=u32
                string=string bool8=bool8
                un=un val=val pos=pos}))
    (lambda (size)
      {o=methods self=(pbuf-ob:t {buf=(make-string size) pos=0 len=size})})
    ))

(define pbuf/make (pbuf-class))

;; -------------- unpacker --------------

(define (ubuf-class)

  (define (ensure self n)
    (if (> (+ self.pos n) self.len)
        (raise (:UnpackUnderflow self))))

  (define (byte self)
    (let ((r (char->ascii (string-ref self.buf self.pos))))
      (set! self.pos (+ 1 self.pos))
      r))

  (define (i8 self)
    (ensure self 1)
    (let ((r (byte self)))
      (if (> r 127)
          (- r 256)
          r)))

  (define (u8 self)
    (ensure self 1)
    (byte self))

  (define (bool8 self)
    (ensure self 1)
    (if (= 0 (byte self)) #f #t))

  (define (u16 self)
    (ensure self 2)
    (let ((b0 (byte self))
          (b1 (byte self)))
      (logior (<< b0 8) b1)))

  (define (i16 self)
    (let ((n (u16 self)))
      (if (> n #x7fff) (- n #x10000) n)))

  (define (u32 self)
    (ensure self 4)
    (let ((b0 (byte self))
          (b1 (byte self))
          (b2 (byte self))
          (b3 (byte self)))
      (logior (<< b0 24) (logior (<< b1 16) (logior (<< b2 8) b3)))))

  (define (i32 self)
    (let ((n (u32 self)))
      (if (> n #x7fffffff) (- n #x100000000) n)))

  (define (string self n)
    (ensure self n)
    (let ((r (make-string n)))
      (buffer-copy self.buf self.pos n r 0)
      (set! self.pos (+ self.pos n))
      r))

  (define (pos self) self.pos)

  (define un (pbuf-ob:t self) -> self)

  (let ((methods
         {i8=i8 u8=u8 i16=i16 u16=u16 i32=i32 u32=u32
                string=string bool8=bool8
                un=un pos=pos}))
    (lambda (data)
      {o=methods self=(pbuf-ob:t {buf=data pos=0 len=(string-length data)})})
    ))

(define ubuf/make (ubuf-class))

;; --------------------------------------------------------------------------------
;; pack bits into an integer:
;; (packbits (width value) ...)
;; Note: it starts with the least significant bits first.

(defmacro bitmask
  (bitmask w n) -> (logand n (- (<< 1 w) 1)))

(defmacro pbits
  (pbits widths acc) -> acc
  (pbits widths acc (width val) rest ...)
  -> (pbits (+ width widths)
            (logior acc (<< (bitmask width val) widths))
            rest ...)
  )

(defmacro packbits
  (packbits item ...)
  -> (pbits 0 0 item ...))

;; unpackbits takes a spec like this:
;; (unpackbits value (width location) ...)
;; and transforms it into:
;; (begin
;;   (set! location (bitmask width val))
;;   (>> val width))
;; then recurses.

(defmacro unpackbits
  (unpackbits val) -> val
  (unpackbits val (width loc) rest ...)
  -> (unpackbits
      (begin (set! loc (bitmask width val))
             (>> val width))
      rest ...)
  )
