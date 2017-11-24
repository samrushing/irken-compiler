;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(datatype big
  (:zero)
  (:pos (list int))
  (:neg (list int))
  )

;; NOTE: multiply and divide algorithms require that
;;  the base be half the word size.

;; XXX revisit this to figure out the exact digit size
;;  we can use on 64-bit.  Because obviously 32 and 64
;;  bit shouldn't be using the same size.

;; for 64-bit platforms:
(define big/base #x20000000)
(define big/repr-width 8)
;; largest power of ten that fits in a digit.
(define big/decimal-base 100000000)
(define big/decimal-pad 8)

(define (go32)
  (set! big/base #x20000000)
  (set! big/repr-width 8)
  (set! big/decimal-base 100000000)
  (set! big/decimal-pad 8)
  )

(define (go16)
  ;; I think it's the division algorithm that is placing
  ;;  such drastic limits on the base here.  Anything larger
  ;;  breaks it.  Needs investigation
  (printf "bignum: using 16-bit limbs.\n")
  (set! big/base #x1000)
  (set! big/repr-width 3)
  (set! big/decimal-base 100)
  (set! big/decimal-pad 2)
  )

;; base16, for testing using relatively small numbers.
(define (go8)
  (printf "bignum: using hex-digit limbs.\n")
  (set! big/base #x10)
  (set! big/repr-width 1)
  (set! big/decimal-base 100)
  (set! big/decimal-pad 2)
  )

(define (check-platform)
  (match (get-word-size) with
    8 -> (go32)
    4 -> (go16)
    _ -> (raise (:StrangePlatform))
    ))

(check-platform)

;; for testing, use hexadecimal digits

(define (digits-repr digs)
  (let loop ((digs digs) (acc '()))
    (match digs with
      () -> acc
      (hd . tl) -> (loop tl (list:cons (format (zpad big/repr-width (hex hd))) acc))
      )))

(define big-repr
  (big:zero) -> "B0"
  (big:pos digits) -> (format "B+" (join id "." (digits-repr digits)))
  (big:neg digits) -> (format "B-" (join id "." (digits-repr digits)))
  )

;; canonicalize the results of a subtraction
;; Note: the list of digits is in MSB form here.
(define remove-zeros
  (0 . tl) -> (remove-zeros tl)
  x	   -> x
  )

;; in standard LSB form
(define (canon x)
  (reverse (remove-zeros (reverse x))))

;; ---------- addition -----------

(define (digits-add0 a b acc carry?)
  (match a b with
    () ()   -> (reverse (if carry? (list:cons 1 acc) acc))
    () digs -> (digits-add0 (LIST 0) digs acc carry?)
    digs () -> (digits-add0 (LIST 0) digs acc carry?)
    (d0 . tl0) (d1 . tl1)
    -> (let ((sum (+ d0 d1 (if carry? 1 0))))
	 (if (>= sum big/base)
	     (digits-add0 tl0 tl1 (list:cons (- sum big/base) acc) #t)
	     (digits-add0 tl0 tl1 (list:cons sum acc) #f)))
    ;;_ _ -> (error "matching is borken?")
    ))

(define (digits-add a b)
  (digits-add0 a b '() #f))

;; this will fail if either list is non-canonical
;;  (i.e. contains zero padding).
(define (digits-cmp da db)
  (let ((na (length da))
	(nb (length db)))
    (cond ((< na nb) (cmp:<)) ;; aa < bbbb
	  ((> na nb) (cmp:>)) ;; aaaa > bb
	  (else
	   (let loop ((da (reverse da))
		      (db (reverse db)))
	     ;; compare most-significant digit by digit...
	     (cond ((null? da) (cmp:=))
		   ((< (car da) (car db)) (cmp:<))
		   ((> (car da) (car db)) (cmp:>))
		   (else
		    (loop (cdr da) (cdr db)))))))))

(define (digits-<? da db)
  (eq? (digits-cmp da db) (cmp:<)))

(define big-add
  (big:zero) x              -> x
  x (big:zero)              -> x
  (big:pos da) (big:pos db) -> (big:pos (digits-add da db))
  (big:pos da) (big:neg db) -> (big-sub (big:pos da) (big:pos db))
  (big:neg da) (big:neg db) -> (big:neg (digits-add da db))
  (big:neg da) (big:pos db) -> (big-sub (big:pos db) (big:pos da))
  )

;; ---------- subtraction -----------

;; used for borrowing
(define digits-sub1
  () -> (raise (:UnderflowError))
  (1) -> '()
  (0 . tl) -> (list:cons (- big/base 1) (digits-sub1 tl))
  (n . tl) -> (list:cons (- n 1) tl)
  )

;; assumes b < a
(define (digits-sub0 a b acc)
  (match a b with
    () ()   -> (reverse (remove-zeros acc))
    () digs -> (raise (:UnderflowError))
    digs () -> (append (reverse acc) digs)
    (d0 . tl0) (d1 . tl1)
    -> (let ((diff (- d0 d1)))
	 (if (< diff 0)
	     (digits-sub0 (digits-sub1 tl0) tl1 (list:cons (+ big/base diff) acc))
	     (digits-sub0 tl0 tl1 (list:cons diff acc))))
    ))

(define (digits-sub a b)
  (digits-sub0 a b '()))

(define (digits-sub-mag da db)
  (match (digits-cmp da db) with
    (cmp:>) -> (big:pos (digits-sub da db))
    (cmp:<) -> (big:neg (digits-sub db da))
    (cmp:=) -> (big:zero)))

(define big-sub
  (big:zero) x              -> (big-negate x)
  x (big:zero)              -> x
  (big:pos da) (big:pos db) -> (digits-sub-mag da db)
  (big:pos da) (big:neg db) -> (big-add (big:pos da) (big:pos db))
  (big:neg da) (big:neg db) -> (big-negate (big-add (big:pos da) (big:pos db)))
  (big:neg da) (big:pos db) -> (big-negate (big-add (big:pos da) (big:pos db)))
  )

;; ---------- utility -----------

(define big-negate
  (big:zero) -> (big:zero)
  (big:pos x) -> (big:neg x)
  (big:neg x) -> (big:pos x)
  )

(define big-<?
  (big:zero)  (big:zero)  -> #f
  (big:zero)  (big:pos _) -> #t
  (big:zero)  (big:neg _) -> #f
  (big:pos _) (big:zero)  -> #f
  (big:neg _) (big:zero)  -> #t
  (big:pos _) (big:neg _) -> #f
  (big:neg _) (big:pos _) -> #t
  (big:pos a) (big:pos b) -> (eq? (digits-cmp a b) (cmp:<))
  (big:neg a) (big:neg b) -> (eq? (digits-cmp a b) (cmp:>))
  )

;; assumes positive n
(define int->digits
  0 acc -> (reverse acc)
  n acc -> (int->digits
	    (/ n big/base)
	    (list:cons (mod n big/base) acc))
  )

(define (int->big n)
  (if (zero? n)
      (big:zero)
      (let ((pos? (>= n 0))
	    (absn (if pos? n (- 0 n))))
	(if pos?
	    (big:pos (int->digits absn '()))
	    (big:neg (int->digits absn '()))))))

(define big=
  (big:zero)  (big:zero)  -> #t
  (big:zero)  _		  -> #f
  _ (big:zero)            -> #f
  (big:pos _) (big:neg _) -> #f
  (big:neg _) (big:pos _) -> #f
  (big:pos a) (big:pos b) -> (eq? (digits-cmp a b) (cmp:=))
  (big:neg a) (big:neg b) -> (eq? (digits-cmp a b) (cmp:=))
  )

(define (add-zeros digs n)
  (append digs (n-of n 0)))

(define (shift digs n)
  (append (n-of n 0) digs))

;; ---------- multiplication -----------

(define (karatsuba da db)

  ;; http://www.keithschwarz.com/interesting/code/karatsuba/Karatsuba.python.html

  (define (add a b)
    (canon (digits-add a b)))

  (define (sub a b)
    (canon (digits-sub a b)))

  (define (pad-to digs n)
    (let ((len (length digs)))
      (if (< len n)
	  (add-zeros digs (- n len))
	  digs)))

  (define K
    () _  -> (LIST 0)
    _ ()  -> (LIST 0)
    (a) (b) -> (let ((prod (* a b))
		     (r1 (/ prod big/base))
		     (r0 (remainder prod big/base)))
		 (LIST r0 r1))
    a b -> (let ((n (max (length a) (length b)))
		 (x (pad-to a n))
		 (y (pad-to b n))
		 (n0 (/ (+ 1 n) 2))
		 (n1 (/ n 2))
		 (x0 (slice x n1 n))
		 (x1 (slice x 0 n1))
		 (y0 (slice y n1 n))
		 (y1 (slice y 0 n1))
		 (p0 (K x0 y0))
		 (p1 (K (add x0 x1) (add y0 y1)))
		 (p2 (K x1 y1))
		 (z0 p0)
		 (z1 (sub p1 (add p0 p2)))
		 (z2 p2)
		 (z0prod (shift z0 (* 2 n1)))
		 (z1prod (shift z1 n1))
		 (z2prod z2)
		 )
	     (add (add z0prod z1prod) z2prod)))

  (canon (K da db))

  )

;; multiply by a single digit
(define digits-mul1
  ()        n carry acc -> (reverse (remove-zeros (list:cons carry acc)))
  (d0 . tl) n carry acc
  -> (let ((next (+ carry (* n d0)))
	   (quo (/ next big/base))
	   (rem (remainder next big/base)))
       (digits-mul1 tl n quo (list:cons rem acc))))

;; grade-school algorithm
(define (digits-mul-school x y)
  (define recur
           () n acc -> acc
    (y0 . tl) n acc
    -> (recur
	tl (+ n 1)
	(digits-add
	 acc
	 (shift (digits-mul1 x y0 0 '()) n)))
    )
  (canon (recur y 0 '())))

;; TODO: compute this.
(define KARATSUBA-CUTOFF 10)

(define digits-mul
  () _  -> (LIST 0)  ;; these handle internal
  _  () -> (LIST 0)  ;;   results of other algorithms
  x  (one) -> (digits-mul1 x one 0 '())
  (one) x  -> (digits-mul1 x one 0 '())
  x y      -> (let ((lx (length x))
		    (ly (length y)))
		(if (and (< lx KARATSUBA-CUTOFF) (< ly KARATSUBA-CUTOFF))
		    (digits-mul-school x y)
		    (karatsuba x y)))
  )

(define big-mul
  (big:zero) x		    -> (big:zero)
  x (big:zero)              -> (big:zero)
  (big:pos da) (big:pos db) -> (big:pos (digits-mul da db))
  (big:neg da) (big:neg db) -> (big:pos (digits-mul da db))
  (big:pos da) (big:neg db) -> (big:neg (digits-mul da db))
  (big:neg da) (big:pos db) -> (big:neg (digits-mul da db))
  )

;; ---------- division -----------

;; Burnikel-Ziegler:
;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.47.565&rep=rep1&type=pdf
;; http://damien-guichard.developpez.com/tutoriels/ocaml/?page=page_6

;; divide by a single digit (grade school algorithm)
;; NOTE: dx in MSB order, output in LSB order
(define (digits-div1 dx n)
  ;;(printf "div1 " (join int->string " " dx) " n= " (int n ) "\n")
  (define div1
    ()        carry acc -> (:tuple (canon acc) (int->digits carry '()))
    (d0 . tl) carry acc
    -> (let ((v (+ d0 (* carry big/base)))
	     (q (/ v n))
	     (r (mod v n))
	     (x 0)
	     )
	 (div1 tl r (list:cons q acc))))
  (div1 dx 0 '())
  )

(define (burnzieg da db)

  (let ((la (length da))
	(lb (length db)))
    (if (<= lb 2)
	(let ((b2 (match db with
		    (b0) -> b0
		    (b0 b1) -> (+ (* b0 big/base) b1)
		    _ -> (impossible))))
	  (digits-div1 da b2))
	(let ((n (/ (- lb 1) 2))
	      (a0 (slice da (- la n) la))
	      (a1 (slice da 0 (- la n))))
	  (if (digits-<? (reverse db) (reverse a1))
	      ;; simple case
	      (let (((q1 r1) (burnzieg a1 db))
                    ((q0 r0) (burnzieg
                              (reverse (digits-add (shift r1 n) (reverse a0)))
                              db)))
		(:tuple (digits-add (shift q1 n) q0) r0))
	      ;; remainder check case
	      (let ((b0 (slice db (- lb n) lb))
		    (b1 (slice db 0 (- lb n)))
                    ((q1 r1) (burnzieg a1 b1))
                    (a0pr1 (digits-add (shift r1 n) (reverse a0)))
                    (b0xq1 (digits-mul (reverse b0) q1)))
                (if (not (digits-<? a0pr1 b0xq1))
                    (:tuple q1
                            (digits-sub a0pr1 b0xq1))
                    (:tuple (digits-sub q1 '(1))
                            (digits-sub (reverse db)
                                        (digits-sub b0xq1 a0pr1)))))
              )))
    ))

(define (digits->big x pos?)
  (match (canon x) pos? with
    () _ -> (big:zero)
    y  #t -> (big:pos y)
    y  #f -> (big:neg y)))

(define (digits-div da db pos?)
  ;; XXX test for zero
  (match (burnzieg (reverse da) (reverse db)) with
    (:tuple quo rem)
    -> (:tuple (digits->big quo pos?) (digits->big rem #t))))

(define big-div
  x            (big:zero)   -> (raise (:ZeroDivisionError))
  (big:zero)   x            -> (:tuple (big:zero) x)
  (big:pos da) (big:pos db) -> (digits-div da db #t)
  (big:neg da) (big:neg db) -> (digits-div da db #t)
  (big:pos da) (big:neg db) -> (digits-div da db #f)
  (big:neg da) (big:pos db) -> (digits-div da db #f)
  )

;; ---------- interface -----------

(define (pair->dec p pad?)
  (let ((n (match p with
	     ()    -> 0
	     (a)   -> a
	     (a b) -> (+ (* b big/base) a)
	     _ -> (impossible))))
    (if pad?
	(format (zpad big/decimal-pad (int n)))
	(format (int n)))))

(define (digits->dec dn)
  (let ((parts
	 (let loop ((dn0 (reverse dn)) (acc '()))
	   (match (digits-div1 dn0 big/decimal-base) with
	     (:tuple quo rem)
	     -> (if (null? quo)
		    (list:cons (pair->dec rem #f) acc)
		    (loop (reverse quo)
			  (list:cons (pair->dec rem #t) acc)))
	     ))))
    (format (join id "" parts))))

(define big->dec
  (big:zero) -> "0"
  (big:pos dn) -> (digits->dec dn)
  _ -> (raise (:NotImplementedError))
  )

;; ---------- testing -----------

(defmacro assert
  (assert x) -> (if x #u (raise (:AssertError)))
  )

(define (test0)
  (go8)
  (assert (string=? (big-repr (big:zero)) "B0"))
  (assert (string=? (big-repr (int->big #x314159)) "B+3.1.4.1.5.9"))

  (printf "addition\n")

  (assert (string=? "B+3.1.4.1.5.a" (big-repr (big-add (int->big #x314159) (int->big 1)))))
  (assert (string=? "B+6.2.8.2.b.2" (big-repr (big-add (int->big #x314159) (int->big #x314159)))))
  (assert (string=? "B+f.0" (big-repr (big-add (int->big #xef) (int->big #x1)))))
  (assert (string=? "B+b.e.f.0" (big-repr (big-add (int->big #xbeef) (int->big #x1)))))
  (assert (string=? "B+1.0.0.0.0" (big-repr (big-add (int->big #xffff) (int->big #x1)))))

  (printf "big-<?\n")
  (assert (not (big-<? (int->big 0) (int->big 0))))
  (assert (big-<? (int->big 0) (int->big 1)))
  (assert (not (big-<? (int->big 1) (int->big 0))))
  (assert (not (big-<? (int->big 1) (int->big 1))))
  (assert (not (big-<? (int->big #x1000) (int->big #x300))))
  (assert (eq? #f (big-<? (int->big #x1000) (int->big #x300))))
  (assert (eq? #t (big-<? (int->big #x300) (int->big #x1000))))

  (printf "digits-sub1\n")
  (assert (string=? "15 15 15" (format (join int->string " " (digits-sub1 '(0 0 0 1))))))

  ;; subtraction
  (printf "subtraction\n")

  (assert (string=? "B0" (big-repr (big-sub (int->big 1) (int->big 1)))))
  (assert (string=? "B+5" (big-repr (big-sub (int->big 10) (int->big 5)))))
  (assert (string=? "B+1.2.0.0" (big-repr (big-sub (int->big #x1234) (int->big #x34)))))
  (assert (string=? "B+a.a.a.e.e" (big-repr (big-sub (int->big #xAAAFF) (int->big #x11)))))
  (assert (string=? "B-1.1" (big-repr (big-sub (int->big #x0) (int->big #x11)))))
  (assert (string=? "B+f" (big-repr (big-sub (int->big #x10) (int->big #x1)))))
  (assert (string=? "B-f" (big-repr (big-sub (int->big #x1) (int->big #x10)))))
  (assert (string=? "B+1.1" (big-repr (big-sub (int->big #x12) (int->big #x1)))))
  (assert (string=? "B+1.0" (big-repr (big-sub (int->big #x12) (int->big #x2)))))
  (assert (string=? "B+1.0" (big-repr (big-sub (int->big #x13) (int->big #x3)))))
  (assert (string=? "B+1.0" (big-repr (big-sub (int->big #x11) (int->big #x1)))))
  (assert (string=? "B+f.f.f.f" (big-repr (big-sub (int->big #x10000) (int->big #x1)))))
  (assert (string=? "B+1.0.0.0.f.f.f" (big-repr (big-sub (int->big #x1001000) (int->big #x1)))))

  (printf "subtraction - negative result\n")
  (assert (string=? "B-1.0" (big-repr (big-sub (int->big #x1) (int->big #x11)))))
  (assert (string=? "B-1.1.0.0" (big-repr (big-sub (int->big #x11) (int->big #x1111)))))

  (printf "larger numbers\n")
  (assert (string=? "B+d.e.a.d.b.e.f.0" (big-repr (big-add (int->big #xdeadbeef) (int->big 1)))))
  (check-platform)
  )

;; exhaustive testing of add & sub

(define (exhaustive stop)
  (go16)
  (let ((counter 0))
    (for-range
	i stop
	(printf "\ni= " (int i) " : ")
	(for-range
	    j stop
	    (printf (int j) " ")
	    (flush)
	    (let ((a (int->big (+ i j)))
		  (b (big-add (int->big i) (int->big j)))
		  (c (int->big (+ (- 0 i) j)))
		  (d (big-add (int->big (- 0 i)) (int->big j)))
		  (e (int->big (- i j)))
		  (f (big-sub (int->big i) (int->big j)))
		  (g (int->big (- (- i) j)))
		  (h (big-sub (int->big (- i)) (int->big j)))
		  )
	      (try
	       (begin
		 (assert (big= a b))
		 (assert (big= c d))
		 (assert (big= e f))
		 (assert (big= g h))
		 (set! counter (+ counter 1)))
	       except
	       (:AssertError)
	       -> (printf "failed with i=" (int i) " j=" (int j) "\n"
			  (big-repr a) " = " (big-repr b) "\n"
			  (big-repr c) " = " (big-repr d) "\n"
			  )
	       )
	      )))
    (printf (int counter) " tests passed.\n")
    )
  (check-platform)
  )

(define (big-fact n)
  (define recur
    1 acc -> acc
    n acc -> (recur (- n 1) (big-mul acc (int->big n)))
    )
  (recur n (int->big 1))
  )

(define (test1)
  (define hex-map
    (literal
     (alist/make
      (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)
      (#\a 10) (#\b 11) (#\c 12) (#\d 13) (#\e 14) (#\f 15)
      (#\A 10) (#\B 11) (#\C 12) (#\D 13) (#\E 14) (#\F 15)
      )))

  (define (read-hex-digit ch)
    (match (alist/lookup hex-map ch) with
      (maybe:no) -> (error "bad hex digit")
      (maybe:yes num) -> num))

  ;; assumes base 16!
  (define (hex->big s)
    (define recur
      () acc -> (big:pos acc)
      (hd . tl) acc -> (recur tl (list:cons (read-hex-digit hd) acc)))
    (recur (string->list s) '()))

  (printf
   (big-repr
    (big-mul
     (hex->big "eb2cb7a132097789dc60a2f4f09c56f51afa09c50e80c95d7b39b3e426690093")
     (int->big #xc)))
   "\n")
  )

(define (big-pow x n)
  (define pow
    (big:zero)  acc -> acc
    (big:neg _) acc -> (raise (:NotImplementedError))
    n           acc -> (pow (big-sub n (int->big 1)) (big-mul acc x))
    )
  (pow n x))

(define (testdiv a b)
  (match (big-div a b) with
    (:tuple quo rem)
    -> (begin
	 (printf "quo " (big-repr quo) "\n")
	 (printf "rem " (big-repr rem) "\n")
	 )))

(define (test2)
  (let (((quo rem) (big-div (big-fact 1000) (big-fact 999))))
    (assert (big= quo (int->big 1000)))
    (assert (big= rem (big:zero)))
    ))

(define (test3)
  (let ((x (big-fact 1000))
	(s "402387260077093773543702433923003985719374864210714632543799910429938512398629020592044208486969404800479988610197196058631666872994808558901323829669944590997424504087073759918823627727188732519779505950995276120874975462497043601418278094646496291056393887437886487337119181045825783647849977012476632889835955735432513185323958463075557409114262417474349347553428646576611667797396668820291207379143853719588249808126867838374559731746136085379534524221586593201928090878297308431392844403281231558611036976801357304216168747609675871348312025478589320767169132448426236131412508780208000261683151027341827977704784635868170164365024153691398281264810213092761244896359928705114964975419909342221566832572080821333186116811553615836546984046708975602900950537616475847728421889679646244945160765353408198901385442487984959953319101723355556602139450399736280750137837615307127761926849034352625200015888535147331611702103968175921510907788019393178114194545257223865541461062892187960223838971476088506276862967146674697562911234082439208160153780889893964518263243671616762179168909779911903754031274622289988005195444414282012187361745992642956581746628302955570299024324153181617210465832036786906117260158783520751516284225540265170483304226143974286933061690897968482590125458327168226458066526769958652682272807075781391858178889652208164348344825993266043367660176999612831860788386150279465955131156552036093988180612138558600301435694527224206344631797460594682573103790084024432438465657245014402821885252470935190620929023136493273497565513958720559654228749774011413346962715422845862377387538230483865688976461927383814900140767310446640259899490222221765904339901886018566526485061799702356193897017860040811889729918311021171229845901641921068884387121855646124960798722908519296819372388642614839657382291123125024186649353143970137428531926649875337218940694281434118520158014123344828015051399694290153483077644569099073152433278288269864602789864321139083506217095002597389863554277196742822248757586765752344220207573630569498825087968928162753848863396909959826280956121450994871701244516461260379029309120889086942028510640182154399457156805941872748998094254742173582401063677404595741785160829230135358081840096996372524230560855903700624271243416909004153690105933983835777939410970027753472000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"))
    (assert (string=? (big->dec x) s))
    ))

(define (test4)
  (let ((i 73)
	(j 186)
	(a (int->big (+ i j)))
	(b (big-add (int->big i) (int->big j)))
	(c (int->big (+ (- 0 i) j)))
	(d (big-add (int->big (- 0 i)) (int->big j)))
	(e (int->big (- i j)))
	(f (big-sub (int->big i) (int->big j)))
	(g (int->big (- (- i) j)))
	(h (big-sub (int->big (- i)) (int->big j)))
	)
    (assert (big= a b))
    (assert (big= c d))
    (assert (big= e f))
    (assert (big= g h))))


;; 16777216 tests
;(exhaustive #x1000)

;; these tests require base16 (for big-repr)
;(when (= big/base 16)
;      (test0)
;      (test1))

;; 65536 tests
;(exhaustive #x100)
;(test0)
(test2)
(test3)
;(test4)
(printf "(big-fact 1000) => \n" (big->dec (big-fact 1000)) "\n")


