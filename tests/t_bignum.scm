;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "demo/bignum.scm")

(define (big-init bits)
  (define biggest-power-of-ten
    0 a -> a
    n a -> (biggest-power-of-ten (/ n 10) (+ 1 a))
    )
  (assert (= 0 (remainder bits 4)))
  (set! big/bits bits)
  (set! big/base (<< 1 bits))
  (set! big/halfbits (/ bits 2))
  (set! big/mask (- (<< 1 big/bits) 1))
  (set! big/halfmask (- (<< 1 big/halfbits) 1))
  (set! big/repr-width (/ bits 4))
  (set! big/decimal-pad (biggest-power-of-ten big/base -1))
  (set! big/decimal-base (pow 10 big/decimal-pad))
  (printf "bignum: using " (int bits) "-bit limbs.\n")
  )

(define (check-platform)
  (match (get-word-size) with
    8 -> (big-init 56)
    4 -> (big-init 28)
    _ -> (raise (:StrangePlatform))
    ))

;; ---------- testing -----------

(defmacro assert
  (assert x) -> (if x #u (raise (:AssertError)))
  )

(define (test0)
  (big-init 4)
  (assert (string=? (big-repr (big:zero)) "B0"))
  (assert (string=? (big-repr (int->big #x314159)) "B+3.1.4.1.5.9"))

  (printf "addition\n")

  (assert (string=? "B+3.1.4.1.5.a" (big-repr (big-add (int->big #x314159) (int->big 1)))))
  (assert (string=? "B+6.2.8.2.b.2" (big-repr (big-add (int->big #x314159) (int->big #x314159)))))
  (assert (string=? "B+f.0" (big-repr (big-add (int->big #xef) (int->big #x1)))))
  (assert (string=? "B+b.e.f.0" (big-repr (big-add (int->big #xbeef) (int->big #x1)))))
  (assert (string=? "B+1.0.0.0.0" (big-repr (big-add (int->big #xffff) (int->big #x1)))))

  (printf "big<\n")
  (assert (not (big< (int->big 0) (int->big 0))))
  (assert (big< (int->big 0) (int->big 1)))
  (assert (not (big< (int->big 1) (int->big 0))))
  (assert (not (big< (int->big 1) (int->big 1))))
  (assert (not (big< (int->big #x1000) (int->big #x300))))
  (assert (eq? #f (big< (int->big #x1000) (int->big #x300))))
  (assert (eq? #t (big< (int->big #x300) (int->big #x1000))))

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
  (big-init 28)
  (let ((counter 0))
    (for-range
	i stop
	;; (printf "\ni= " (int i) " : ")
	(for-range
	    j stop
	    ;; (printf (int j) " ")
	    ;; (flush)
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

(define (test-pow)
  (let ((r (big-pow (int->big 2) 255)))
    ;;(printf "2^255 = " (big->dec r) "\n")
    (printf "2^255 = " (big-repr r) "\n")
    (printf "2^255-19 = " (big-repr (big (- (pow (I 2) 255) (I 19)))) "\n")
    ))

(define (test-dec->big2)
  (let ((n0 (big-pow (int->big 2) 113))
        (n1 (dec->big "10384593717069655257060992658440192")))
    (printn n0)
    (printn n1)
    (printf (big-repr n0) "\n")
    (printf (big-repr n1) "\n")
    (assert (big= n0 n1))
    (assert (big= (dec->big "-324982934234") (big-negate (dec->big "324982934234"))))
    ))

(define (test-dec->big)
  (let ((n1 (dec->big "10384593717069655257060992658440192")))
    (printf (big-repr n1) "\n")
    ))

(define (test-big-odd?)
  (let ((n0 (big-pow (int->big 2) 113))
        (n1 (big-add n0 (int->big 1))))
    (assert (eq? #f (big-odd? n0)))
    (assert (eq? #t (big-odd? n1)))
    ))

(define (test-exp-mod)
  (let ((n0 (big-sub (big-pow (int->big 2) 255) (int->big 19))))
    (assert (big= (big-exp-mod n0 (int->big 23) (int->big 19)) (int->big 12)))))

(define (test-div)
  (let ((n0 (big-sub (big-pow (int->big 2) 255) (int->big 19)))
        (n1 (dec->big "20394293842039843948398394"))
        ((quo rem) (big-div n0 n1)))
    (printf "quo " (big->dec quo) "\n")
    (printf "rem " (big->dec rem) "\n")
    ))

(define (test-divschool0)
  (let ((a (dec->big "938429382929393234239482398329348293423"))
        (b (dec->big "139823948293842938429384293842938429384")))
    (printf "a: " (big-repr a) "\n")
    (printf "b: " (big-repr b) "\n")
    (match a b with
      (big:pos da) (big:pos db)
      -> (let (((q r) (divschool0 da db)))
           (printf "q: " (join "." (digits-repr q)) "\n"
                   "r: " (join "." (digits-repr r)) "\n"))
      _ _ -> (impossible)
      )))

(define (test-divschool2)
  (let ((a (dec->big "938429382929393234239482398329348293423"))
        (b (dec->big "139823948293842939384")))
    (printf "a: " (big-repr a) "\n")
    (printf "b: " (big-repr b) "\n")
    (match a b with
      (big:pos da) (big:pos db)
      -> (let (((q r) (divschool2 da db)))
           (printf "q: " (join "." (digits-repr q)) "\n"
                   "r: " (join "." (digits-repr r)) "\n"))
      _ _ -> (impossible)
      )))

(define (test-divschool2-2)
  (let ((a (dec->big "93856334684490212823810501753001496597211646236962549453697696969742360550032"))
        (b (dec->big "753001496597211646236")))
    (printf "a: " (big-repr a) "\n")
    (printf "b: " (big-repr b) "\n")
    (match a b with
      (big:pos da) (big:pos db)
      -> (let (((q r) (divschool2 da db)))
           (printf "q: " (join "." (digits-repr q)) "\n"
                   "r: " (join "." (digits-repr r)) "\n"))
      _ _ -> (impossible)
      )))

(define (F n)
  (format (zpad big/repr-width (hex n))))

(define (test-mul2n1)
  (let ((a1 #x1234567)
        (a0 #x89abcde)
        (b0 #xcafef00)
        ((x y z) (mul2n1 a1 a0 b0)))
    (printf "x " (F x) " y " (F y) " z " (F z) "\n")
    ))

;; cf80cd8aed482d5d1527d7dc72fceff84e6326592848447d2dc0b0e87dfc9a90
(define (test-mul2)
  (let ((a #x123456789abcde)
        ((r1 r0) (mul2 a a))
        (x #xcf80cd8aed482d)
        (y #x5d1527d7dc72fc)
        ((r3 r2) (mul2 x y)))
    (printf "r1 " (F r1) " r0 " (F r0) "\n")
    (printf "r3 " (F r3) " r2 " (F r2) "\n")
    ))

(define (test-mul)
  (let ((a (int->big #x174876e800))
        (b (int->big #x3de5a1eb)))
    (printf "a: " (big-repr a) "\n")
    (printf "b: " (big-repr b) "\n")
    (printf "c: " (big-repr (big-mul b a)) "\n")
    (assert (big= (big-mul b a) (big-mul a b)))
    ))

(define (test-mul-2)
  (let ((x (dec->big "46316835694926478169428394003475163141307993866256225615783033603165251855960")))
    (printf "x   " (big-repr x) "\n")
    (printf "x*x " (big-repr (big-mul x x)) "\n")))

(define (test-add)
  (let ((a (big:pos (list 58656410045163520 144115187987555)))
        (b (big:pos (list 1038459371))))
    (printf "a:  " (big-repr a) "\n")
    (printf "b:  " (big-repr b) "\n")
    (printf "a+b " (big-repr (big-add a b)) "\n")
    ))

(define (test-digits-lshift)
  (let ((n0 (dec->big "938429382929393234239482398329348293423"))
        (digs0 (big->digits n0)))
    (define (DO bits)
      (printf "<< " (lpad 2 (int bits)) " "
              (big-repr (big:pos (digits-lshift digs0 bits))) "\n"))
    (for-list bits (list 8 9 10 48 52 64 84)
      (DO bits))))

(define (test-digits-rshift)
  (let ((n0 (dec->big "938429382929393234239482398329348293423"))
        (digs0 (big->digits n0)))
    (define (DO bits)
      (printf ">> " (lpad 2 (int bits)) " "
              (big-repr (big:pos (digits-rshift digs0 bits))) "\n"))
    (printf "      " (big-repr n0) "\n")
    (for-list bits (list 8 9 10 48 52 64 84)
      (DO bits))))

(define (test-rshift)
  (let ((n0 (dec->big "938429382929393234239482398329348293423")))
    (printf "n      " (big-repr n0) "\n")
    (printf "n >> 1 " (big-repr (big-rshift n0 1)) "\n")
    ))

(define (test-power-of-two?)
  (define (T n)
    (match (power-of-two? n 1) with
      (maybe:yes s) -> s
      (maybe:no)    -> 0
      ))
  (assert (= 14 (T 16384)))
  (assert (= 0  (T 16383)))
  (assert (= 16 (T 65536)))
  (assert (= 0  (T 65537)))
  )

(define (test-take-drop)
  (let ((x (list 0 1 2 3 4 5 6 7 8)))
    (printn (take x 4))
    (printn (drop x 4))
    ))

(define (test-mod)
  (let ((q (big (- (pow (I 2) 255) (I 19))))
        (q2 (big-mul q q)))
    (printf "q+1%q " (big-repr (big (mod (+ (I 1) q) q))) "\n")
    (printf "q^2   " (big-repr (big q2)) "\n")
    (printf "q^2%q " (big-repr (big (mod q2 q))) "\n")
    (printf "1%q   " (big-repr (big (mod (I 1) q))) "\n")
    ))

(define (test-1div)
  (let ((x (big (dec "982394829348234234")))
        ((q r) (big-div (int->big 1) x)))
    (printf "q " (big-repr q) "\n")
    (printf "r " (big-repr r) "\n")
    ))

;(test-rshift)
;(test-mul-2)

;(test-mod)
;(test-1div)

;(test-big-init)

;(test-take-drop)
;(test-digits-lshift)
;(test-digits-rshift)
;(test-power-of-two?)
;;(test-mul2n1)
;(test-mul2)
;(test-divschool0)
;(test-divschool2)
;(test-divschool2-2)
;(test-pow)
;(printf (big-repr (big (+ (pow (I 2) 252) (dec "27742317777372353535851937790883648493")))) "\n")

;; XXX need some randomized testing of mul & div.
;; if we use a deterministic RNG we can test against
;;   python for correctness.

;; 16777216 tests
;(exhaustive #x1000)

;; these tests require base16 (for big-repr)
;(when (= big/base 16)
;      (test0)
;      (test1))

;; 65536 tests
(exhaustive #x100)
;(test0)
;(test2)
;(test3)
;(test4)
;(printf "(big-fact 100) =>\n" (big->dec (big-fact 100)) "\n")
;(printf "(big-fact 100) =>\n" (big-repr (big-fact 100)) "\n")
;(printf "(big-fact 1000) =>\n" (big-repr (big-fact 1000)) "\n")
;(printf "(big-fact 1000) =>\n" (big->dec (big-fact 1000)) "\n")
;(test-mul)
;(test-dec->big)
;(test-dec->big2)
;(test-add)
;(test-big-odd?)
;(test-exp-mod)
;(test-div)

(define (test-expmod-1)
  (let ((b (int->big 121666))
        (e (dec->big "57896044618658097711785492504343953926634992332820282019728792003956564819947"))
        (m (dec->big "57896044618658097711785492504343953926634992332820282019728792003956564819949")))
    (printf "= " (big-repr (big-exp-mod b e m)) "\n")
    ))

;(test-expmod-1)

(define (test-div-0)
  (let ((xx (big:pos
             #(#x6 #xdaaf3e78ce9b06 #x601452d3bfbab6 #xd919633990c300 #x39d680a4a4c97c
                   #x3f3e6f53141949 #x76d81951e69332 #x39be6597777794 #x5487f21094366a
                   #xaae13040000000)))
        (m (big:pos #(#x7fffffff #xffffffffffffff #xffffffffffffff #xffffffffffffff #xffffffffffffed)))
        ((q r) (big-div xx m)))
    (printf "xx " (big-repr xx) "\n"
            " m " (big-repr m) "\n"
            " q " (big-repr q) "\n"
            " r " (big-repr r) "\n")
    ))

;(test-div-0)
