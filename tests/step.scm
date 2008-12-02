(define (gen-finals)
  #(
  #f
  #f
  whitespace
  #f
  #f
  comment
  newline
  #f
  #f
  string1
  #f
  #f
  mulop
  augassign
  punctuation
  #f
  #f
  string2
  #f
  #f
  punctuation
  mulop
  power
  addop
  addop
  mulop
  mulop
  number
  compare
  shift
  compare
  punctuation
  compare
  compare
  compare
  shift
  ident
  ident
  punctuation
  ident
  ident
  keyword
  ident
  ident
  ident
  ident
  keyword
  ident
  ident
  keyword
  ident
  ident
  ident
  keyword
  ident
  keyword
  ident
  ident
  keyword
  ident
  keyword
  keyword
  keyword
  ident
  ident
  keyword
  ident
  ident
  ident
  keyword
  ident
  keyword
  ident
  ident
  ident
  ident
  keyword
  ident
  ident
  ident
  ident
  keyword
  punctuation
  ))
(define (step ch st)
  (case st
((0)
 (cond
   ((eq? ch 124) 82)
   ((eq? ch 121) 77)
   ((eq? ch 119) 72)
   ((eq? ch 116) 66)
   ((eq? ch 110) 63)
   ((eq? ch 105) 59)
   ((eq? ch 102) 56)
   ((eq? ch 101) 50)
   ((eq? ch 100) 47)
   ((eq? ch 99) 42)
   ((eq? ch 97) 39)
   ((eq? ch 94) 38)
   ((eq? ch 62) 33)
   ((eq? ch 61) 31)
   ((eq? ch 60) 28)
   ((sym-test-0 ch) 27)
   ((eq? ch 47) 25)
   ((eq? ch 45) 24)
   ((eq? ch 43) 23)
   ((eq? ch 42) 21)
   ((eq? ch 39) 15)
   ((eq? ch 38) 14)
   ((eq? ch 37) 12)
   ((eq? ch 35) 3)
   ((eq? ch 34) 7)
   ((eq? ch 10) 6)
   ((sym-test-1 ch) 2)
   ((sym-test-2 ch) 1)
   ((sym-test-3 ch) 20)
   (else 36)
  )
 )
((1)
 (cond
   (else 1)
  )
 )
((2)
 (cond
   ((eq? ch 35) 3)
   ((sym-test-1 ch) 2)
   (else 1)
  )
 )
((3)
 (cond
   ((eq? ch 10) 5)
   (else 4)
  )
 )
((4)
 (cond
   ((eq? ch 10) 5)
   (else 4)
  )
 )
((5)
 (cond
   (else 1)
  )
 )
((6)
 (cond
   (else 1)
  )
 )
((7)
 (cond
   ((eq? ch 92) 10)
   ((eq? ch 34) 9)
   ((eq? ch 10) 1)
   (else 8)
  )
 )
((8)
 (cond
   ((eq? ch 92) 10)
   ((eq? ch 34) 9)
   ((eq? ch 10) 1)
   (else 8)
  )
 )
((9)
 (cond
   (else 1)
  )
 )
((10)
 (cond
   (else 11)
  )
 )
((11)
 (cond
   ((eq? ch 92) 10)
   ((eq? ch 34) 9)
   ((eq? ch 10) 1)
   (else 8)
  )
 )
((12)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((13)
 (cond
   (else 1)
  )
 )
((14)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((15)
 (cond
   ((eq? ch 92) 18)
   ((eq? ch 39) 17)
   ((eq? ch 10) 1)
   (else 16)
  )
 )
((16)
 (cond
   ((eq? ch 92) 18)
   ((eq? ch 39) 17)
   ((eq? ch 10) 1)
   (else 16)
  )
 )
((17)
 (cond
   (else 1)
  )
 )
((18)
 (cond
   (else 19)
  )
 )
((19)
 (cond
   ((eq? ch 92) 18)
   ((eq? ch 39) 17)
   ((eq? ch 10) 1)
   (else 16)
  )
 )
((20)
 (cond
   (else 1)
  )
 )
((21)
 (cond
   ((eq? ch 61) 13)
   ((eq? ch 42) 22)
   (else 1)
  )
 )
((22)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((23)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((24)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((25)
 (cond
   ((eq? ch 61) 13)
   ((eq? ch 47) 26)
   (else 1)
  )
 )
((26)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((27)
 (cond
   ((sym-test-0 ch) 27)
   (else 1)
  )
 )
((28)
 (cond
   ((eq? ch 61) 30)
   ((eq? ch 60) 29)
   (else 1)
  )
 )
((29)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((30)
 (cond
   (else 1)
  )
 )
((31)
 (cond
   ((eq? ch 61) 32)
   (else 1)
  )
 )
((32)
 (cond
   (else 1)
  )
 )
((33)
 (cond
   ((eq? ch 62) 35)
   ((eq? ch 61) 34)
   (else 1)
  )
 )
((34)
 (cond
   (else 1)
  )
 )
((35)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((36)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((37)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((38)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
((39)
 (cond
   ((eq? ch 110) 40)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((40)
 (cond
   ((eq? ch 100) 41)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((41)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((42)
 (cond
   ((eq? ch 108) 43)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((43)
 (cond
   ((eq? ch 97) 44)
   ((sym-test-6 ch) 37)
   (else 1)
  )
 )
((44)
 (cond
   ((eq? ch 115) 45)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((45)
 (cond
   ((eq? ch 115) 46)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((46)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((47)
 (cond
   ((eq? ch 101) 48)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((48)
 (cond
   ((eq? ch 102) 49)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((49)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((50)
 (cond
   ((eq? ch 108) 51)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((51)
 (cond
   ((eq? ch 115) 54)
   ((eq? ch 105) 52)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((52)
 (cond
   ((eq? ch 102) 53)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((53)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((54)
 (cond
   ((eq? ch 101) 55)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((55)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((56)
 (cond
   ((eq? ch 111) 57)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((57)
 (cond
   ((eq? ch 114) 58)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((58)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((59)
 (cond
   ((eq? ch 115) 62)
   ((eq? ch 110) 61)
   ((eq? ch 102) 60)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((60)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((61)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((62)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((63)
 (cond
   ((eq? ch 111) 64)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((64)
 (cond
   ((eq? ch 116) 65)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((65)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((66)
 (cond
   ((eq? ch 114) 70)
   ((eq? ch 104) 67)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((67)
 (cond
   ((eq? ch 101) 68)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((68)
 (cond
   ((eq? ch 110) 69)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((69)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((70)
 (cond
   ((eq? ch 121) 71)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((71)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((72)
 (cond
   ((eq? ch 104) 73)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((73)
 (cond
   ((eq? ch 105) 74)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((74)
 (cond
   ((eq? ch 108) 75)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((75)
 (cond
   ((eq? ch 101) 76)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((76)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((77)
 (cond
   ((eq? ch 105) 78)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((78)
 (cond
   ((eq? ch 101) 79)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((79)
 (cond
   ((eq? ch 108) 80)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((80)
 (cond
   ((eq? ch 100) 81)
   ((sym-test-5 ch) 1)
   (else 37)
  )
 )
((81)
 (cond
   ((sym-test-4 ch) 37)
   (else 1)
  )
 )
((82)
 (cond
   ((eq? ch 61) 13)
   (else 1)
  )
 )
))
(define (sym-test-2 ch)
  (eq? (string-ref "1111111110011111111111111111111101001000000000000000000000000001100000000000000000000000000010001000000000000000000000000000000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" ch) #\1))
(define (sym-test-0 ch)
  (eq? (string-ref "0000000000000000000000000000000000000000000000001111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ch) #\1))
(define (sym-test-5 ch)
  (eq? (string-ref "1111111111111111111111111111111111111111111111110000000000111111100000000000000000000000000111101000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" ch) #\1))
(define (sym-test-6 ch)
  (eq? (string-ref "0000000000000000000000000000000000000000000000001111111111000000011111111111111111111111111000010011111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ch) #\1))
(define (sym-test-4 ch)
  (eq? (string-ref "0000000000000000000000000000000000000000000000001111111111000000011111111111111111111111111000010111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ch) #\1))
(define (sym-test-3 ch)
  (eq? (string-ref "0000000000000000000000000000000000000000110010100000000000110000000000000000000000000000000101000000000000000000000000000001011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ch) #\1))
(define (sym-test-1 ch)
  (eq? (string-ref "0000000001000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ch) #\1))
