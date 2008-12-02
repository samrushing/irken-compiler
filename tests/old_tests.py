old_tests = [
    ("123", "123"),
    ("(%ident 1)", "1"),
    ("(%plus 3 4)", "7"),
    ("(%minus 12 9)", "3"),
    ("(%is_zero 0)", "#t"),
    ("(%is_zero 1)", "#f"),
    ("(%is_zero (%minus 3 (%plus 2 2)))", "#f"),
    ("(%minus (%plus 3 (%plus 19 34)) (%minus 12 (%minus 6 3)))", "47"),
    ("(if (%is_zero (%minus 3 (%plus 2 1))) (%plus 12 13) (%plus 22 24))", "25"),
    ("(if (%is_zero (%minus 3 (%plus 2 2))) (%plus 12 13) (%plus 22 24))", "46"),
    ("(begin 1 2 3)", "3"),
    ("(and 1 2 3)", "3"),
    ("(let ((x 1) (y 2)) (%print 99) (%plus x y))", "99\n3"),
    ("(let ((x 3)) (%plus x 5))", "8"),
    ("(let loop ((n 5)) (if (%is_zero n) 1 (%times n (loop (%minus n 1)))))", "120"),
    ("(let loop ((n 6) (a 1)) (if (%is_zero n) a (loop (%minus n 1) (%times n a))))", "720"),
    ("((lambda () 5) 3)", "5"),
    ("((lambda (x) (%plus 2 3)) 7)", "5"),
    ("(%plus 3 ((lambda (x) (%plus 2 3)) 7))", "8"),
    ("((lambda (x) (%plus x 1)) 3)", "4"),
    ("((lambda (x y) (%plus x y)) 3 4)", "7"),
    ("((lambda (x) (begin (set! x 3) x)) 5)", "3"),
    ("((lambda (x) ((lambda (y) (%plus x y)) 3)) 5)", "8"),
    ("((lambda (x) (%plus (%zero) (%plus x 1))) 5)", "6"),
    ("(let ((y 200)) (let ((x 100)) (begin 2 3 (%dump (%getcc)) 4 5 6 7)))", None),
    # strangely, this definition of callcc seems to work fine,
    #  but when I try to make more primitive use of getcc/putcc, I get scragged.
    ("""
(let ((callcc
       (lambda (p) (let ((k (%getcc))) (p (lambda (r) (%putcc k r)))))
       ))
  (begin
    (%print 1)
    (%print 2)
    (%print 3)
    (%print 4)
    (callcc (lambda (exit) (%print 5) (%print 6) (exit 3) (%print 7) (%print 8)))
    (%print 9)
    88
    ))""", "1\n2\n3\n4\n5\n6\n9\n88"),
    # guaranteed to hit gc (at least until primops don't cons)
    ("""
(let loop ((n 20))
  (let tak ((x 18) (y 12) (z 6))
    (if (%ge? y x)
        z
        (tak (tak (%minus x 1) y z)
             (tak (%minus y 1) z x)
             (tak (%minus z 1) x y))))
  (if (%is_zero n)
      99
      (loop (%minus n 1))))""", "99"),
    #("(let loop ((n 6) (a 1)) (if (%is_zero n) a (loop (%minus n 1) (%times n a))))", "720")
    #("(%dump (%getcc))", None),
    # should trigger error...
    #("(1 2 3)", None),
    #("(%getcc)", None),
    #("(begin 1 2 (let ((k (%getcc))) (%print 3) (%print 4)(%putcc k 99)(%print 5)(%print 6)) (%print 7) (%print 8))", None),
    #("(begin 2 3 4 (%print 50) (let ((cc (%getcc))) (begin 5 6 (%print 100) (%putcc cc 7) (%print 200) 8 9 10)))", "7"),
    #("(%plus 2 (let ((cc (%getcc))) (begin 3 4 5 (%putcc cc 6) 7 8)))", "8"),
]
