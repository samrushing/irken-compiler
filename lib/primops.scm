
(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (- x y)
  (%%cexp (int int -> int) "%s-%s" x y))

(define (* x y)
  (%%cexp (int int -> int) "%s*%s" x y))

(define (/ x y)
  (%%cexp (int int -> int) "%s/%s" x y))

(define (<< x y)
  (%%cexp (int int -> int) "%s<<%s" x y))

(define (>> x y)
  (%%cexp (int int -> int) "%s>>%s" x y))

(define (logior x y)
  (%%cexp (int int -> int) "%s|%s" x y))

(define (logand x y)
  (%%cexp (int int -> int) "%s&%s" x y))

(define (= x y)
  (%%cexp (int int -> bool) "%s==%s" x y))

(define (eq? x y)
  (%%cexp (? ? -> bool) "%s==%s" x y))

(define (zero? x)
  (%%cexp (int -> bool) "%s==0" x))

(define (>= x y)
  (%%cexp (int int -> bool) "%s >= %s" x y))

(define (> x y)
  (%%cexp (int int -> bool) "%s > %s" x y))

(define (<= x y)
  (%%cexp (int int -> bool) "%s <= %s" x y))

(define (< x y)
  (%%cexp (int int -> bool) "%s < %s" x y))

(define (print x)
  (%%cexp (? -> undefined) "dump_object (%s, 0)" x))

(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (newline)
  (%%cexp (-> undefined) "fprintf (stdout, \"\\n\")"))

(define (%getcc)
  (%%cexp (-> save) "k"))

(define (%putcc k v)
  (%%cexp (save ? -> undefined) "(k = %s, %s)" k v))

(define (%getlenv)
  (%%cexp (-> tuple) "lenv"))

(define (%make-tuple tag len)
  (%%cexp (int int -> ?) "allocate (%s, %s)" tag len))

(define (%make-immediate tag val)
  (%%cexp (int int -> ?) "(object *)(((%s<<8)|%s))" tag val))

;; XXX this is probably wrong
(define (%get-typecode ob)
  (%%cexp (? -> int) "is_immediate(%s)" ob))

;; primop_types = {
;;     '%+' : ('box(unbox(%s)+unbox(%s))', ('int', ('int', 'int'))),
;;     '%-' : ('box(unbox(%s)-unbox(%s))', ('int', ('int', 'int'))),
;;     '%*' : ('box(unbox(%s)*unbox(%s))', ('int', ('int', 'int'))),
;;     '%/' : ('box(unbox(%s)/unbox(%s))', ('int', ('int', 'int'))),
;;     '%<<': ('box(unbox(%s)<<unbox(%s))', ('int', ('int', 'int'))),
;;     '%>>': ('box(unbox(%s)>>unbox(%s))', ('int', ('int', 'int'))),
;;     '%|' : ('box(unbox(%s)|unbox(%s))', ('int', ('int', 'int'))),
;;     '%&' : ('box(unbox(%s)&unbox(%s))', ('int', ('int', 'int'))),
;;     '%==': ('PXLL_TEST(unbox(%s) == unbox(%s))', ('bool', ('int', 'int'))),
;;     '%eq?': ('PXLL_TEST(%s == %s)', ('bool', ('?', '?'))),
;;     '%zero?': ('PXLL_TEST(%s == box(0))', ('bool', ('int',))),
;;     '%ge?'  : ('PXLL_TEST(unbox(%s) >= unbox(%s))', ('bool', ('int', 'int'))),
;;     '%gt?'  : ('PXLL_TEST(unbox(%s) > unbox(%s))', ('bool', ('int', 'int'))),
;;     '%lt?'  : ('PXLL_TEST(unbox(%s) < unbox(%s))', ('bool', ('int', 'int'))),
;;     '%le?'  : ('PXLL_TEST(unbox(%s) <= unbox(%s))', ('bool', ('int', 'int'))),
;;     '%print' : ('dump_object (%s, 0)', ('int', ('?',))),
;;     '%printn' : ('dump_object (%s, 0); fprintf (stdout, "\\n")', ('int', ('?',))),
;;     '%newline' : ('fprintf (stdout, "\\n")', ('int', ())),
;;     '%getcc' : ('k', ('cont', ())),
;;     '%putcc' : ('(k = %s, %s)', ('?', ('cont', '?'))),
;;     '%getlenv' : ('lenv', ('lenv', ())),
;;     '%make-tuple' : ('allocate (unbox(%s), unbox(%s))', ('?', ('int', 'int'))),
;;     '%make-immediate': ('(object *)((unbox(%s)<<8)|(unbox(%s)))', ('?', ('int', 'int'))),
;;     '%get-typecode': ('box(is_immediate(%s))', ('int', ('?',))),
;;     }
