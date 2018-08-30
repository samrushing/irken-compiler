;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(require-ffi 'stdio)

(datatype insn
  (:incd)
  (:decd)
  (:incp)
  (:decp)
  (:out)
  (:in)
  (:left int)  ;; how far to jump forward
  (:right int) ;; how far to jump backward
  )

(define (parse gen)
  (define (P)
    (let loop ((code '())
               (i 0))
      (match (gen) with
        (maybe:yes ch)
        -> (match ch with
             #\> -> (loop (cons (insn:incd) code) (+ i 1))
             #\< -> (loop (cons (insn:decd) code) (+ i 1))
             #\+ -> (loop (cons (insn:incp) code) (+ i 1))
             #\- -> (loop (cons (insn:decp) code) (+ i 1))
             #\. -> (loop (cons (insn:out) code) (+ i 1))
             #\, -> (loop (cons (insn:in) code) (+ i 1))
             #\[ -> (let ((sub (P))
                          (len (length sub)))
                      (push! code (insn:left len))
                      (loop (append sub code) (+ i len)))
             #\] -> (let ((len (length code)))
                      (cons (insn:right (+ len 1)) code))
             _   -> (loop code i)
             )
        (maybe:no)
        -> code
        )))
  (list->vector (reverse (P))))

(defmacro inc! (inc! x) -> (set! x (+ x 1)))
(defmacro dec! (dec! x) -> (set! x (- x 1)))

(define (interp bf ingen)

  (define (getchar)
    (let ((ch (ingen)))
      (if (eq? ch #\eof)
          (%exit #f 0)
          ch)))

  (define (putchar ch)
    (stdio/putchar ch)
    #u)

  (let ((mem (make-vector 30000 0))
        (dp 0)
        (ip 0)
        (insn (insn:incd))
        )
    (while (< ip (vector-length bf))
      (set! insn bf[ip])
      (match insn with
        (insn:incd)    -> (inc! dp)
        (insn:decd)    -> (dec! dp)
        (insn:incp)    -> (inc! mem[dp])
        (insn:decp)    -> (dec! mem[dp])
        (insn:out)     -> (putchar mem[dp])
        (insn:in)      -> (set! mem[dp] (char->int (getchar)))
        (insn:left n)  -> (if (= 0 mem[dp]) (set! ip (+ ip n)))
        (insn:right n) -> (if (not (= 0 mem[dp])) (set! ip (- ip n)))
        )
      (inc! ip)
      )))

(define hello
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  )

(define (go bfgen)
  (let ((parsed (parse bfgen))
        (stdin (file/open-stdin)))
    (interp parsed (lambda () (file/read-char stdin)))
    ))

(if (< sys.argc 2)
    (go (string-generator hello))
    (go (file-char-generator (file/open-read sys.argv[1]))))

