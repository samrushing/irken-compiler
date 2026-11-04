;; -*- Mode: Irken -*-

;; dump/disassemble a bytecode file.

(require "lib/basis.scm")
(require "self/byteops.scm")

(define (read-bytecode-file f)
  (let ((pos 0))

    (define (get1)
      (let ((r (file/read-char f)))
        (inc! pos)
        r))

    (define (getn n)
      (let loop ((r '())
                 (n n))
        (if (= n 0)
            (reverse r)
            (loop (list:cons (get1) r) (- n 1)))))

    (define (read-magic)
      (match (getn 6) with
        (#\I #\R #\K #\V #\M #\0)
        -> #u
        _
        -> (raise (:BytecodeError pos))
        ))

    (define bytes->int
      n () -> n
      n (hd . tl) -> (bytes->int (logior (<< n 8) (char->ascii hd)) tl)
      )

    (define (read-int)
      (match (char->ascii (get1)) with
        254 -> (- (read-int))
        255 -> (bytes->int 0 (getn (char->ascii (get1))))
        n   -> n
        ))

    (define (read-string)
      (list->string (getn (read-int))))

    (define (read-n-of reader n)
      (let ((r '()))
        (for-range i n
          (push! r (reader)))
        (reverse r)))

    (define (read-literal)
      (match (get1) with
        #\+ -> (sexp:int (read-int))
        #\- -> (sexp:int (- (read-int)))
        #\T -> (sexp:bool #t)
        #\F -> (sexp:bool #f)
        #\u -> (sexp:undef)
        #\c -> (sexp:char (int->char (read-int)))
        #\S -> (sexp:string (read-string))
        #\Y -> (sexp:symbol (string->symbol (read-string)) (src:nil))
        #\I -> (sexp (sym 'imm) (int (read-int)))
        #\C -> (let ((tag (read-int))
                     (vals (read-n-of read-literal (read-int))))
                 (sexp (sym 'tuple) (int tag) (list vals)))
        #\V -> (sexp:vector (read-n-of read-literal (read-int)))
        #\P -> (sexp (sym 'ref) (int (read-int)))
        _ -> (raise (:BytecodeError pos))
        ))

    (define (read-literals)
      (match (get1) with
        #\V -> (let ((nlits (read-int))
                     (v (make-vector nlits (sexp:undef))))
                 (for-range i nlits
                   (set! v[i] (read-literal))
                   (printf "lit " (int i) " " (repr v[i]) "\n"))
                 (let ((field-lookup-table (read-literal))
                       (sizeoff-index (read-literal))
                       (sizeoff-literal (read-literal))
                       (metadata-index (read-literal)))
                   (printf "field-lookup-table " (repr field-lookup-table)
                           "\nsizeoff-index " (repr sizeoff-index)
                           "\nsizeoff-literal " (repr sizeoff-literal)
                           "\nmetadata-index " (repr metadata-index)
                           "\n")
                   ))
        x -> (%exit #f -1)
        ))

    (define (read-bytecode)
      (let ((codelen (read-int))
            (code (make-vector codelen 0)))
        (for-range i codelen
          (set! code[i] (read-int)))
        (printf "bytecode len = " (int codelen) "\n")
        ;; (for-range i codelen
        ;;   (printf (lpad 7 (int i)) " " (int code[i]) "\n"))
        (disassemble code)
        ))

    (define (go)
      (read-magic)
      (read-literals)
      (read-bytecode)
      )

    (go)

    ))

(define (disassemble code)

  (define compute-num-varargs
    pc 'nvcase -> (let ((nvar code[(+ pc 3)]))
                    (+ 3 (* 2 nvar)))
    pc 'trcall -> (let ((nvar code[(+ pc 3)]))
                    (+ 3 nvar))
    pc 'make   -> (let ((nvar code[(+ pc 3)]))
                    (+ 3 nvar))
    pc 'ffi    -> (let ((nvar code[(+ pc 4)]))
                    (+ 4 nvar))
    pc 'ffiv   -> (let ((nvar code[(+ pc 5)]))
                    (+ 5 nvar))
    pc other
    -> (raise (:BytecodeDisassemblyError (format "unknown varargs opcode? " (sym other))))
    )

  (let ((pc 0))
    (printf "disassembly: {\n")
    (while (< pc (vector-length code))
      (let ((opcode code[pc])
            (info opcode-info[opcode])
            (nargs info.nargs))
        (if info.varargs
            (set! nargs (compute-num-varargs pc info.name)))
        (printf (lpad 7 (int pc)) " " (sym info.name) " ")
        (for-range i nargs
          (printf (int code[(+ pc i 1)]) " "))
        (printf "\n")
        (inc! pc (+ nargs 1))
        ))
    (printf "}\n")
  ))

(define (main)
  (if (< sys.argc 2)
      (printf "Usage: " sys.argv[0] " sample.byc\n")
      (read-bytecode-file (file/open-read sys.argv[1]))))

(main)
