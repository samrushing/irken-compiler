;; -*- Mode: Irken -*-

;; table of all opcodes for the vm.

;; NOTE: if you change this file, you need to re-run `vm/genopcodes` to
;;   regenerate the include file used by `vm/irkvm.c`.

(defmacro OI
  (OI name nargs varargs target?)
  -> {code=0 name=name nargs=nargs varargs=varargs target=target?})

;; XXX some bunches of these opcodes could be collapsed.
;; e.g. vlen/vref/vset == rlen/rref/rset (and maybe slen/sref/sset with a quick TC_STRING check)

(define opcode-info
  ;;    name   nargs varargs target? args
  #((OI 'lit     2      #f     #t)   ;; target index
    (OI 'litc    2      #f     #t)   ;; target index
    (OI 'ret     1      #f     #f)   ;; val
    (OI 'add     3      #f     #t)   ;; target a b
    (OI 'sub     3      #f     #t)   ;; target a b
    (OI 'mul     3      #f     #t)   ;; target a b
    (OI 'div     3      #f     #t)   ;; target a b
    (OI 'srem    3      #f     #t)   ;; target a b
    (OI 'shl     3      #f     #t)   ;; target a b
    (OI 'ashr    3      #f     #t)   ;; target a b
    (OI 'or      3      #f     #t)   ;; target a b
    (OI 'xor     3      #f     #t)   ;; target a b
    (OI 'and     3      #f     #t)   ;; target a b
    (OI 'eq      3      #f     #t)   ;; target a b
    (OI 'lt      3      #f     #t)   ;; target a b
    (OI 'gt      3      #f     #t)   ;; target a b
    (OI 'le      3      #f     #t)   ;; target a b
    (OI 'ge      3      #f     #t)   ;; target a b
    (OI 'cmp     3      #f     #t)   ;; target a b
    (OI 'tst     2      #f     #f)   ;; val offset
    (OI 'jmp     1      #f     #f)   ;; offset
    (OI 'fun     2      #f     #t)   ;; target offset
    (OI 'tail    2      #f     #f)   ;; closure args
    (OI 'tail0   1      #f     #f)   ;; closure
    (OI 'env     2      #f     #t)   ;; target size
    (OI 'stor    3      #f     #f)   ;; tuple arg index
    (OI 'ref     3      #f     #t)   ;; target depth index
    (OI 'mov     2      #f     #f)   ;; dst src
    (OI 'epush   1      #f     #f)   ;; args
    (OI 'trcall  3      #t     #f)   ;; offset depth nregs reg0 ...
    (OI 'trcall0 2      #f     #f)   ;; offset depth
    (OI 'ref0    2      #f     #t)   ;; target index
    (OI 'call    3      #f     #f)   ;; closure args nregs
    (OI 'call0   2      #f     #f)   ;; closure nregs
    (OI 'pop     1      #f     #t)   ;; target
    (OI 'printo  1      #f     #f)   ;; arg
    (OI 'prints  1      #f     #f)   ;; arg
    (OI 'topis   1      #f     #f)   ;; env
    (OI 'topref  2      #f     #t)   ;; target index
    (OI 'topset  2      #f     #f)   ;; index val
    (OI 'set     3      #f     #f)   ;; depth index val
    (OI 'set0    2      #f     #f)   ;; index val
    (OI 'pop0    0      #f     #f)   ;;
    (OI 'epop    0      #f     #f)   ;;
    (OI 'tron    0      #f     #f)   ;;
    (OI 'troff   0      #f     #f)   ;;
    (OI 'gc      0      #f     #f)   ;;
    (OI 'imm     2      #f     #t)   ;; target tag
    (OI 'make    4      #t     #t)   ;; target tag nelem elem0 ...
    (OI 'makei   3      #f     #t)   ;; target tag payload
    (OI 'exit    1      #f     #f)   ;; arg
    (OI 'nvcase  5      #t     #f)   ;; ob elabel nalts tag0 off0 tag1 off1 ...
    (OI 'tupref  3      #f     #t)   ;; target ob index
    (OI 'vlen    2      #f     #t)   ;; target vec
    (OI 'vref    3      #f     #t)   ;; target vec index-reg
    (OI 'vset    3      #f     #f)   ;; vec index-reg val
    (OI 'vmake   3      #f     #t)   ;; target size val
    (OI 'alloc   3      #f     #t)   ;; target tag size
    (OI 'rref    3      #f     #t)   ;; target rec label-code
    (OI 'rset    3      #f     #f)   ;; rec label-code val
    (OI 'getcc   1      #f     #t)   ;; target
    (OI 'putcc   3      #f     #t)   ;; target k val
    ;; (OI 'irk     3      #t     #t)   ;; target closure nargs arg0 ...
    ;; (OI 'getc    1      #f     #t)   ;; target
    (OI 'dlsym   2      #f     #t)   ;; target name
    (OI 'ffi     4      #t     #t)   ;; target fun nargs arg0 ...
    (OI 'smake   2      #f     #t)   ;; target size
    (OI 'sfromc  2      #f     #t)   ;; target src
    (OI 'slen    2      #f     #t)   ;; target string
    (OI 'sref    3      #f     #t)   ;; target string index
    (OI 'sset    3      #f     #f)   ;; string index char
    (OI 'scopy   5      #f     #f)   ;; src src-start n dst dst-start
    (OI 'unchar  2      #f     #t)   ;; target char
    (OI 'gist    1      #f     #t)   ;; target
    (OI 'argv    1      #f     #t)   ;; target
    (OI 'quiet   1      #f     #f)   ;; bool
    (OI 'heap    2      #f     #f)   ;; size nreg
    (OI 'readf   2      #f     #t)   ;; target path
    (OI 'malloc  3      #f     #t)   ;; target sindex size
    (OI 'halloc  3      #f     #t)   ;; target sindex size
    (OI 'cget    3      #f     #t)   ;; target src code
    (OI 'cset    3      #f     #f)   ;; src code val
    (OI 'free    1      #f     #f)   ;; src
    (OI 'sizeoff 2      #f     #f)   ;; index val
    (OI 'sgetp   2      #f     #t)   ;; dst src
    (OI 'caref   3      #f     #t)   ;; dst sindex num
    (OI 'csref   3      #f     #t)   ;; dst src sindex
    (OI 'dlsym2  2      #f     #t)   ;; target name
    (OI 'csize   2      #f     #t)   ;; target sindex
    (OI 'cref2int 2     #f     #t)   ;; target src
    (OI 'int2cref 2     #f     #t)   ;; target src
    (OI 'errno   0      #f     #t)   ;; target
    ;;  name   nargs varargs target? args
    ))

(for-range i (vector-length opcode-info)
  (let ((info opcode-info[i]))
    (set! info.code i)))

