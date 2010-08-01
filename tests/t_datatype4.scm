;; parser tables

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")

;; parser tables

(datatype action
  (:shift int)
  (:reduce int int))

(datatype action-list
  (:nil)
  (:cons int (action) (action-list)))

(datatype goto
  (:nil)
  (:cons int int (goto)))

(define terminals #(
    'caret
    'slashslash
    'lshift
    'rshift
    'STRING
    'minus
    'percent
    'NUMBER
    '<e>
    '<$>
    'plus
    'vbar
    'slash
    'tilde
    'ampersand
    'splatsplat
    'splat
    'NAME
  ))
(define non-terminals #(
    'atom_52
    'term_28
    'atom_50
    'atom_54
    'and_expr_10
    'factor_41
    'term_32
    'term_30
    'factor_44
    'xor_expr_6
    'arith_expr_24
    '<S>
    'arith_expr_21
    'term_35
    'expr
    'shift_expr_17
    'expr_2
    'shift_expr_14
    'power_46
    'factor_39
  ))
(define actions
  #(
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 16 (action:reduce 1 0) (action-list:cons 15 (action:reduce 1 0) (action-list:cons 14 (action:reduce 1 0) (action-list:cons 12 (action:reduce 1 0) (action-list:cons 11 (action:reduce 1 0) (action-list:cons 10 (action:reduce 1 0) (action-list:cons 9 (action:reduce 1 0) (action-list:cons 6 (action:reduce 1 0) (action-list:cons 5 (action:reduce 1 0) (action-list:cons 4 (action:reduce 1 0) (action-list:cons 3 (action:reduce 1 0) (action-list:cons 2 (action:reduce 1 0) (action-list:cons 1 (action:reduce 1 0) (action-list:cons 0 (action:reduce 1 0) (action-list:nil)))))))))))))))
    (action-list:cons 17 (action:reduce 1 19) (action-list:cons 13 (action:reduce 1 19) (action-list:cons 10 (action:reduce 1 19) (action-list:cons 7 (action:reduce 1 19) (action-list:cons 5 (action:reduce 1 19) (action-list:cons 4 (action:reduce 1 19) (action-list:nil)))))))
    (action-list:cons 16 (action:reduce 1 2) (action-list:cons 15 (action:reduce 1 2) (action-list:cons 14 (action:reduce 1 2) (action-list:cons 12 (action:reduce 1 2) (action-list:cons 11 (action:reduce 1 2) (action-list:cons 10 (action:reduce 1 2) (action-list:cons 9 (action:reduce 1 2) (action-list:cons 6 (action:reduce 1 2) (action-list:cons 5 (action:reduce 1 2) (action-list:cons 3 (action:reduce 1 2) (action-list:cons 2 (action:reduce 1 2) (action-list:cons 1 (action:reduce 1 2) (action-list:cons 0 (action:reduce 1 2) (action-list:nil))))))))))))))
    (action-list:cons 17 (action:reduce 1 19) (action-list:cons 13 (action:reduce 1 19) (action-list:cons 10 (action:reduce 1 19) (action-list:cons 7 (action:reduce 1 19) (action-list:cons 5 (action:reduce 1 19) (action-list:cons 4 (action:reduce 1 19) (action-list:nil)))))))
    (action-list:cons 17 (action:reduce 1 5) (action-list:cons 13 (action:reduce 1 5) (action-list:cons 10 (action:reduce 1 5) (action-list:cons 7 (action:reduce 1 5) (action-list:cons 5 (action:reduce 1 5) (action-list:cons 4 (action:reduce 1 5) (action-list:nil)))))))
    (action-list:cons 16 (action:reduce 1 2) (action-list:cons 15 (action:reduce 1 2) (action-list:cons 14 (action:reduce 1 2) (action-list:cons 12 (action:reduce 1 2) (action-list:cons 11 (action:reduce 1 2) (action-list:cons 10 (action:reduce 1 2) (action-list:cons 9 (action:reduce 1 2) (action-list:cons 6 (action:reduce 1 2) (action-list:cons 5 (action:reduce 1 2) (action-list:cons 3 (action:reduce 1 2) (action-list:cons 2 (action:reduce 1 2) (action-list:cons 1 (action:reduce 1 2) (action-list:cons 0 (action:reduce 1 2) (action-list:nil))))))))))))))
    (action-list:cons 16 (action:reduce 1 3) (action-list:cons 15 (action:reduce 1 3) (action-list:cons 14 (action:reduce 1 3) (action-list:cons 12 (action:reduce 1 3) (action-list:cons 11 (action:reduce 1 3) (action-list:cons 10 (action:reduce 1 3) (action-list:cons 9 (action:reduce 1 3) (action-list:cons 6 (action:reduce 1 3) (action-list:cons 5 (action:reduce 1 3) (action-list:cons 4 (action:shift 14) (action-list:cons 3 (action:reduce 1 3) (action-list:cons 2 (action:reduce 1 3) (action-list:cons 1 (action:reduce 1 3) (action-list:cons 0 (action:reduce 1 3) (action-list:nil)))))))))))))))
    (action-list:cons 16 (action:reduce 1 3) (action-list:cons 15 (action:reduce 1 3) (action-list:cons 14 (action:reduce 1 3) (action-list:cons 12 (action:reduce 1 3) (action-list:cons 11 (action:reduce 1 3) (action-list:cons 10 (action:reduce 1 3) (action-list:cons 9 (action:reduce 1 3) (action-list:cons 6 (action:reduce 1 3) (action-list:cons 5 (action:reduce 1 3) (action-list:cons 3 (action:reduce 1 3) (action-list:cons 2 (action:reduce 1 3) (action-list:cons 1 (action:reduce 1 3) (action-list:cons 0 (action:reduce 1 3) (action-list:nil))))))))))))))
    (action-list:cons 16 (action:reduce 0 18) (action-list:cons 15 (action:shift 15) (action-list:cons 14 (action:reduce 0 18) (action-list:cons 12 (action:reduce 0 18) (action-list:cons 11 (action:reduce 0 18) (action-list:cons 10 (action:reduce 0 18) (action-list:cons 9 (action:reduce 0 18) (action-list:cons 6 (action:reduce 0 18) (action-list:cons 5 (action:reduce 0 18) (action-list:cons 3 (action:reduce 0 18) (action-list:cons 2 (action:reduce 0 18) (action-list:cons 1 (action:reduce 0 18) (action-list:cons 0 (action:reduce 0 18) (action-list:nil))))))))))))))
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 16 (action:reduce 0 13) (action-list:cons 14 (action:reduce 0 13) (action-list:cons 12 (action:reduce 0 13) (action-list:cons 11 (action:reduce 0 13) (action-list:cons 10 (action:reduce 0 13) (action-list:cons 9 (action:reduce 0 13) (action-list:cons 6 (action:reduce 0 13) (action-list:cons 5 (action:reduce 0 13) (action-list:cons 3 (action:reduce 0 13) (action-list:cons 2 (action:reduce 0 13) (action-list:cons 1 (action:reduce 0 13) (action-list:cons 0 (action:reduce 0 13) (action-list:nil)))))))))))))
    (action-list:cons 9 (action:shift 19) (action-list:nil))
    (action-list:cons 17 (action:reduce 1 5) (action-list:cons 13 (action:reduce 1 5) (action-list:cons 10 (action:reduce 1 5) (action-list:cons 7 (action:reduce 1 5) (action-list:cons 5 (action:reduce 1 5) (action-list:cons 4 (action:reduce 1 5) (action-list:nil)))))))
    (action-list:cons 16 (action:reduce 2 0) (action-list:cons 15 (action:reduce 2 0) (action-list:cons 14 (action:reduce 2 0) (action-list:cons 12 (action:reduce 2 0) (action-list:cons 11 (action:reduce 2 0) (action-list:cons 10 (action:reduce 2 0) (action-list:cons 9 (action:reduce 2 0) (action-list:cons 6 (action:reduce 2 0) (action-list:cons 5 (action:reduce 2 0) (action-list:cons 4 (action:reduce 2 0) (action-list:cons 3 (action:reduce 2 0) (action-list:cons 2 (action:reduce 2 0) (action-list:cons 1 (action:reduce 2 0) (action-list:cons 0 (action:reduce 2 0) (action-list:nil)))))))))))))))
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 16 (action:reduce 2 8) (action-list:cons 14 (action:reduce 2 8) (action-list:cons 12 (action:reduce 2 8) (action-list:cons 11 (action:reduce 2 8) (action-list:cons 10 (action:reduce 2 8) (action-list:cons 9 (action:reduce 2 8) (action-list:cons 6 (action:reduce 2 8) (action-list:cons 5 (action:reduce 2 8) (action-list:cons 3 (action:reduce 2 8) (action-list:cons 2 (action:reduce 2 8) (action-list:cons 1 (action:reduce 2 8) (action-list:cons 0 (action:reduce 2 8) (action-list:nil)))))))))))))
    (action-list:cons 16 (action:reduce 2 8) (action-list:cons 14 (action:reduce 2 8) (action-list:cons 12 (action:reduce 2 8) (action-list:cons 11 (action:reduce 2 8) (action-list:cons 10 (action:reduce 2 8) (action-list:cons 9 (action:reduce 2 8) (action-list:cons 6 (action:reduce 2 8) (action-list:cons 5 (action:reduce 2 8) (action-list:cons 3 (action:reduce 2 8) (action-list:cons 2 (action:reduce 2 8) (action-list:cons 1 (action:reduce 2 8) (action-list:cons 0 (action:reduce 2 8) (action-list:nil)))))))))))))
    (action-list:cons 16 (action:shift 24) (action-list:cons 14 (action:reduce 0 10) (action-list:cons 12 (action:shift 23) (action-list:cons 11 (action:reduce 0 10) (action-list:cons 10 (action:reduce 0 10) (action-list:cons 9 (action:reduce 0 10) (action-list:cons 6 (action:shift 22) (action-list:cons 5 (action:reduce 0 10) (action-list:cons 3 (action:reduce 0 10) (action-list:cons 2 (action:reduce 0 10) (action-list:cons 1 (action:shift 21) (action-list:cons 0 (action:reduce 0 10) (action-list:nil)))))))))))))
    (action-list:cons 8 (action:reduce 2 11) (action-list:nil))
    (action-list:cons 16 (action:reduce 2 18) (action-list:cons 14 (action:reduce 2 18) (action-list:cons 12 (action:reduce 2 18) (action-list:cons 11 (action:reduce 2 18) (action-list:cons 10 (action:reduce 2 18) (action-list:cons 9 (action:reduce 2 18) (action-list:cons 6 (action:reduce 2 18) (action-list:cons 5 (action:reduce 2 18) (action-list:cons 3 (action:reduce 2 18) (action-list:cons 2 (action:reduce 2 18) (action-list:cons 1 (action:reduce 2 18) (action-list:cons 0 (action:reduce 2 18) (action-list:nil)))))))))))))
    (action-list:cons 17 (action:reduce 1 6) (action-list:cons 13 (action:reduce 1 6) (action-list:cons 10 (action:reduce 1 6) (action-list:cons 7 (action:reduce 1 6) (action-list:cons 5 (action:reduce 1 6) (action-list:cons 4 (action:reduce 1 6) (action-list:nil)))))))
    (action-list:cons 17 (action:reduce 1 7) (action-list:cons 13 (action:reduce 1 7) (action-list:cons 10 (action:reduce 1 7) (action-list:cons 7 (action:reduce 1 7) (action-list:cons 5 (action:reduce 1 7) (action-list:cons 4 (action:reduce 1 7) (action-list:nil)))))))
    (action-list:cons 17 (action:reduce 1 1) (action-list:cons 13 (action:reduce 1 1) (action-list:cons 10 (action:reduce 1 1) (action-list:cons 7 (action:reduce 1 1) (action-list:cons 5 (action:reduce 1 1) (action-list:cons 4 (action:reduce 1 1) (action-list:nil)))))))
    (action-list:cons 17 (action:reduce 1 1) (action-list:cons 13 (action:reduce 1 1) (action-list:cons 10 (action:reduce 1 1) (action-list:cons 7 (action:reduce 1 1) (action-list:cons 5 (action:reduce 1 1) (action-list:cons 4 (action:reduce 1 1) (action-list:nil)))))))
    (action-list:cons 17 (action:reduce 1 7) (action-list:cons 13 (action:reduce 1 7) (action-list:cons 10 (action:reduce 1 7) (action-list:cons 7 (action:reduce 1 7) (action-list:cons 5 (action:reduce 1 7) (action-list:cons 4 (action:reduce 1 7) (action-list:nil)))))))
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 17 (action:reduce 1 6) (action-list:cons 13 (action:reduce 1 6) (action-list:cons 10 (action:reduce 1 6) (action-list:cons 7 (action:reduce 1 6) (action-list:cons 5 (action:reduce 1 6) (action-list:cons 4 (action:reduce 1 6) (action-list:nil)))))))
    (action-list:cons 14 (action:reduce 0 15) (action-list:cons 11 (action:reduce 0 15) (action-list:cons 10 (action:shift 31) (action-list:cons 9 (action:reduce 0 15) (action-list:cons 5 (action:shift 30) (action-list:cons 3 (action:reduce 0 15) (action-list:cons 2 (action:reduce 0 15) (action-list:cons 0 (action:reduce 0 15) (action-list:nil)))))))))
    (action-list:cons 16 (action:reduce 3 13) (action-list:cons 14 (action:reduce 3 13) (action-list:cons 12 (action:reduce 3 13) (action-list:cons 11 (action:reduce 3 13) (action-list:cons 10 (action:reduce 3 13) (action-list:cons 9 (action:reduce 3 13) (action-list:cons 6 (action:reduce 3 13) (action-list:cons 5 (action:reduce 3 13) (action-list:cons 3 (action:reduce 3 13) (action-list:cons 2 (action:reduce 3 13) (action-list:cons 1 (action:reduce 3 13) (action-list:cons 0 (action:reduce 3 13) (action-list:nil)))))))))))))
    (action-list:cons 17 (action:reduce 1 12) (action-list:cons 13 (action:reduce 1 12) (action-list:cons 10 (action:reduce 1 12) (action-list:cons 7 (action:reduce 1 12) (action-list:cons 5 (action:reduce 1 12) (action-list:cons 4 (action:reduce 1 12) (action-list:nil)))))))
    (action-list:cons 17 (action:reduce 1 12) (action-list:cons 13 (action:reduce 1 12) (action-list:cons 10 (action:reduce 1 12) (action-list:cons 7 (action:reduce 1 12) (action-list:cons 5 (action:reduce 1 12) (action-list:cons 4 (action:reduce 1 12) (action-list:nil)))))))
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 14 (action:reduce 0 4) (action-list:cons 11 (action:reduce 0 4) (action-list:cons 9 (action:reduce 0 4) (action-list:cons 3 (action:shift 36) (action-list:cons 2 (action:shift 35) (action-list:cons 0 (action:reduce 0 4) (action-list:nil)))))))
    (action-list:cons 16 (action:reduce 0 13) (action-list:cons 14 (action:reduce 0 13) (action-list:cons 12 (action:reduce 0 13) (action-list:cons 11 (action:reduce 0 13) (action-list:cons 10 (action:reduce 0 13) (action-list:cons 9 (action:reduce 0 13) (action-list:cons 6 (action:reduce 0 13) (action-list:cons 5 (action:reduce 0 13) (action-list:cons 3 (action:reduce 0 13) (action-list:cons 2 (action:reduce 0 13) (action-list:cons 1 (action:reduce 0 13) (action-list:cons 0 (action:reduce 0 13) (action-list:nil)))))))))))))
    (action-list:cons 17 (action:reduce 1 17) (action-list:cons 13 (action:reduce 1 17) (action-list:cons 10 (action:reduce 1 17) (action-list:cons 7 (action:reduce 1 17) (action-list:cons 5 (action:reduce 1 17) (action-list:cons 4 (action:reduce 1 17) (action-list:nil)))))))
    (action-list:cons 17 (action:reduce 1 17) (action-list:cons 13 (action:reduce 1 17) (action-list:cons 10 (action:reduce 1 17) (action-list:cons 7 (action:reduce 1 17) (action-list:cons 5 (action:reduce 1 17) (action-list:cons 4 (action:reduce 1 17) (action-list:nil)))))))
    (action-list:cons 14 (action:shift 40) (action-list:cons 11 (action:reduce 0 9) (action-list:cons 9 (action:reduce 0 9) (action-list:cons 0 (action:reduce 0 9) (action-list:nil)))))
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 16 (action:shift 24) (action-list:cons 14 (action:reduce 4 10) (action-list:cons 12 (action:shift 23) (action-list:cons 11 (action:reduce 4 10) (action-list:cons 10 (action:reduce 4 10) (action-list:cons 9 (action:reduce 4 10) (action-list:cons 6 (action:shift 22) (action-list:cons 5 (action:reduce 4 10) (action-list:cons 3 (action:reduce 4 10) (action-list:cons 2 (action:reduce 4 10) (action-list:cons 1 (action:shift 21) (action-list:cons 0 (action:reduce 4 10) (action-list:nil)))))))))))))
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 11 (action:reduce 0 16) (action-list:cons 9 (action:reduce 0 16) (action-list:cons 0 (action:shift 44) (action-list:nil))))
    (action-list:cons 16 (action:reduce 0 13) (action-list:cons 14 (action:reduce 0 13) (action-list:cons 12 (action:reduce 0 13) (action-list:cons 11 (action:reduce 0 13) (action-list:cons 10 (action:reduce 0 13) (action-list:cons 9 (action:reduce 0 13) (action-list:cons 6 (action:reduce 0 13) (action-list:cons 5 (action:reduce 0 13) (action-list:cons 3 (action:reduce 0 13) (action-list:cons 2 (action:reduce 0 13) (action-list:cons 1 (action:reduce 0 13) (action-list:cons 0 (action:reduce 0 13) (action-list:nil)))))))))))))
    (action-list:cons 16 (action:reduce 0 13) (action-list:cons 14 (action:reduce 0 13) (action-list:cons 12 (action:reduce 0 13) (action-list:cons 11 (action:reduce 0 13) (action-list:cons 10 (action:reduce 0 13) (action-list:cons 9 (action:reduce 0 13) (action-list:cons 6 (action:reduce 0 13) (action-list:cons 5 (action:reduce 0 13) (action-list:cons 3 (action:reduce 0 13) (action-list:cons 2 (action:reduce 0 13) (action-list:cons 1 (action:reduce 0 13) (action-list:cons 0 (action:reduce 0 13) (action-list:nil)))))))))))))
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 11 (action:shift 49) (action-list:cons 9 (action:reduce 7 14) (action-list:nil)))
    (action-list:cons 16 (action:shift 24) (action-list:cons 14 (action:reduce 0 10) (action-list:cons 12 (action:shift 23) (action-list:cons 11 (action:reduce 0 10) (action-list:cons 10 (action:reduce 0 10) (action-list:cons 9 (action:reduce 0 10) (action-list:cons 6 (action:shift 22) (action-list:cons 5 (action:reduce 0 10) (action-list:cons 3 (action:reduce 0 10) (action-list:cons 2 (action:reduce 0 10) (action-list:cons 1 (action:shift 21) (action-list:cons 0 (action:reduce 0 10) (action-list:nil)))))))))))))
    (action-list:cons 16 (action:shift 24) (action-list:cons 14 (action:reduce 0 10) (action-list:cons 12 (action:shift 23) (action-list:cons 11 (action:reduce 0 10) (action-list:cons 10 (action:reduce 0 10) (action-list:cons 9 (action:reduce 0 10) (action-list:cons 6 (action:shift 22) (action-list:cons 5 (action:reduce 0 10) (action-list:cons 3 (action:reduce 0 10) (action-list:cons 2 (action:reduce 0 10) (action-list:cons 1 (action:shift 21) (action-list:cons 0 (action:reduce 0 10) (action-list:nil)))))))))))))
    (action-list:cons 16 (action:reduce 0 13) (action-list:cons 14 (action:reduce 0 13) (action-list:cons 12 (action:reduce 0 13) (action-list:cons 11 (action:reduce 0 13) (action-list:cons 10 (action:reduce 0 13) (action-list:cons 9 (action:reduce 0 13) (action-list:cons 6 (action:reduce 0 13) (action-list:cons 5 (action:reduce 0 13) (action-list:cons 3 (action:reduce 0 13) (action-list:cons 2 (action:reduce 0 13) (action-list:cons 1 (action:reduce 0 13) (action-list:cons 0 (action:reduce 0 13) (action-list:nil)))))))))))))
    (action-list:cons 17 (action:shift 6) (action-list:cons 13 (action:shift 5) (action-list:cons 10 (action:shift 4) (action-list:cons 7 (action:shift 3) (action-list:cons 5 (action:shift 2) (action-list:cons 4 (action:shift 1) (action-list:nil)))))))
    (action-list:cons 14 (action:reduce 5 15) (action-list:cons 11 (action:reduce 5 15) (action-list:cons 10 (action:shift 31) (action-list:cons 9 (action:reduce 5 15) (action-list:cons 5 (action:shift 30) (action-list:cons 3 (action:reduce 5 15) (action-list:cons 2 (action:reduce 5 15) (action-list:cons 0 (action:reduce 5 15) (action-list:nil)))))))))
    (action-list:cons 14 (action:reduce 0 15) (action-list:cons 11 (action:reduce 0 15) (action-list:cons 10 (action:shift 31) (action-list:cons 9 (action:reduce 0 15) (action-list:cons 5 (action:shift 30) (action-list:cons 3 (action:reduce 0 15) (action-list:cons 2 (action:reduce 0 15) (action-list:cons 0 (action:reduce 0 15) (action-list:nil)))))))))
    (action-list:cons 16 (action:shift 24) (action-list:cons 14 (action:reduce 0 10) (action-list:cons 12 (action:shift 23) (action-list:cons 11 (action:reduce 0 10) (action-list:cons 10 (action:reduce 0 10) (action-list:cons 9 (action:reduce 0 10) (action-list:cons 6 (action:shift 22) (action-list:cons 5 (action:reduce 0 10) (action-list:cons 3 (action:reduce 0 10) (action-list:cons 2 (action:reduce 0 10) (action-list:cons 1 (action:shift 21) (action-list:cons 0 (action:reduce 0 10) (action-list:nil)))))))))))))
    (action-list:cons 16 (action:reduce 0 13) (action-list:cons 14 (action:reduce 0 13) (action-list:cons 12 (action:reduce 0 13) (action-list:cons 11 (action:reduce 0 13) (action-list:cons 10 (action:reduce 0 13) (action-list:cons 9 (action:reduce 0 13) (action-list:cons 6 (action:reduce 0 13) (action-list:cons 5 (action:reduce 0 13) (action-list:cons 3 (action:reduce 0 13) (action-list:cons 2 (action:reduce 0 13) (action-list:cons 1 (action:reduce 0 13) (action-list:cons 0 (action:reduce 0 13) (action-list:nil)))))))))))))
    (action-list:cons 14 (action:reduce 6 4) (action-list:cons 11 (action:reduce 6 4) (action-list:cons 9 (action:reduce 6 4) (action-list:cons 3 (action:shift 36) (action-list:cons 2 (action:shift 35) (action-list:cons 0 (action:reduce 6 4) (action-list:nil)))))))
    (action-list:cons 14 (action:reduce 0 15) (action-list:cons 11 (action:reduce 0 15) (action-list:cons 10 (action:shift 31) (action-list:cons 9 (action:reduce 0 15) (action-list:cons 5 (action:shift 30) (action-list:cons 3 (action:reduce 0 15) (action-list:cons 2 (action:reduce 0 15) (action-list:cons 0 (action:reduce 0 15) (action-list:nil)))))))))
    (action-list:cons 16 (action:shift 24) (action-list:cons 14 (action:reduce 0 10) (action-list:cons 12 (action:shift 23) (action-list:cons 11 (action:reduce 0 10) (action-list:cons 10 (action:reduce 0 10) (action-list:cons 9 (action:reduce 0 10) (action-list:cons 6 (action:shift 22) (action-list:cons 5 (action:reduce 0 10) (action-list:cons 3 (action:reduce 0 10) (action-list:cons 2 (action:reduce 0 10) (action-list:cons 1 (action:shift 21) (action-list:cons 0 (action:reduce 0 10) (action-list:nil)))))))))))))
    (action-list:cons 14 (action:reduce 0 4) (action-list:cons 11 (action:reduce 0 4) (action-list:cons 9 (action:reduce 0 4) (action-list:cons 3 (action:shift 36) (action-list:cons 2 (action:shift 35) (action-list:cons 0 (action:reduce 0 4) (action-list:nil)))))))
    (action-list:cons 14 (action:reduce 0 15) (action-list:cons 11 (action:reduce 0 15) (action-list:cons 10 (action:shift 31) (action-list:cons 9 (action:reduce 0 15) (action-list:cons 5 (action:shift 30) (action-list:cons 3 (action:reduce 0 15) (action-list:cons 2 (action:reduce 0 15) (action-list:cons 0 (action:reduce 0 15) (action-list:nil)))))))))
    (action-list:cons 14 (action:shift 40) (action-list:cons 11 (action:reduce 7 9) (action-list:cons 9 (action:reduce 7 9) (action-list:cons 0 (action:reduce 7 9) (action-list:nil)))))
    (action-list:cons 14 (action:reduce 0 4) (action-list:cons 11 (action:reduce 0 4) (action-list:cons 9 (action:reduce 0 4) (action-list:cons 3 (action:shift 36) (action-list:cons 2 (action:shift 35) (action-list:cons 0 (action:reduce 0 4) (action-list:nil)))))))
    (action-list:cons 14 (action:shift 40) (action-list:cons 11 (action:reduce 0 9) (action-list:cons 9 (action:reduce 0 9) (action-list:cons 0 (action:reduce 0 9) (action-list:nil)))))
    (action-list:cons 11 (action:reduce 8 16) (action-list:cons 9 (action:reduce 8 16) (action-list:cons 0 (action:shift 44) (action-list:nil))))
    ))
(define goto
  #(
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 14 12 (goto:cons 8 11 (goto:cons 19 13 (goto:nil))))))))
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:cons 18 16 (goto:nil))
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 8 17 (goto:cons 19 13 (goto:nil)))))))
    (goto:cons 13 18 (goto:nil))
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 8 20 (goto:cons 19 13 (goto:nil)))))))
    (goto:nil)
    (goto:nil)
    (goto:cons 1 25 (goto:cons 6 26 (goto:cons 10 28 (goto:cons 7 27 (goto:nil)))))
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 8 29 (goto:cons 19 13 (goto:nil)))))))
    (goto:nil)
    (goto:cons 15 33 (goto:cons 12 32 (goto:nil)))
    (goto:nil)
    (goto:nil)
    (goto:nil)
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 8 34 (goto:cons 19 13 (goto:nil)))))))
    (goto:cons 17 38 (goto:cons 4 37 (goto:nil)))
    (goto:cons 13 39 (goto:nil))
    (goto:nil)
    (goto:nil)
    (goto:cons 9 41 (goto:nil))
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 8 42 (goto:cons 19 13 (goto:nil)))))))
    (goto:cons 1 25 (goto:cons 6 26 (goto:cons 7 27 (goto:nil))))
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 8 43 (goto:cons 19 13 (goto:nil)))))))
    (goto:cons 16 45 (goto:nil))
    (goto:cons 13 46 (goto:nil))
    (goto:cons 13 47 (goto:nil))
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 8 48 (goto:cons 19 13 (goto:nil)))))))
    (goto:nil)
    (goto:cons 1 25 (goto:cons 6 26 (goto:cons 10 50 (goto:cons 7 27 (goto:nil)))))
    (goto:cons 1 25 (goto:cons 6 26 (goto:cons 10 51 (goto:cons 7 27 (goto:nil)))))
    (goto:cons 13 52 (goto:nil))
    (goto:cons 0 7 (goto:cons 2 8 (goto:cons 3 9 (goto:cons 5 10 (goto:cons 8 53 (goto:cons 19 13 (goto:nil)))))))
    (goto:cons 12 32 (goto:nil))
    (goto:cons 15 54 (goto:cons 12 32 (goto:nil)))
    (goto:cons 1 25 (goto:cons 6 26 (goto:cons 10 55 (goto:cons 7 27 (goto:nil)))))
    (goto:cons 13 56 (goto:nil))
    (goto:cons 17 38 (goto:nil))
    (goto:cons 15 57 (goto:cons 12 32 (goto:nil)))
    (goto:cons 1 25 (goto:cons 6 26 (goto:cons 10 58 (goto:cons 7 27 (goto:nil)))))
    (goto:cons 17 38 (goto:cons 4 59 (goto:nil)))
    (goto:cons 15 60 (goto:cons 12 32 (goto:nil)))
    (goto:nil)
    (goto:cons 17 38 (goto:cons 4 61 (goto:nil)))
    (goto:cons 9 62 (goto:nil))
    (goto:nil)
    ))

{a=terminals b=non-terminals c=actions d=goto}
