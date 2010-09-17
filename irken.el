;; -*- Mode: Emacs-Lisp -*-

(require 'scheme)

;; my first attempt at writing a derived mode.

(defconst irken-font-lock-keywords
  (eval-when-compile
    (list
     (cons
      (concat
       "(" (regexp-opt
	    '("begin" "call-with-current-continuation" "call/cc"
	      "case" "cond" "vcase" "match" "with" "datatype" "defmacro"
	      "else" "if" "lambda"
	      "let" "let*" "letrec"
	      "define" "set!"
	      "include"
	      ) t)
       "\\>") 1)
     ;; named let
     '("(let\\s-+\\(\\sw+\\)"
       (1 font-lock-function-name-face))
     ;; functions
     '("(define\\s-+(?\\(\\sw+\\)"
       (1 font-lock-function-name-face))
     ;; datatypes
     '("(datatype\\s-+\\(\\sw+\\)" (1 font-lock-type-face))
     ;; constructors
      '("\\<\\sw*:\\sw+\\>" . font-lock-type-face)
      ;; matching
      '("->" . font-lock-function-name-face)
      ))
  "Gaudy expressions to highlight in Scheme modes.")

(define-derived-mode irken-mode scheme-mode "Irken"
  "Major mode for editing Irken code."
  (setq major-mode 'irken-mode
	mode-name "Irken")
  (setq font-lock-defaults
       '((irken-font-lock-keywords)
         nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
         beginning-of-defun
         (font-lock-mark-block-function . mark-defun)
         (font-lock-syntactic-face-function
          . scheme-font-lock-syntactic-face-function)
         (parse-sexp-lookup-properties . t)
         (font-lock-extra-managed-props syntax-table)))
  )

(put 'vcase 'scheme-indent-function 'scheme-let-indent)
(put 'match 'scheme-indent-function 'scheme-let-indent)
(put 'datatype 'scheme-indent-function 1)

(provide 'irken)
