;; -*- Mode: Emacs-Lisp -*-

(require 'scheme)

;; Note: the default scheme mode will work just fine, but there are
;;   some additional features here that are pretty handy, including
;;   the highlighting of '->', M-x align, and some indentation tweaks.

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
	      "typealias"
	      ) t)
       "\\>") 1)
     ;; named let
     '("(let\\s-+\\(\\sw+\\)"
       (1 font-lock-function-name-face))
     ;; functions
     ;; SMR: 2015 - had to tweak this regex, something breaking, probably syntax-table related?
     ;;   may very well affect the other uses of \\sw...
     '("(define\\s-+(?\\([^ \t\n)]+\\)"
       (1 font-lock-function-name-face))
     ;; datatypes
     '("(datatype\\s-+\\(\\sw+\\)" (1 font-lock-type-face))
     ;; constructors
      '("\\<\\sw*:\\sw+\\>" . font-lock-type-face)
      ;; matching
      '("->" . font-lock-function-name-face)
      ))
  "Gaudy expressions to highlight in Scheme modes.")

;;;###autoload
(define-derived-mode irken-mode scheme-mode "Irken"
  "Major mode for editing Irken code."
  :syntax-table nil
  (setq major-mode 'irken-mode
	mode-name "Irken")
  (setq font-lock-defaults
       '((irken-font-lock-keywords))
       ;; nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
       ;; beginning-of-defun
       ;; (font-lock-mark-block-function . mark-defun)
       ;; (font-lock-syntactic-face-function
       ;; 	. scheme-font-lock-syntactic-face-function)
         ;; (parse-sexp-lookup-properties . t)
         ;; (font-lock-extra-managed-props syntax-table)))
       )
  )

(defun scheme-indent-match (state indent-point normal-indent)
  ;; there's probably an easier way to do this, but I'd like to
  ;;  be able to make this smarter in the future.  Specifically
  ;;  it might be nice to leave a hanging -> at the end of a line,
  ;;  and have this function automatically indent the next line by 2.
  ;; See http://community.schemewiki.org/?emacs-indentation
  (forward-char 1)
  (goto-char (elt state 1))
  (+ 2 (current-column)))

(put 'vcase 'scheme-indent-function 'scheme-let-indent)
(put 'match 'scheme-indent-function 'scheme-indent-match)
(put 'datatype 'scheme-indent-function 1)
(put 'map-range 'scheme-indent-function 1)
(put 'for-range 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 'scheme-let-indent)

;; This allows "M-x align" to line up a series of pattern-matches, try it!
(add-to-list 'align-rules-list
   '(irken-patterns
      (tab-stop . nil)
      (regexp . "\\(\\s-*\\)\\->\\(\\s-*\\)")
      (modes . '(irken-mode)))
   )

(provide 'irken)
