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
	      "let" "let*" "letrec" "let-values"
	      "define" "set!"
	      "include"
	      "typealias"
              "letcc" "getcc" "putcc" "let/cc"
              "make-enum" "try" "except"
	      ) t)
       "\\>") 1)
     ;; named let
     '("(let\\s-+\\(\\sw+\\)" (1 font-lock-function-name-face))
     '("(make-enum\\s-+\\([^ \t\n)]+\\)" (1 font-lock-type-face))
     ;; functions
     ;; SMR: 2015 - had to tweak this regex, something breaking, probably syntax-table related?
     ;;   may very well affect the other uses of \\sw...
     '("(define\\s-+(?\\([^ \t\n)]+\\)" . (1 font-lock-function-name-face))
     '("(\\([^) \t\n]*:[^) \t\n]+\\)" . (1 font-lock-type-face))
     ;; datatypes
     '("(datatype\\s-+(?\\([^ \t\n)]+\\)" . (1 font-lock-type-face))
     ;; constructors (again, the issue with \\sw)
     ;; '("\\<\\sw*:\\sw+\\>" . font-lock-type-face)
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
(put 'makegen 'scheme-indent-function 1)
(put 'for-range 'scheme-indent-function 2)
(put 'for-range* 'scheme-indent-function 3)
(put 'for-range-rev 'scheme-indent-function 2)
(put 'for-list 'scheme-indent-function 2)
(put 'for-list2 'scheme-indent-function 4)
(put 'for-set 'scheme-indent-function 2)
(put 'for-set2 'scheme-indent-function 4)
(put 'for-map 'scheme-indent-function 3)
(put 'for-alist 'scheme-indent-function 3)
(put 'for-vector 'scheme-indent-function 2)
(put 'for-vector-rev 'scheme-indent-function 2)
(put 'for-string 'scheme-indent-function 2)
(put 'for 'scheme-indent-function 2)
(put 'let/cc 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'when-maybe 'scheme-indent-function 2)
(put 'while-maybe 'scheme-indent-function 2)
(put 'while-queue 'scheme-indent-function 2)
(put '%backend 'scheme-indent-function 1)
(put 'make-enum 'scheme-indent-function 1)
(put 'with-file 'scheme-indent-function 2)
(put 'with-mutex 'scheme-indent-function 2)

;; I want it to indent to the same place (i.e., unnatural lisp).
;;  this doesn't do it.
;;(put '%backend 'scheme-indent-function 0)

(require 'align)
;; This allows "M-x align" to line up a series of pattern-matches, try it!
(add-to-list 'align-rules-list
   '(irken-patterns
      (tab-stop . nil)
      (regexp . "\\(\\s-*\\)\\->\\(\\s-*\\)")
      (modes . '(irken-mode)))
   )

(provide 'irken)
