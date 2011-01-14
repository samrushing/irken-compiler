;; -*- Mode: Irken -*-

(defmacro format
  (format)			 -> (list:nil)
  (format (<int> n) item ...)	 -> (list:cons (int->string n) (format item ...))
  (format (<char> ch) item ...)	 -> (list:cons (char->string ch) (format item ...))
  (format (<bool> b) item ...)	 -> (list:cons (bool->string b) (format item ...))
  (format (<string> s) item ...) -> (list:cons s (format item ...))
  (format x item ...)		 -> (list:cons x (format item ...)))
  )

(define emit
  (insn:return target) -> (print-string "PXLL_RETURN(" 
