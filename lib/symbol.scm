
(define (string->uninterned-symbol str)
  (let ((sym (%make-tuple #x1c 1)))
    (%%cexp "%s[1] = %s" sym str)
    sym))

;; this version uses frb.scm
(define string->symbol
  (let ((table (make-tree string-<? #f)))
    (lambda (str)
      (let ((probe (tree-member table str)))
	(if probe
	    probe
	    (let ((sym (string->uninterned-symbol str)))
	      (set! table (tree-insert table str sym))
	      sym))))))

;; this version uses a simple assoc list, probably more appropriate
;; unless the symbol table gets really big
(define (assoc-string str al)
  (cond ((null? al) #f)
	((string-=? str (caar al)) (cdar al))
	(else (assoc-string str (cdr al)))))

;; (define string->symbol
;;   (let ((table '()))
;;     (lambda (str)
;;       (let ((probe (assoc-string str table)))
;; 	(if probe
;; 	    probe
;; 	    (let ((sym (string->uninterned-symbol str)))
;; 	      (set! table (cons (cons str sym) table))
;; 	      sym))))))

