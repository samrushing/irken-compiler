(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

;; this thing somehow manages to evade all check_heap()
;; calls and will overflow the heap.  However, even with
;; that problem addressed it still doesn't work right.
;; most likely an issue with the 'stateful' implementation
;; of let_splat, and most likely something I won't be fixing.

;; (let ((x '())
;;       (y 0))
;;   (^call/cc 
;;    (lambda (escape)
;;      (let* ((yin ((lambda (foo)
;; 		    (%printn y)
;; 		    (set! x (cons y x))
;; 		    (if (= y 10)
;; 			(escape x)
;; 			(begin
;; 			  (set! y 0)
;; 			  foo)))
;; 		  (^call/cc (lambda (bar) bar))))
;; 	    (yang ((lambda (foo) 
;; 		     (set! y (+ y 1))
;; 		     foo)
;; 		   (^call/cc (lambda (baz) baz)))))
;;        (yin yang)))))

;; (define (yin/yang-letcc)
;;   (let ((x '())
;; 	(y 0))
;;     (call/cc
;;      (lambda (escape)
;;        (let* ((yin
;; 	       (let/cc (foo)
;; 		  (set! x (cons y x))
;; 		  (if (= y 10)
;; 		      (escape x)
;; 		      (begin
;; 			(set! y 0)
;; 			foo))))
;; 	      (yang
;; 	       (let/cc (foo)
;; 		  (set! y (+ y 1))
;; 		  foo)))
;; 	 (yin yang))))))

(define (yin/yang-relet)
  (define (id z) z)
  (let ((x '())
        (y 0))
    (^call/cc
     (lambda (escape)
       (let* ((yin
               (let ((foo (^call/cc id)))
		 (%printn x)
                 (set! x (cons y x))
                 (if (= y 10)
                     (escape x)
                     (begin
                       (set! y 0)
                       foo))))
              (yang
               (let ((foo (^call/cc id)))
		 (%printn y)
                 (set! y (+ y 1))
                 foo)))
         (yin yang))))))

(define (yin/yang)
  (define (id z) z)
  (let ((x '())
	(y 0))
    (^call/cc
     (lambda (escape)
       (let ((yin
	      ((lambda (foo)
		 (%printn x)
		 (set! x (cons y x))
		 (if (= y 10)
		     (escape x)
		     (begin
		       (set! y 0)
		       foo)))
	       (^call/cc id))))
	 (let ((yang
		((lambda (foo)
		   (%printn y)
		   (set! y (+ y 1))
		   foo)
		 (^call/cc id))))
	   (yin yang)))))))

;;(yin/yang-relet)

(define (yin/yang2)
  (define (id z) z)
  (let ((x '())
	(y 0))
    (^call/cc
     (lambda (escape)
       (let ((yin #f)
	     (yang #f))
	 (set! yin 
	       ((lambda (foo)
		 (%printn x)
		 (set! x (cons y x))
		 (if (= y 10)
		     (escape x)
		     (begin
		       (set! y 0)
		       foo)))
		(^call/cc id)))
	 (set! yang
	       ((lambda (foo)
		  (%printn y)
		  (set! y (+ y 1))
		  foo)
		(^call/cc id)))
	 (yin yang))))))

;(yin/yang2)

(define (yin/yang3)
  (define (id z) z)
  (let ((x '())
	(y 0))
    (^call/cc
     (lambda (escape)
       ;; un-let: (let ((x <xinit>)) <body>) => ((lambda (x) <body>) <xinit>)
       ((lambda (yin)
	  ((lambda (yang) (yin yang))
	   ((lambda (foo)
	      (%printn y)
	      (set! y (+ y 1))
	      foo)
	    (^call/cc id))))
	((lambda (foo)
	   (%printn x)
	   (set! x (cons y x))
	   (if (= y 10)
	       (escape x)
	       (begin
		 (set! y 0)
		 foo)))
	 (^call/cc id)))))))

;(yin/yang3)

(define (thing)
  (let* ((yin ((lambda (foo) (print-string ".") foo)
	       (^call/cc (lambda (bar) bar))))
	 (yang ((lambda (foo) (print-string "*") foo)
		(^call/cc (lambda (bar) bar)))))
    (yin yang)))
;(thing)

(define (thing2)
  (define (id x) x)
  (let ((yin (^call/cc id)))
    (print-string "\n")
    (let ((yang (^call/cc id)))
      (print-string "*")
      (yin yang)))
  )

(thing2)

;; ok, here's what pxll did to that:
;; (letrec ((^call/cc_16
;; 	  (lambda (p_72)
;; 	    (let* ((k_73 (%getcc)))
;; 	      (p_72 (lambda (r_74) (%putcc k_73 r_74)))))))
;;   (let* ((x_125_i3 nil)
;; 	 (y_126_i3 0))
;;     (^call/cc_16
;;      (lambda (escape_127_i3)
;;        (let* ((foo_129_i3
;; 	       (^call/cc_16
;; 		(lambda (bar_130_i3) bar_130_i3)))
;; 	      (yin_128_i3
;; 	       (begin
;; 		 (set! x_125_i3 (cons y_126_i3 x_125_i3))
;; 		 (if (%eq? y_126_i3 10)
;; 		     (escape_127_i3 x_125_i3)
;; 		     (begin (set! y_126_i3 0) foo_129_i3))))
;; 	      (foo_132_i3
;; 	       (^call/cc_16
;; 		(lambda (baz_133_i3) baz_133_i3)))
;; 	      (yang_131_i3
;; 	       (begin (set! y_126_i3
;; 			    (%+ y_126_i3 1))
;; 		      foo_132_i3)))
;; 	 (yin_128_i3 yang_131_i3))))))

;; (letrec ((=_8
;; 	  (lambda (x_58 y_59)
;; 	    (begin (verify TC_INT 1 x_58) (verify TC_INT 1 y_59) (%eq? x_58 y_59))))
;; 	 (+_9
;; 	  (lambda (x_60 y_61)
;; 	    (begin (verify TC_INT 1 x_60) (verify TC_INT 1 y_61) (%+ x_60 y_61))))
;; 	 (^call/cc_16
;; 	  (lambda (p_72)
;; 	    (let* ((k_73 (%getcc)))
;; 	      (p_72 (lambda (r_74) (%putcc k_73 r_74))))))
;; 	 (cons_20
;; 	  (lambda (a_86 b_87)
;; 	    (%%cexp "( t = %s, ((pxll_pair*)t)->car = %s, ((pxll_pair*)t)->cdr = %s, t)" (%make-tuple 24 2) a_86 b_87)))
;; 	 (yin/yang_46
;; 	  (lambda ()
;; 	    (let* ((x_125 nil)
;; 		   (y_126 0))
;; 	      (^call/cc_16
;; 	       (lambda (escape_127)
;; 		 (let* ((yin_128
;; 			 (let* ((foo_129 (^call/cc_16 (lambda (bar_130) bar_130))))
;; 			   (begin (set! x_125 (cons_20 y_126 x_125))
;; 				  (if (=_8 y_126 10)
;; 				      (escape_127 x_125)
;; 				      (begin (set! y_126 0) foo_129)))))
;; 			(yang_131
;; 			 (let* ((foo_132 (^call/cc_16 (lambda (baz_133) baz_133))))
;; 			   (begin (set! y_126 (+_9 y_126 1)) foo_132))))
;; 		   (yin_128 yang_131))))))))
;;   (yin/yang_46))
  

;; (letrec ((=_8
;; 	  (lambda (x_77 y_78) (begin (verify TC_INT 1 x_77) (verify TC_INT 1 y_78) (%eq? x_77 y_78))))
;; 	 (+_9
;; 	  (lambda (x_79 y_80) (begin (verify TC_INT 1 x_79) (verify TC_INT 1 y_80) (%+ x_79 y_80))))
;; 	 (^call/cc_16
;; 	  (lambda (p_91) (let* ((k_92 (%getcc))) (p_91 (lambda (r_93) (%putcc k_92 r_93))))))
;; 	 (cons_20
;; 	  (lambda (a_105 b_106) (%%cexp "( t = %s, ((pxll_pair*)t)->car = %s, ((pxll_pair*)t)->cdr = %s, t)" (%make-tuple 24 2) a_105 b_106)))
;; 	 (yin/yang_64
;; 	  (lambda ()
;; 	    (letrec ((id_183 (lambda (z_184) z_184)))
;; 	      (let* ((x_185 nil)
;; 		     (y_186 0))
;; 		(^call/cc_16
;; 		 (lambda (escape_187)
;; 		   (let* ((yin_188 ((lambda (foo_189)
;; 				      (begin
;; 					(set! x_185 (cons_20 y_186 x_185))
;; 					(if (=_8 y_186 10)
;; 					    (escape_187 x_185)
;; 					    (begin (set! y_186 0) foo_189))
;; 					))
;; 				    (^call/cc_16 id_183))))
;; 		     (let* ((yang_190 ((lambda (foo_191)
;; 					 (begin
;; 					   (set! y_186 (+_9 y_186 1))
;; 					   foo_191))
;; 				       (^call/cc_16 id_183))))
;; 		       (yin_188 yang_190))))))))))
;;   (yin/yang_64))


;; (letrec ((=_8 (lambda (x_79 y_80) (begin (verify TC_INT 1 x_79) (verify TC_INT 1 y_80) (%eq? x_79 y_80))))
;; 	 (+_9 (lambda (x_81 y_82) (begin (verify TC_INT 1 x_81) (verify TC_INT 1 y_82) (%+ x_81 y_82))))
;; 	 (^call/cc_16 (lambda (p_93) (let* ((k_94 (%getcc))) (p_93 (lambda (r_95) (%putcc k_94 r_95))))))
;; 	 (cons_20 (lambda (a_107 b_108) (%%cexp "( t = %s, ((pxll_pair*)t)->car = %s, ((pxll_pair*)t)->cdr = %s, t)" (%make-tuple 24 2) a_107 b_108)))
;; 	 (yin/yang3_66
;; 	  (lambda ()
;; 	    (letrec ((id_203 (lambda (z_204) z_204)))
;; 	      (let* ((x_205 nil) (y_206 0))
;; 		(^call/cc_16
;; 		 (lambda (escape_207)
;; 		   ((lambda (yin_208)
;; 		      ((lambda (yang_209) (yin_208 yang_209))
;; 		       ((lambda (foo_210)
;; 			  (begin
;; 			    (%printn y_206)
;; 			    (set! y_206 (+_9 y_206 1))
;; 			    foo_210))
;; 			(^call/cc_16 id_203))))
;; 		    ((lambda (foo_211)
;; 		       (begin (%printn x_205)
;; 			      (set! x_205 (cons_20 y_206 x_205))
;; 			      (if (=_8 y_206 10)
;; 				  (escape_207 x_205)
;; 				  (begin
;; 				    (set! y_206 0)
;; 				    foo_211))))
;; 		     (^call/cc_16 id_203))))))))))
;;   (yin/yang3_66)
;;   )
