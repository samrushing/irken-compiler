
;; let's try to build these data structures for the parser:

;; stack = (:elem item state stack) | (:nil)
;; item  = (:nt kind args) | (:t val)
;; args  = (:cons item args) | (:nil)
;; state = int
;; kind  = symbol

(datatype list
  (:cons 'a (list 'a))
  (:nil)
  )

(datatype item
  (:nt string (list (item 'a)))
  (:t string)
  )

(datatype stack
  (:empty)
  (:elem (item 'a) int (stack 'a))
  )

(let ((stack (stack:empty))
      (item0 (item:t "terminal"))
      (item1 (item:nt "non-terminal" (list:cons 12 (list:nil))))
      )
  
  (define (push x y)
    (set! stack (:elem x y stack))
    stack
    )

  (push item0 34)
  (push item1 9)
  ;(push item0 #t)
  (set! stack (:elem item0 #t))
  )

