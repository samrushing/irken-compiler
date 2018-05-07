;; -*- Mode: Irken -*-

;; DSL for generating HTML

(define (rope-bracket left middle right)
  (rope-make left (rope-make middle right)))

(define (open-tag name)
  (rope-bracket
   (rope:leaf "<")
   (rope:leaf (symbol->string name))
   (rope:leaf ">")))

(define (close-tag name)
  (rope-bracket
   (rope:leaf "</")
   (rope:leaf (symbol->string name))
   (rope:leaf ">")))

(define (html-wrap tag contents)
  (rope-bracket (open-tag tag) contents (close-tag tag)))

(defmacro hitem
  ;; '&=' means 'escape html language'
  (hitem (<&=> x))      -> x
  ;; string
  (hitem (<&s> s))      -> (rope:leaf s)
  ;; format
  (hitem (<&f> x ...))  -> (rope:leaf (format x ...))
  ;; cat
  (hitem (<&cat> x))     -> (rope-cat x)
  ;; XXX tag-specific hacks could go here (like 'dl').
  ;; generic 'wrap with tag'
  (hitem (tag sub ...)) -> (html-wrap (quote (%%symbol tag)) (html sub ...))
  ;; XXX need a way to add params!
  ;; lone items must be strings.
  (hitem item)          -> (rope:leaf item)
  )

(defmacro html
  (html item) -> (hitem item)
  (html item0 item1 ...) -> (rope-make (hitem item0) (html item1 ...))
  )
