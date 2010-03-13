;; create a bunch of symbols.

(datatype list
  (:nil)
  (:cons 'a (list 'a)))

'(one two three four five six seven eight nine ten)
