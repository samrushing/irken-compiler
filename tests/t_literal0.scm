(datatype tree
  (:leaf 'a)
  (:node (tree 'a) (tree 'a))
  )

(literal (tree:node (tree:leaf 1) (tree:node (tree:leaf 2) (tree:leaf 3))))

