
(datatype goto
  (:nil)
  (:cons int int (goto)))

(let ((x (goto:cons 1 1 (goto:cons 2 2 (goto:cons 3 3 (goto:nil))))))
  x)
