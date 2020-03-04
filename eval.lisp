
(defun eval-operator (ast-data)
  (let ((operator (getf (first ast-data) :value)))
    (let ((ast-data (rest ast-data)) (accum 0) (if_first 1))
      (progn (loop do 
                   (progn (let ((operand-node (first ast-data)) (operand nil))
                            (progn (cond ((string= (getf operand-node :type) "NESTED") 
                                          (setq operand (eval-tree (getf operand-node :value))))
                                         ((string= (getf operand-node :type) "NUMBER")
                                          (setq operand (parse-integer (getf operand-node :value)))))
                                   (cond ((string= operator "PLUS")
                                          (setq accum (+ accum operand)))
                                         ((string= operator "MINUS")
                                          (if if_first 
                                            (progn 
                                              (if (< 1 (list-length ast-data)) 
                                                (setq accum operand)
                                                (setq accum (- operand)))
                                              (setq if_first nil))
                                            (setq accum (- accum operand))))))) 
                          (setf ast-data (rest ast-data)))
                   while ast-data)
             accum))
    )


  )


(defun eval-tree (ast-data)
  (let ((type (getf (first ast-data) :type)))
    (cond
      ((string= type "OP")
       (eval-operator ast-data)
      ))))
