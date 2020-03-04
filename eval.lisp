
(defun eval-tree (ast-data)
  (let ((type (getf (first ast-data) :type)))
    (cond
     ((string= type "OP")
      (let ((operator (getf (first ast-data) :value)))
           (let ((ast-data (rest ast-data)) (accum 0))
             (progn (loop do 
                   (progn (let ((operand-node (first ast-data)) (operand nil))
                           (progn (cond ((string= (getf operand-node :type) "NESTED") 
                                         (setq operand (eval-tree (getf operand-node :value))))
                                        ((string= (getf operand-node :type) "NUMBER")
                                         (setq operand (parse-integer (getf operand-node :value)))))
                                  (cond ((string= operator "PLUS")
                                         (setq accum (+ accum operand)))
                                        ((string= operator "MINUS")
                                         (setq accum (- accum operand)))
                                        )
                            )) 
                          (setf ast-data (rest ast-data)))
                    while ast-data)
                    accum)
             )

        )))))
