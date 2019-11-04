
(defun eval-terminal (token)
  (let ((type (getf token :type))
	(value (getf token :value)))
    (cond
     ((string= type "NUMBER")
      (parse-integer value)))))


(defun eval-op (ast-data)
  (let ((op-token (getf (first ast-data)
			:value))
	(operand-1 (second ast-data))
	(operand-2 (third ast-data)))
    (progn
      (if (some #'listp operand-1)
	  (setq operand-1 (eval-ast operand-1))
	(setq operand-1 (eval-terminal operand-1)))
      (if (some #'listp operand-2)
	  (setq operand-2 (eval-ast operand-2))
	(setq operand-2 (eval-terminal operand-2)))
      (cond
       ((string= op-token "PLUS")
	(+ operand-1 operand-2))
       ((string= op-token "MINUS")
	(- operand-1 operand-2))))))

(defun eval-ast (ast-data)
  (let ((type (getf (first ast-data)
		    :type)))
    (cond
     ((string= type "OP")
      (eval-op ast-data)))))
