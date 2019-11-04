
(defun get-all-token (input-data)
  (let ((token (genToken input-data)))
    (let ((tokens (list token)))
      (progn
	(loop do
	      (progn
		(setq token (genToken input-data))
		(if token
		    (nconc tokens
			   (list token))))
	      while
	      token)
	tokens))))


;;(defparameter *tokens* (list :data (get-all-token *input-data*):pos			     0))

(defun advance-get-token (tokens)
  (let ((pos (getf tokens :pos))
	(data (getf tokens :data)))
    (progn
      (setf (getf tokens :pos) (+ pos 1))
      (nth (+ pos 1)
	   data))))

(defun get-token (tokens)
  (let ((pos (getf tokens :pos))
	(data (getf tokens :data)))
    (nth pos data)))

(defun token-is (token type)
  (let ((tokentype (getf token :type)))
    (string= tokentype type)))

(defun parser (tokens)
  (let ((token (get-token tokens)))
    (if (token-is token "LPAREN")
	(let ((op-token (advance-get-token tokens)))
	  (let ((ast-list (list op-token)))
	    (progn
	      (loop do
		    (progn
		      (setq token (advance-get-token tokens))
		      (if (token-is token "LPAREN")
			  (progn
			    (setq token (parser tokens))
			    (nconc ast-list
				   (list token))
			    (setq token (list :type "dummy") ))
			(if (not (token-is token "RPAREN"))
			    (nconc ast-list
				   (list token)))))
		    while
		    (not (token-is token "RPAREN")))
	      ast-list))))))
