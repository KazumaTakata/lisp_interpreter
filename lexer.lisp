



(defun create-lex-data (input)
  (list :pos 0
	:data input))

(defparameter *input-data* (create-lex-data "(+ 41 13)"))

(defun getCurChar (lex-data)
  (let ((pos (getf lex-data :pos)))
    (let ((data (getf lex-data :data)))
      (char data pos))))

(defun advanceChar ()
  (let ((curpos (getf *input-data* :pos)))
    ((setf (getf *input-data* :pos) curpos))))

(setf (getf *input-data* :pos) 1)


(defun isNumber (ch)
  (digit-char-p ch))

(defun isAlpha (ch)
  (alpha-char-p ch))


(defmacro Infix (infixAri)
  (let ((var1 (first infixAri))
	(op (second infixAri))
	(var2 (third infixAri)))
    `(,op ,var1 ,var2)))

(defmacro inc (var)
  (list 'setq
	var
	(list '1+ var)))



(inc x)
