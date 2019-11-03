



(defun create-lex-data (input)
  (list :pos -1
	:data input
	:cur nil))

(defparameter *input-data* (create-lex-data "(getf   41 13)"))


(defun lex-data-length (lex-data)
  (let ((data (getf lex-data :data)))
    (length data)))

(defun genToken (lex-data)
  (let ((curChar (get-cur-char lex-data)))
    (if curChar
	(cond
	 ((char= curChar
		 #\()
	  "LPAREN")
	 ((char= curChar #\)) "RPAREN")
	 ((char= curChar #\+) "PLUS")
	 ((char= curChar #\-) "MINUS")
	 ((alpha-char-p curChar)
	  (get-ident lex-data))
	 ((digit-char-p curChar)
	  (get-number lex-data)))
      nil)))

(defun get-number (lex-data)
  (let ((start-pos (getf lex-data :pos)))
    (progn
      (loop do
	    (get-cur-char lex-data)
	    while
	    (digit-char-p (getf lex-data :cur)))
      (let ((end-pos (getf lex-data :pos))
	    (data (getf lex-data :data)))
	(subseq data start-pos end-pos)))))


(defun get-ident (lex-data)
  (let ((start-pos (getf lex-data :pos)))
    (progn
      (loop do
	    (get-cur-char lex-data)
	    while
	    (alpha-char-p (getf lex-data :cur)))
      (let ((end-pos (getf lex-data :pos))
	    (data (getf lex-data :data)))
	(subseq data start-pos end-pos))))) 

(defun get-cur-char (lex-data)
  (progn
    (advanceChar)
    (let ((pos (getf lex-data :pos)))
      (if (> (lex-data-length lex-data) pos)
	  (let ((data (getf lex-data :data)))
	    (let ((curChar (char data pos)))
	      (setf (getf *input-data* :cur) curChar)))
	nil))))


(defun advanceChar ()
  (let ((curpos (getf *input-data* :pos)))
    (setf (getf *input-data* :pos) (+ curpos 1))))




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


