



(defun create-lex-data (input)
  (list :pos -1
	:data input
	:cur nil))

;;(defparameter *input-data* (create-lex-data "(+ 3 (- 3 3) )"))


(defun lex-data-length (lex-data)
  (let ((data (getf lex-data :data)))
    (length data)))

(defun if-white-space (ch)
  (if (and ch
	   (or (char= ch #\Space)
	       (char= ch #\Newline)
	       (char= ch #\Tab)))
      1
    nil))

(defun eat-space (lex-data)
  (if (if-white-space (get-next-char lex-data))
      (loop do
	    (advance-get-cur-char lex-data)
	    while
	    (if-white-space (get-next-char lex-data)))))

(defun genToken (lex-data)
  (progn
    (eat-space lex-data)
    (let ((curChar (advance-get-cur-char lex-data)))
      (if curChar
	  (cond
	   ((char= curChar
		   #\()

	    (create-token "LPAREN" "LPAREN"))
	   ((char= curChar #\))
	    (create-token "RPAREN" "RPAREN"))
	   ((char= curChar #\+)
	    (create-token "OP" "PLUS"))
	   ((char= curChar #\-)
	    (create-token "OP" "MINUS"))
	   ((alpha-char-p curChar)
	    (if-keyword (get-ident lex-data)))
	   ((digit-char-p curChar)
	    (create-token "NUMBER"
			  (get-number lex-data))))
	nil))))

(defun if-keyword (string-data)
  (if (or (string= string-data "defun")
	  (string= string-data "defparameter")
	  (string= string-data "defvar"))
      (create-token "KEYWORD" string-data)
    (create-token "IDENT" string-data)))

(defun create-token (type value)
  (list :type type
	:value value))

(defun get-number (lex-data)
  (let ((start-pos (getf lex-data :pos)))
    (progn
      (loop do
	    (if (digit-char-p (get-next-char lex-data))
		(advance-get-cur-char lex-data))
	    while
	    (digit-char-p (get-next-char lex-data)))
      (let ((end-pos (getf lex-data :pos))
	    (data (getf lex-data :data)))
	(subseq data
		start-pos
		(+ 1 end-pos))))))


(defun get-ident (lex-data)
  (let ((start-pos (getf lex-data :pos)))
    (progn
      (loop do
	    (if (alpha-char-p (get-next-char lex-data))
		(advance-get-cur-char lex-data))
	    while
	    (alpha-char-p (get-next-char lex-data)))
      (let ((end-pos (getf lex-data :pos))
	    (data (getf lex-data :data)))
	(subseq data
		start-pos
		(+ 1 end-pos)))))) 

(defun advance-get-cur-char (lex-data)
  (progn
    (advanceChar)
    (let ((pos (getf lex-data :pos)))
      (if (> (lex-data-length lex-data) pos)
	  (let ((data (getf lex-data :data)))
	    (let ((curChar (char data pos)))
	      (setf (getf lex-data :cur) curChar)))
	nil))))

(defun get-cur-char (lex-data)
  (let ((pos (getf lex-data :pos)))
    (if (> (lex-data-length lex-data) pos)
	(let ((data (getf lex-data :data)))
	  (char data pos))
      nil)))

(defun get-next-char (lex-data)
  (let ((nextPos (+ 1
		    (getf lex-data :pos))))
    (if (> (lex-data-length lex-data) nextPos)
	(let ((data (getf lex-data :data)))
	  (char data nextPos))
      nil)))

(defun advanceChar ()
  (let ((curpos (getf *input-data* :pos)))
    (setf (getf *input-data* :pos) (+ curpos 1))))





