
(defun token-is (token type)
  (let ((tokentype (getf token :type)))
    (string= tokentype type)))

(defun parse_eat_lparen (input-data) 
  (progn 
    (let ((token (genToken input-data))) 
      (if (not (token-is token "LPAREN"))
          (format t "expect lparen")
          (parse input-data)
      ))
    )
 )

(defun parse (input-data) 
  (let ((ast-list nil) (token nil))  
      (progn 
        (loop do 
            (progn (setq token (genToken input-data))
               (if token 
                   (if (token-is token "LPAREN") 
                     (let ((new_element  (parse input-data)))
                       (nconc ast-list  (list (list :type "NESTED" :value new_element)))
                     )
                     (if (token-is token "RPAREN") 
                       (return)
                       (if ast-list
                         (nconc ast-list (list token))
                         (setq ast-list (list token))
                         )
                      )
                     )
                )) 
            while token)
         ast-list
        ))
)

