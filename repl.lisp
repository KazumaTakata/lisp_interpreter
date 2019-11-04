

(load (compile-file "lexer.lisp"))
(load (compile-file "parser.lisp"))
(load (compile-file "eval.lisp"))


(defparameter *input-data* (create-lex-data "(- 3 (- 10 3) )"))
(defparameter *tokens* (list :data (get-all-token *input-data*):pos
			     0))
(defparameter *ast-data* (parser *tokens*))
(defparameter result (eval-ast *ast-data*))

