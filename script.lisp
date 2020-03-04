(load (compile-file "lexer.lisp"))
(load (compile-file "parser.lisp"))
(load (compile-file "eval.lisp"))



(defvar input (create-lex-data "(+ (- 1 10) 1)"))
(defvar tree (parse_eat_lparen input))

