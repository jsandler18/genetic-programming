;;; redefining basic functions so they are closed in lisp numbers
(defun % (num denom)
	"Protected division"
	(if (= denom 0) 1 (/ num denom))
)
(defun gt (arg1 arg2)
	"Greater than function closed under integers"
	(if (> arg1 arg2) 1 -1)
)
;;; stuff pertaining to generating program trees for the initial population
(defun full (funcs terms level size)
	"grows a full program tree"
	(if (/= level size)
		(CONS (NTH (RANDOM (LENGTH FUNCs)) funcs) `(,(full funcs terms (+ level 1) size) ,(full funcs terms (+ level 1) size)))
		(nth (random (length terms)) terms)
	)
)
(defun grow (funcs terms level size)
	;pick a node randomly from set of functions and terminals
	(setq funcsandterms (append funcs terms))
	(setq node (random (length funcsandterms)))
	(if (/= level size)
		;if not at max size
		(if (= 1 level)
			;if at root, make a function
			(CONS (NTH (RANDOM (LENGTH FUNCs)) funcs) `(,(grow funcs terms (+ level 1) size) ,(grow funcs terms (+ level 1) size)))
			;if not at root, choose node from terminals and functions
			(if (< node (length funcs))
				;if an operator, generate children 
				(CONS (NTH node funcs) `(,(grow funcs terms (+ level 1) size) ,(grow funcs terms (+ level 1) size)))
				;if a terminal, put terminal
				(nth node funcsandterms)
			)
		)
		;if at amx size, make a termina
		(nth (random (length terms)) terms)
	)	
)

(setq functions '(+ - * %))
(setq terminals '(1 2 3 4 5 6 7 8 9))

(print (full functions terminals 1 4))
