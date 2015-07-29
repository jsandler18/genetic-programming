;;; stuff pertaining to generating program trees for the initial population
(defun full (funcs terms level size )
	"creates a program tree using the full method and recursive preorder traversal"
	(if (/= level size)
		(CONS (NTH (RANDOM (LENGTH FUNCs)) funcs) `(,(full funcs terms (+ level 1) size),(full funcs terms (+ level 1) size)))
		(nth (random (length terms)) terms)
	)
)
(defun grow (funcs terms level size)
	"creates a program tree using the grow method and recursive preorder traversal"
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
(defun rhah (funcs terms popsize maxsize)
	"creates an initial generation of programs using the 'ramped half-and-half' method (thus rhah)"
	(setq result ()) ;an initial empty list
	(setq numpersize (/ popsize (- maxsize 1))) ;sets number of programs per size to be equal percentages
	(loop for size from 2 to maxsize ; for each program size
		do(
			loop for program from 1 to numpersize ;for each program of this size
				do (
					if (= (mod program 2) 0) ;alternate between full and grow methods
						(setq result (cons (full funcs terms 1 size) result))
						(setq result (cons (grow funcs terms 1 size) result))
				)
		)	
	)
	result
	
)

;;; stuff pertaining to fitness



(setq functions '(+ - * %))
(setq terminals '(1 2 3 4 5 6 7 8 9))

(setq a (rhah functions terminals 10 6))
(dolist (n a) (print (eval n)))
