;;; stuff pertaining to generating program trees for the initial population
(defun full (funcs funcargmap terms level size)
	"creates a program tree using the full method and recursive preorder traversal"
	(setq func (random (length funcs)))
	(if (/= level size)
		(CONS (NTH func funcs) (loop for x from 1 to (nth func funcargmap) collect (full funcs funcargmap terms (+ level 1) size)))
		(nth (random (length terms)) terms)
	)
)
(defun grow (funcs funcargmap terms level size)
	"creates a program tree using the grow method and recursive preorder traversal"
	;pick a node randomly from set of functions and terminals
	(setq funcsandterms (append funcs terms))
	(setq node (random (length funcsandterms)))
	(if (/= level size)
		;if not at max size
		(if (= 1 level)
			;if at root, make a function
			(CONS (NTH (mod node (length funcs)) funcs) (loop for x from 1 to (nth (mod node (length funcs)) funcargmap) collect (grow funcs funcargmap terms (+ level 1) size)))
			;if not at root, choose node from terminals and functions
			(if (< node (length funcs))
				;if an operator, generate children 
				(CONS (NTH node funcs) (loop for x from 1 to (nth node funcargmap) collect (grow funcs funcargmap terms (+ level 1) size)))
				;if a terminal, put terminal
				(nth node funcsandterms)
			)
		)
		;if at amx size, make a termina
		(nth (random (length terms)) terms)
	)	
)
(defun rhah (funcs funcargmap terms popsize maxsize)
	"creates an initial generation of programs using the 'ramped half-and-half' method (thus rhah)"
	(setq result ()) ;an initial empty list
	(setq numpersize (/ popsize (- maxsize 1))) ;sets number of programs per size to be equal percentages
	(loop for size from 2 to maxsize ; for each program size
		do(
			loop for program from 1 to numpersize ;for each program of this size
				do (
					if (= (mod program 2) 0) ;alternate between full and grow methods
						(setq result (cons (full funcs funcargmap terms 1 size) result))
						(setq result (cons (grow funcs funcargmap terms 1 size) result))
				)
		)	
	)
	result
	
)

;;; stuff pertaining to fitness
(defstruct program-fitness
	"Defines a structure for holding a program and its associated fitnesses" 
	prog
	raw
	std
	adj
	nrm
)
(defun stdfitness (rawfitness bestValue)
	"Function that takes an individual's raw fitness and converts it to standardized fitness. The first argument is jsut that raw fitness.  The second argument is the best possible value. If the raw fitness is lowest is best, then give this parameter 0. If highest is best, then give it the best value.  If Highest is best and the best value is unknown, use an arbitrarily high value"
	(abs (- bestValue rawfitness))
)
(defun adjfitness (stdfitness) 
	"Function that takes an individual's standard fitness and turns it into adjusted fitness."
	(/ 1 (+ 1 stdfitness))
)
(defun nrmfitness (adjfitness sum-adjfitness)
	"Takes the adjusted fitness of an induvidual and the sum of all of the adjusted fitnesses as arguments. puts out the normalized, or proportional fitness of that value"
	(/ adjfitness sum-adjfitness)
)
(defun listnrmfitness (rawfitnesses bestValue)
	"takes a list of the raw fitnesses as an argument and spits out the a list that of the normalized fitnesses in the same order"
	(setq std (loop for x from 0 to (- (length rawfitnesses) 1) collect (stdfitness (nth x rawfitnesses) bestValue)))
	(setq adj (loop for x from 0 to (- (length std) 1) collect (adjfitness (nth x std))))
	(setq sum (apply #'+ adj))
	(loop for x from 0 to (- (length adj) 1) collect (nrmfitness (nth x adj) sum))
)
