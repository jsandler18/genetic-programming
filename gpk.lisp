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
(defun rawfitness (programs fit-func)
	"Takes a list of programs as a first argument and returns an array of program-fitness structures with the prog and raw fields filled in. 
	Also takes the fitness function being used"
	(setf prog-fit (make-array (length programs)))
	(setq x 0)
	(dolist (n programs) 
		(setq rawfit (funcall fit-func n))
		(setq fit-struct (make-program-fitness :prog n :raw rawfit)) ; creates a new program-fitness function
		(setf (aref prog-fit x) fit-struct) 
		(incf x 1)
	)
	prog-fit

)
(defun stdfitness (rawfitness bestValue)
	"Function that takes an array ofprogram-fitness structures with raw fitness filled in and converts it to standardized fitness. The first argument is jsut that raw fitness.
	The second argument is the best possible value. If the raw fitness is lowest is best, 
	then give this parameter 0. If highest is best, then give it the best value. 
	If Highest is best and the best value is unknown, use an arbitrarily high value"
	(dotimes (x (nth 0 (array-dimensions rawfitness)))
		(setq rawfit (program-fitness-raw (aref rawfitness x))) ;get the raw fitness of the element
		(setq stdfit (abs (- bestValue rawfit)))
		(setf (program-fitness-std (aref rawfitness x)) stdfit)
	)
)
(defun adjfitness (stdfitness) 
	"Function that takes an array of program-fitness structures with std filled in and converts it to adjusted fitness."
	(dotimes (x (nth 0 (array-dimensions stdfitness)))
		(setq stdfit (program-fitness-std (aref stdfitness x))) ;get the std fitness of the element
		(setq adjfit (/ 1 (+ 1 stdfit)))
		(setf (program-fitness-adj (aref stdfitness x)) adjfit)
	)
	
)
(defun nrmfitness (adjfitness)
	"takes an array of program-fitness structures with adj filled in. puts out the normalized, or proportional fitness of that value"
	(setq sum-adjfitness 0)
	(dotimes (x (nth 0 (array-dimensions adjfitness)))
		(incf sum-adjfitness (program-fitness-adj (aref adjfitness x)))
	)
	(dotimes (x (nth 0 (array-dimensions adjfitness)))
		(setq adjfit (program-fitness-adj (aref adjfitness x)))
		(setq nrmfit (/ adjfit sum-adjfitness))
		(setf (program-fitness-nrm (aref adjfitness x)) nrmfit)
	)
)
(defun sort-fit-first (programs)
	"Takes an array of fully filled program-fitness structures and sorts them such that the most fit are first. uses insertion sort"
	(loop for i from 1 to (- (nth 0 (array-dimensions programs)) 1) 

		do (setq x (aref programs i))
		do (setq j i)
		do (loop while (and (> j 0) (> (program-fitness-nrm (aref programs (- j 1))) (program-fitness-nrm x))) 
			do (setf (aref programs j) (aref programs (- j 1)))
			do (decf j 1)
		)
		do (setf (aref programs j) x)

	)

)
(defun fit-and-sort (programs fit-func bestValue)
	"Ties together all of the functions nessesary for preparing programs for selection"
	(setq prog-ar (rawfitness programs fit-func))
	(stdfitness prog-ar bestValue)
	(adjfitness prog-ar)
	(nrmfitness prog-ar)
	(sort-fit-first prog-ar)
	prog-ar
)

;;; stuff pertaining to creating a new generation
(defun select (programs)
	"takes an array of fully filled out program-fitness structs that are sorted in ascending order by normalized fitness.
	Creates a population of equal size based on the fitnesses of the current population.  Does not perform cross-over or mutation"
)
