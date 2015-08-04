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
(defun pick-individual (programs)
	"Takes an array of fully filled out program-fitness structures and selects a single individual 
	from the population based on the fitness probability (normalized fitness)"
	(setq rnd (random 1.0)) 
	(setq idx (nth 0 (array-dimensions programs))) ;sets the index to the most fit individual
	(loop while (>= rnd 0)
		do (decf idx 1)
		do (decf rnd (program-fitness-nrm (aref programs idx)))
	)
	(aref programs idx)
)
(defun get-good-cross-point (program)
	"function that takees a single program as an argument and finds a point for crossover to occur such that a leaf has 
	a 10% chance of being chosen as the cross point and a non-leaf has a 90% chance."

	(setq traversed 0)
	(setq queue (list program))

	;; these two lists are going to hold the breadth first locations of the tree nodes. they are sepatrated by 
	;; leaves and non-leaves so i can more easily have different probabilities for selecting a leaf or a node
	(setq leaves nil)
	(setq non-leaves nil)

	(loop while (/= 0 (length queue)) ; while queue is not empty
		do (setq current (car queue)) ; dequeue
		do (setq queue (cdr queue))

		do (if (atom current)
			(setq leaves (append leaves (list traversed)))
			(progn (dolist (x (cdr current))
					(setq queue (append queue (list x)))
				)
				(setq non-leaves (append non-leaves (list traversed)))
			)
		)
		do (incf traversed 1)
	)
	(if (= 0 (random 10)) ; (random 10) == 0 is a 10% chance
		(nth (random (length leaves)) leaves) ;pick random leaf (10%)
		(nth (random (length non-leaves)) non-leaves) ;pick random non-leaf (90%)
	)

)

(defun get-nth-subtree (program n)
	"Gets the nth subtree of the given program returns program subtree. This is done in a breadth first way, so the whole program 
	is n == 0, the program's first child is 1, second child is 2, etc."
	(if (= n 0) (return-from get-nth-subtree program) nil)
	(setq traversed 0)
	(setq queue (list program))
	(loop while (/= 0 (length queue)) ; while queue is not empty
		do (setq current (car queue)) ; dequeue
		do (setq queue (cdr queue))

		do (if (atom current)
			nil ;atoms are leaves. nothing to do
			(dolist (x (cdr current))
				(setq queue (append queue (list x)))
				(incf traversed 1)
				(if (= traversed n)
					(return-from get-nth-subtree x)
					nil
				)
			)
		)
	)
	nil ;shouldnt make it here
)

(defun set-nth-subtree (program n subtree)
	"sets the nth subtree of the given program with the given subtree"
	(if (= n 0) 
		(return-from set-nth-subtree (if (atom subtree) 
							(progn (setf (car program) subtree) (setf (cdr program) nil)) 
							(progn (setf (car program) (car subtree)) (setf (cdr program) (cdr subtree)))
						)
		) 
		nil
	)
	(setq traversed 0)
	(setq queue (list program))
	(loop while (/= 0 (length queue)) ; while queue is not empty
		do (setq current (car queue)) ; dequeue
		do (setq queue (cdr queue))
		do (setq idx 1)
		do (if (atom current)
			nil ;atoms are leaves. nothing to do
			(dolist (x (cdr current))
				(setq queue (append queue (list x)))
				(incf traversed 1)
				(if (= traversed n)
					(setf (nth idx current) subtree)
					nil
				)
				(incf idx 1)
			)
		) 
	)
	nil ;shouldnt make it here
)
(defun crossover (parent1 parent2)
	"performs the crossover operation on the given parents.  This function modifies the given programs"
	(print parent1)
	(print parent2)
	(setq cross-point-1 (random (get-num-nodes parent1)))
	(setq cross-point-2 (random (get-num-nodes parent2)))
	(print cross-point-1)
	(print cross-point-2)
	(setq swap-1 (get-nth-subtree parent1 cross-point-1))
	(setq swap-2 (get-nth-subtree parent2 cross-point-2))
	(print swap-1)
	(print swap-2)
	(set-nth-subtree parent1 cross-point-1 swap-2)
	(set-nth-subtree parent2 cross-point-2 swap-1)
	(print parent1)
	(print parent2)
)
