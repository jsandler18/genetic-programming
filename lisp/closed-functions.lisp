;;; functions that are closed under the numbers
(defun % (num denom) 
	"Protected division that does not die when divide by zero"
	(if (= denom 0) 1 (/ num denom))
)

(defun srt (n)
	"Protected square root that protects from negative square roots"
	(if (< n 0) 1 (sqrt n))
)

(defun rlog (n)
	"Protected natural log that protects from 0 arg"
	(if (<= n 0) 1 (log n))
)

(defun gt (a1 a2) 
	"> operation that returns an int"
	(if (> a1 a2) 1 -1)
)

;;; defines some lists for the gpk for the cart centering problem
(setq functions '(gt abs + - * %))
(setq argmap '(2 1 2 2 2 2))
(setq terminals '(x v -1))


;;; defines various fitness functons for practice problems
(defun cart-center-fit (program)
	"fitness function for the cart centering problem. works with programs that have terminals X, V, and -1"
	(setq timestep .02) ;.02 seconds per time step
	(setq timeout 10) ;times out after 10 seconds (500 time steps)
	(setq current-time 0.0) 
	(setq bounds .75) ; the absolute value of the max/min value of the domains of x and v for this problem
	(setq cases 20) ;number of different cases to test a program against
	(setq sum 0) ;holds the sum of the results of all of the cases, this will be the raw fitness
	(setq capture-range (sqrt (+ (* .05 .05) (* .05 .05)))) ;capture range is when euclidian distance from 0 position, 0 velocity is less than that of 0.05 position, 0.05 velocity
	(setq distance 50) ;the euclidian distance the cart is from being centered. 50 is an arbitraray distance that is outside of the capture range to start
	(setq accel .5) ;the magnitude of the acceleration caused by the "bang-bang" force that must be applied. .5 comes from m = 2 and F = 1 so the programs can be simpler
	(dotimes (n cases) 
		(setq v (- (* (random bounds) 2) bounds)) ;generate a random velocity and random position from -bounds to bounds
		(setq x (- (* (random bounds) 2) bounds))

		(loop while (and (< current-time timeout) (> distance capture-range) (< distance 100)) ;loop while not timed out and not captured
			do (setq direction (eval program))
			do (setq x (+ x (* timestep v))) ;calculate x for the next timestep
			do (setq v (+ v (* timestep (* direction accel)))) ;calculate v for next timestep
			do (incf current-time timestep)
			do (setq distance (sqrt (+ (* x x) (* v v)))) ;calculate new distance
		)
		(if (>= distance 100) 
			(setq current-time 10)
			nil
		)
		(incf sum current-time) ;add time to center to sum
		(setq current-time 0) ;reset time for next senario
		(setq distance 50) ;reset distance for next senario
	)	
	sum
	
)
;;;functions for the artificial ant problem

(setq board (make-array '(32 32);makes a 2d array of the example board from the book for the ant problem. ^,v,<,> represent the ant, facing up, down, left or right, respectively
				:initial-contents
				'((">" 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0)
				  (0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0)
				  (0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0)
				  (0 0 0 1 1 1 1 0 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 1 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0)
				  (0 0 0 1 1 0 0 1 1 1 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 1 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				 )))
;;set up x and y coords for the ant
(setq x 0)
(setq y 0)
(setq eaten 0)


(defun move () 
	"terminal function moves the and forward 1 place on the board, leaving an empty cell behind. will not move if hits a wall.
	depends on  the global variables board, x and y being defined already"
	;;get direction
	(setq dir (aref board x y))
	;check if going out of bounds, else move in the specified direction
	(if (equal dir ">") 
		(if (= (nth 0 (array-dimensions board)) (+ 1 y)) 
			nil
			(progn 
				(setf (aref board x y) 0) 
				(incf y 1)
				(if (= 1 (aref board x y))
					(incf eaten 1)
					nil
				)
				(setf (aref board x y) ">")
			) ;leave prev cell empty, move
		)
		nil
	)
	(if (equal dir "<") 
		(if (= 0 y) 
			nil
			(progn 
				(setf (aref board x y) 0) 
				(decf y 1)
				(if (= 1 (aref board x y))
					(incf eaten 1)
					nil
				)
				(setf (aref board x y) "<")
			) ;leave prev cell empty, move
		)
		nil
	)
	(if (equal dir "v") 
		(if (= (nth 1 (array-dimensions board)) (+ 1 x)) 
			nil
			(progn 
				(setf (aref board x y) 0) 
				(incf x 1) 
				(if (= 1 (aref board x y))
					(incf eaten 1)
					nil
				)
				(setf (aref board x y) "v")
			) ;leave prev cell empty, move
		)
		nil
	)
	(if (equal dir "^") 
		(if (= 0 x) 
			nil
			(progn 
				(setf (aref board x y) 0) 
				(decf x 1) 
				(if (= 1 (aref board x y))
					(incf eaten 1)
					nil
				)
				(setf (aref board x y) "^")) ;leave prev cell empty, move
		)
		nil
	)
)

(defun artificial-ant-fit (program)
	"Fitness functions for the artificial ant problem. works with programs that have terminals (move), (right), (left) "
	
	

)
