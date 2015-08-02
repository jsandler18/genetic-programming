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

;;; defines some lists for the gpk using these functions
(setq functions '(rlog srt abs + - * %))
(setq argmap '(1 1 1 2 2 2 2))
(setq terminals '(0 1 2 3 4 5 6 7 8 9))