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
