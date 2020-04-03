
(defun TREE-CONTAINS (n TREE)
	(if (atom car(TREE)) ; if the first element is an atom 
		(if (= car(TREE) n) ; check if that atom is equal to n
			(t))  ; if it is, return true 
			(NIL)	  ; else, return false 
	)
	(or (TREE-CONTAIINS (car(TREE)) (TREE-CONTAIINS (cdr(TREE))))) ; it's a list, recursively check each for list
)

(defun TREE-CONTAINS (n TREE)
	(cond ((atom car(TREE)) 
			(if (= car(TREE) n)
				(t))
				(NIL)) 
		   ((list car(TREE)) (or (TREE-CONTAIINS (car(TREE)) (TREE-CONTAINS (cdr(TREE))))))
		   (t (NIL))
		  )
) 

;(defun TREE-MIN (TREE)
;	(if (= atom (TREE)) 
;			(TREE)
;			(TREE-MIN (car(TREE))
;		)
;	)
;)

;(defun TREE-ORDER (TREE)
	;()
;)
