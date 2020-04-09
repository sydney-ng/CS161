(defun DFS (TREE)
	(cond ((null TREE) nil) ; null return null 
		  ((atom TREE) (cons TREE nil)) ; one thing, return it 
		  (t (append (DFS (cdr TREE)) (DFS (car TREE)))) ; add right hand thing, add left 
	)
)

(defun BFS (TREE)
	(cond ((null TREE) nil) ; null return null 
		  ((atom TREE) (cons TREE nil)) ; one thing, return it 
		  (t (append (DFS (second TREE)) (DFS (car (first TREE))) (DFS (car (third TREE))) (DFS (cdr (first TREE))) (DFS (cdr (third TREE))) ))
	)
)

(defun DFID ()

)