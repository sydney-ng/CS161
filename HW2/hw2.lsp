(defun DFS (TREE)
	(cond ((null TREE) nil) ; null return null 
		  ((atom TREE) (cons TREE nil)) ; one thing, return it 
		  (t (append (DFS (cdr TREE)) (DFS (car TREE)))) ; add right hand thing, add left 
	)
)

(defun BFS (TREE)
	(cond ((null TREE) nil) ; null return null 
		  ((atom TREE) (cons TREE nil)) ; one thing, return it 
		  (t (append (DFS (second TREE)) (DFS (first (first TREE))) (DFS (first (third TREE))) (DFS (cdr (first TREE))) (DFS (cdr (third TREE))) ))
	)
)

(defun DFID (TREE depth)
	(cond ((null TREE) nil)
		  ((equal depth -1) nil) ; check if reached ideal depth
		  (t (append (DFID TREE (- depth 1)) (parse_tree TREE depth))) ; make sure we can still continue 
 	)
 )

(defun parse_tree (TREE depth)
	(cond ((equal depth -1) nil)
		  ((null TREE) nil)
		  ((atom TREE) (cons TREE nil)) ; only one element left 
		  (t (keep_parsing TREE depth)); go one level deeper on curr level + parse rest of branches 
	)
)

(defun keep_parsing (TREE depth)
	(cond ((= (length TREE) 1) (parse_tree (car TREE) (- depth 1))) ;go one iteration deeper on first element
		  (t (append (parse_tree (car TREE) (- depth 1)) (parse_tree (cdr TREE) depth))) ; 
	)
)