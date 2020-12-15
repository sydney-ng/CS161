(defun DFS (TREE)
	(cond ((null TREE) nil) ; null return null 
		  ((atom TREE) (cons TREE nil)) ; one thing, return it 
		  (t (append (DFS (cdr TREE)) (DFS (car TREE)))) ; add right hand thing, add left 
	)
)

(defun BFS (TREE)
	(cond ((null TREE) nil) ; null return null 
		  ((atom (car TREE)) (cons (car TREE) (BFS (cdr TREE)))) ; one thing, return it 
		  (t (BFS (append (cdr TREE) (car TREE))))
	)
)

(defun DFID (TREE depth)
	(cond ((null TREE) nil)
		  ((equal -1 depth) nil) ; check if reached ideal depth
		  (t (append (DFID TREE (- depth 1)) (parse_tree TREE depth))) ; make sure we can still continue 
 	)
 )

(defun parse_tree (TREE depth)
	(cond ((equal -1 depth) nil)
		  ((null TREE) nil)
		  ((atom TREE) (cons TREE nil)) ; only one element left 
		  (t (keep_parsing TREE depth)); go one level deeper on curr level + parse rest of branches 
	)
)

(defun keep_parsing (TREE depth)
	(cond ((= 1 (length TREE)) (parse_tree (car TREE) (- depth 1))) ;go one iteration deeper on first element
		  (t (append (parse_tree (car TREE) (- depth 1)) (parse_tree (cdr TREE) depth))) ; 
	)
)